import xml.etree.ElementTree as ET
import os
import collections
import sys
from random import sample
import random

import numpy as np
import matplotlib.pyplot as plt
from sklearn import metrics
from sklearn.pipeline import Pipeline, FeatureUnion
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from sklearn.feature_extraction import DictVectorizer
from sklearn.svm import LinearSVC
from sklearn.base import BaseEstimator, TransformerMixin

import pdb

TEST_RATIO = .7
REPLACE_ENTITIES = True

######### READ AND SPLIT SENTENCES #########
def read_sentences(filepath, interaction_type):
    true_samples = []
    false_samples = []

    for filename in os.listdir(filepath):
       if (".xml" in filename):
            tree = ET.parse(filepath+filename)
            root = tree.getroot()

            for document in root.iter('document'):
                for sentence in document.findall('sentence'):
                    entities = sentence.findall('entity')
                    text = sentence.get('text')
                    for pair in sentence.findall('pair'):
                        pair_e1 = pair.get('e1')
                        pair_e2 = pair.get('e2')
                        e1 = filter(lambda x: x.get('id') == pair_e1, entities)[0]
                        e2 = filter(lambda x: x.get('id') == pair_e2, entities)[0]

                        e1_offsets = get_offsets(e1)
                        e2_offsets = get_offsets(e2)

                        if REPLACE_ENTITIES:
                            text = replaceEntity(text, e1_offsets)
                            text = replaceEntity(text, e2_offsets)

                        if pair.get('ddi') == "true" and pair.get('type') == interaction_type:
                            true_samples.append((text, e1_offsets, e2_offsets))
                        else:
                            false_samples.append((text, e1_offsets, e2_offsets))

    return true_samples, false_samples

def replaceEntity(text, offsets):
    for offset in offsets:
        text = text[:offset[0]] +\
            (offset[1] - offset[0] + 1)*'#' +\
            text[offset[1] + 1:]
    return text


def words_before(sentence, e1_offsets, e2_offsets):
    text = sentence[:e1_offsets[0][0]]
    return text.replace('#', '').replace('  ', ' ').strip(' ,.:!;?')

def words_after(sentence, e1_offsets, e2_offsets):
    text = sentence[e2_offsets[-1][-1] + 1:]
    return text.replace('#', '').replace('  ', ' ').strip(' ,.:!;?')

def words_between(sentence, e1_offsets, e2_offsets):
    begin = e1_offsets[0][-1] + 1
    end = e2_offsets[0][0]
    text = sentence[begin:end]
    return text.replace('#', '').replace('  ', ' ').strip(' ,.:!;?')

def get_offsets(entity):
    offsets_str = entity.get('charOffset').split(';')
    offsets = []
    for offset in offsets_str:
        offsets.extend(map(int, offset.split('-')))
    result = []
    for i in range(0, len(offsets), 2):
        result.append((offsets[i], offsets[i+1]))
    return result

def loadStopWords():
    stops = []
    with open('stop-word-list.txt', 'r') as f:
        for line in f:
            stops.append(line.translate(None, '\r\n'))
    return stops

def splitSamples(smaples):
    return smaples[:int(TEST_RATIO*len(smaples))], smaples[int(TEST_RATIO*len(smaples)):]

######### SKLEARN PIPELINE #########
class ItemSelector(BaseEstimator, TransformerMixin):
    def __init__(self, key):
        self.key = key

    def fit(self, x, y=None):
        return self

    def transform(self, data_dict):
        return data_dict[self.key]

class SentencePartExtractor(BaseEstimator, TransformerMixin):
    def fit(self, x, y=None):
        return self

    def transform(self, triples):
        features = {}
        features['before'] = []
        features['between'] = []
        features['after'] = []
        for sentence, e1_offsets, e2_offsets in triples:
            features['before'].append(words_before(sentence, e1_offsets, e2_offsets))
            features['between'].append(words_between(sentence, e1_offsets, e2_offsets))
            features['after'].append(words_after(sentence, e1_offsets, e2_offsets))
        return features

class IDFExtractor(BaseEstimator, TransformerMixin):
    def __init__(self, collection):
        self.collection = collection

    def fit(self, x, y=None):
        return self

    def transform(self, sentences):
        features = []
        for sentence in sentences:
            values = {}
            for term in set(sentence.split(' ')):
                # if term == '': continue
                values[term] = idf(term, self.collection)
            features.append(values)
        return features

def idf(term, collection):
    return len([s for s in collection if term in s[0]])

######### APPLY #########
def test(interaction_type):
    print interaction_type
    print 'Reading files...'
    true_samples, false_samples = read_sentences(sys.argv[1], interaction_type)
    # randomly select as many as true samples
    false_samples = sample(false_samples, len(true_samples))

    sentences = true_samples + false_samples
    targets = len(true_samples) * [True] + len(false_samples) * [False]

    z = zip(sentences, targets)

    random.shuffle(z)
    sentences, targets = zip(*z)

    train_sentences, test_sentences = splitSamples(sentences)
    train_targets, test_targets = splitSamples(targets)

    #stop_words = []
    # stop_words = 'english'
    stop_words = loadStopWords()

    pipeline = Pipeline([
        ('sentence_parts', SentencePartExtractor()),

        ('union', FeatureUnion(
            transformer_list=[
                ('before', Pipeline([
                    ('selector', ItemSelector(key='before')),
                    ('features_before', FeatureUnion([
                        ('count_before', CountVectorizer(stop_words = stop_words)),
                        ('idf', Pipeline([
                            ('dict_before', IDFExtractor(train_sentences)),
                            ('idf_before', DictVectorizer())
                        ]))
                        # ('tf_idf_before', TfidfVectorizer(stop_words = stop_words))
                    ]))
                ])),
                ('between', Pipeline([
                    ('selector', ItemSelector(key='between')),
                    ('features_between', FeatureUnion([
                        ('count_between', CountVectorizer(stop_words = stop_words)),
                        ('idf', Pipeline([
                            ('dict_between', IDFExtractor(train_sentences)),
                            ('idf_between', DictVectorizer())
                        ]))                      # ('tf_idf_between', TfidfVectorizer(stop_words = stop_words))
                    ]))
                ])),
                ('after', Pipeline([
                    ('selector', ItemSelector(key='after')),
                    ('features_after', FeatureUnion([
                        ('count_after', CountVectorizer(stop_words = stop_words)),
                        ('idf', Pipeline([
                            ('dict_after', IDFExtractor(train_sentences)),
                            ('idf_after', DictVectorizer())
                        ]))                        # ('tf_idf_after', TfidfVectorizer(stop_words = stop_words))
                    ]))
                ]))
            ]
        )),
        ('clf', LinearSVC())
    ])

    print 'Training...'
    pipeline.fit(train_sentences, train_targets)

    print 'Predicting...'
    predicted = pipeline.predict(test_sentences)


    print(metrics.classification_report(test_targets, predicted))
    # print(metrics.roc_curve(test_targets, predicted))
    return test_targets, predicted


types = ['advise', 'effect', 'mechanism', 'int']
for i in types:
    targets, predicted = test(i)
    # roc = metrics.roc_curve(targets, predicted)
    # print(roc)
    # roc_auc = metrics.auc(roc[0], roc[1])
    # plt.plot(roc[0], roc[1],
    #          label= ' ' + i + ' area = %0.2f' % (roc_auc)
    #         )

# plt.plot([0, 1], [0, 1], 'k--')
# plt.xlim([0.0, 1.0])
# plt.ylim([0.0, 1.05])
# plt.xlabel('False Positive Rate')
# plt.ylabel('True Positive Rate')
# plt.title('Some extension of Receiver operating characteristic to multi-class')
# plt.legend(loc="lower right")
# plt.show()
