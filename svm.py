import xml.etree.ElementTree as ET
import os
import collections
import sys

from sklearn import metrics
from sklearn.pipeline import Pipeline, FeatureUnion
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.svm import LinearSVC
from sklearn.base import BaseEstimator, TransformerMixin


def loadStopWords():
    stops = []
    with open('stop-word-list.txt', 'r') as f:
        for line in f:
            stops.append(line.translate(None, '\r\n'))
    return stops

class ItemSelector(BaseEstimator, TransformerMixin):
    def __init__(self, key):
        self.key = key

    def fit(self, x, y=None):
        return self

    def transform(self, data_dict):
        return data_dict[self.key]


def words_before(sentence, e1_offsets, e2_offsets):
    return sentence[:e1_offsets[0][0]].strip(' ,.:!;?')

def words_after(sentence, e1_offsets, e2_offsets):
    return sentence[e2_offsets[-1][-1] + 1:].strip(' ,.:!;?')

def words_between(sentence, e1_offsets, e2_offsets):
    begin = e1_offsets[0][-1] + 1
    end = e2_offsets[0][0]
    return sentence[begin:end].strip(' ,.:!;?')

def get_offsets(entity):
    offsets_str = entity.get('charOffset').split(';')
    offsets = []
    for offset in offsets_str:
        offsets.extend(map(int, offset.split('-')))
    result = []
    for i in range(0, len(offsets), 2):
        result.append((offsets[i], offsets[i+1]))
    return result

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


def read_sentences(filepath):
    sentences = []
    targets = []

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

                        sentences.append((text, e1_offsets, e2_offsets))
                        targets.append(pair.get('ddi') == "true")

    return sentences, targets








sentences, targets = read_sentences(sys.argv[1])
train_sentences, test_sentences = sentences[:int(0.7*len(sentences))], sentences[int(0.7*len(sentences)):]
train_targets, test_targets = targets[:int(0.7*len(targets))], targets[int(0.7*len(targets)):]

pipeline = Pipeline([
    ('sentence_parts', SentencePartExtractor()),

    ('union', FeatureUnion(
        transformer_list=[
            ('subject', Pipeline([
                ('selector', ItemSelector(key='before')),
                ('count_before', CountVectorizer(stop_words = loadStopWords())),
            ])),
            ('subject', Pipeline([
                ('selector', ItemSelector(key='between')),
                ('count_between', CountVectorizer(stop_words = loadStopWords())),
            ])),
            ('subject', Pipeline([
                ('selector', ItemSelector(key='after')),
                ('count_after', CountVectorizer(stop_words = loadStopWords())),
            ])),

        ],
        transformer_weights={
            'count_before': 0.5,
            'count_between': 2.0,
            'count_after': 0.5,
        },
    )),
    ('clf', LinearSVC())
])


pipeline.fit(train_sentences, train_targets)

print 'Predicting...'
predicted = pipeline.predict(test_sentences)


print(metrics.classification_report(test_targets, predicted))


#
#
# filepath = sys.argv[1]
#
# print 'Reading sentences...'
# # all_sentences = sentences(filepath)
# all_sentences = between_pairs(filepath)
#
#
# train_tuples, test_tuples = all_sentences[:int(0.7*len(all_sentences))], all_sentences[int(0.7*len(all_sentences)):]
# sentence_data = [i[0] for i in train_tuples]
# sentence_target = [i[1] for i in train_tuples]
# test_target = [i[1] for i in test_tuples]
# test_data = [i[0] for i in test_tuples]
#
# print 'Training...'
# clf = Pipeline([('vect', CountVectorizer(stop_words = loadStopWords())),
#                ('clf', LinearSVC())
#                ])
#
# clf.fit(sentence_data, sentence_target)
#
# print 'Predicting...'
# predicted = clf.predict(test_data)
#
#
# print(metrics.classification_report(test_target, predicted))
