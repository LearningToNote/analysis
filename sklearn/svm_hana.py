import xml.etree.ElementTree as ET
import os
import collections
import sys
from random import sample
import random
import json
import numpy as np
# import matplotlib.pyplot as plt
from sklearn import metrics
from sklearn.pipeline import Pipeline, FeatureUnion
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from sklearn.feature_extraction import DictVectorizer
from sklearn.svm import LinearSVC
from sklearn.base import BaseEstimator, TransformerMixin
import time
import pdb

import pyhdb
import pyhdb.exceptions
from pprint import pprint as pp

#Load credentials
with open("../secrets.json") as f:
    secrets = json.load(f)

connection = pyhdb.connect(
    host=secrets['host'],
    port=secrets['port'],
    user=secrets['username'],
    password=secrets['password']
)

cursor = connection.cursor()

TEST_RATIO = .7
REPLACE_ENTITIES = True

######### READ AND SPLIT SENTENCES #########
def entity_tokens():
    cursor.execute("""
        SELECT E1_ID, E2_ID, DDI,
        CASE WHEN FTI.TA_STEM IS NULL THEN FTI.TA_NORMALIZED ELSE FTI.TA_STEM END AS TOKEN,
        CASE
            WHEN FTI.TA_COUNTER < FTI1.TA_COUNTER THEN -1
            WHEN (FTI.TA_COUNTER > FTI1.TA_COUNTER AND FTI.TA_COUNTER < FTI2.TA_COUNTER) THEN 0
            WHEN FTI.TA_COUNTER > FTI2.TA_COUNTER THEN 1
        END AS POSITION
        FROM LEARNING_TO_NOTE.PAIRS P
        JOIN LEARNING_TO_NOTE.ENTITIES E1 ON P.E1_ID = E1.ID
        JOIN LEARNING_TO_NOTE.ENTITIES E2 ON P.E2_ID = E2.ID
        JOIN LEARNING_TO_NOTE.USER_DOCUMENTS UD ON E1.USER_DOC_ID = UD.ID AND E2.USER_DOC_ID = UD.ID
        JOIN LEARNING_TO_NOTE.OFFSETS O1 ON O1.ENTITY_ID = E1.ID
        JOIN LEARNING_TO_NOTE.OFFSETS O2 ON O2.ENTITY_ID = E2.ID
        JOIN LEARNING_TO_NOTE."$TA_FTI" FTI1 ON FTI1.ID = UD.DOCUMENT_ID AND FTI1.TA_OFFSET = O1."START" AND FTI1.TA_TOKEN = E1.TEXT
        JOIN LEARNING_TO_NOTE."$TA_FTI" FTI2 ON FTI2.ID = UD.DOCUMENT_ID AND FTI2.TA_OFFSET = O2."START" AND FTI2.TA_TOKEN = E2.TEXT
        JOIN LEARNING_TO_NOTE."$TA_FTI" FTI ON FTI.ID = UD.DOCUMENT_ID
        WHERE UD.USER_ID = 'DDI-IMPORTER'
        AND FTI.TA_TYPE <> 'punctuation'
        AND FTI1.TA_SENTENCE = FTI2.TA_SENTENCE
        AND FTI.TA_SENTENCE = FTI1.TA_SENTENCE
        AND FTI.TA_COUNTER <> FTI1.TA_COUNTER
        AND FTI.TA_COUNTER <> FTI2.TA_COUNTER
        AND FTI1.TA_COUNTER < FTI2.TA_COUNTER

        --LIMIT 50000

        --AND UD.DOCUMENT_ID = 'DDI-DrugBank.d522'
        --AND E1_ID = 'DDI-DrugBank.d522.s1.e0'
        --ORDER BY E1_ID, E2_ID, TA_SENTENCE
        ;
    """)
    results = cursor.fetchall()
    print 'done'
    return results


def loadStopWords():
    stops = []
    with open('stop-word-list.txt', 'r') as f:
        for line in f:
            stops.append(line.translate(None, '\r\n'))
    return stops

def splitSamples(smaples):
    return smaples[:int(TEST_RATIO*len(smaples))], smaples[int(TEST_RATIO*len(smaples)):]


def read_words():
    true_pairs = {}
    false_pairs = {}

    for e1, e2, DDI, token, pos in entity_tokens():
        if DDI == 0:
            insert_pair(false_pairs, e1, e2, token, pos)
        else:
            insert_pair(true_pairs, e1, e2, token, pos)

    return true_pairs.values(), false_pairs.values()

def insert_pair(pairs, e1, e2, token, pos):
    pairs.setdefault((e1, e2), ([],[],[]))
    if pos < 0:
        pairs[e1, e2][0].append(token)
    elif pos == 0:
        pairs[e1, e2][1].append(token)
    else:
        pairs[e1, e2][2].append(token)

######### SKLEARN PIPELINE #########
class SentencePartExtractor(BaseEstimator, TransformerMixin):
    def fit(self, x, y=None):
        return self

    def transform(self, triples):
        features = {}
        features['before'] = []
        features['between'] = []
        features['after'] = []
        for before, between, after in triples:
            features['before'].append(' '.join(before))
            features['between'].append(' '.join(between))
            features['after'].append(' '.join(after))
        return features

class ItemSelector(BaseEstimator, TransformerMixin):
    def __init__(self, key):
        self.key = key

    def fit(self, x, y=None):
        return self

    def transform(self, data_dict):
        return data_dict[self.key]

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
def test():
    t0 = t1 = time.time()
    print 'reading from hana'
    true_samples, false_samples = read_words()
    print 'that needed {}'.format(time.time() - t1)
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

    t2 = time.time()
    print 'Training...'
    pipeline.fit(train_sentences, train_targets)
    print 'that needed {}'.format(time.time() - t2)


    t3 = time.time()
    print 'Predicting...'
    predicted = pipeline.predict(test_sentences)
    print 'that needed {}'.format(time.time() - t3)


    print(metrics.classification_report(test_targets, predicted))
    # print(metrics.roc_curve(test_targets, predicted))

    print 'the whole thing needed {}'.format(time.time() - t0)
    return test_targets, predicted

test()
# types = ['advise', 'effect', 'mechanism', 'int']
# for i in types:
#     targets, predicted = test(i)
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
