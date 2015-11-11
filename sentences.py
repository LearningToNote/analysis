import xml.etree.ElementTree as ET
import os
import collections
import sys

from sklearn import metrics
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.svm import LinearSVC




pair_types = ['effect', 'mechanism', 'advise', 'int']
entity_types = ['drug', 'drug_n', 'group', 'brand']

def sentences(filepath):
    sentences = []
    for filename in os.listdir(filepath):
       if (".xml" in filename):
            tree = ET.parse(filepath+filename)
            root = tree.getroot()

            for document in root.iter('document'):
                for sentence in document.findall('sentence'):
                    found = False
                    text = sentence.get('text')
                    for pair in sentence.findall('pair'):
                        if (pair.get('ddi') == "true" and pair.get('type') == sys.argv[2]):
                            sentences.append((text, True))
                            found = True
                    if not found:
                        sentences.append((text, False))
    return sentences


def loadStopWords():
    stops = []
    with open('stop-word-list.txt', 'r') as f:
        for line in f:
            stops.append(line.translate(None, '\r\n'))
    return stops


filepath = sys.argv[1]

print 'Reading sentences...'
all_sentences = sentences(filepath)

train_tuples, test_tuples = all_sentences[:int(0.7*len(all_sentences))], all_sentences[int(0.7*len(all_sentences)):]
sentence_data = [i[0] for i in train_tuples]
sentence_target = [i[1] for i in train_tuples]
test_target = [i[1] for i in test_tuples]
test_data = [i[0] for i in test_tuples]

print 'Training...'
clf = Pipeline([('vect', CountVectorizer(stop_words = loadStopWords())),
               ('clf', LinearSVC())
               ])

clf.fit(sentence_data, sentence_target)

print 'Predicting...'
predicted = clf.predict(test_data)


print(metrics.classification_report(test_target, predicted))
