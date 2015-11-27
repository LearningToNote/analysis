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
                        if (pair.get('ddi') == "true"):
                            sentences.append((text, True))
                            found = True
                    if not found:
                        sentences.append((text, False))
    return sentences


def between_pairs(filepath):
        sentences = []
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


                            # if there are multiple offsets take min and max of all offsets
                            offset_begins = []
                            offset_ends = []
                            e_1_offsets = e1.get('charOffset').split(';')
                            for offset in e_1_offsets:
                                b, e = map(int, offset.split('-'))
                                offset_begins.append(b)
                                offset_ends.append(e)

                            e_2_offsets = e2.get('charOffset').split(';')
                            for offset in e_2_offsets:
                                b, e = map(int, offset.split('-'))
                                offset_begins.append(b)
                                offset_ends.append(e)

                            begin, end = min(offset_ends) + 1, max(offset_begins)

                            pair_text = text[begin:end]
                            pair_text = pair_text.strip(' ,.:!;?')

                            sentences.append((pair_text, pair.get('ddi') == "true"))
                            if filename == 'Aprepitant_ddi.xml':
                                print text
                                print e1.get('text')
                                print e2.get('text')
                                print pair_text
                                print begin, end

        return sentences

def loadStopWords():
    stops = []
    with open('stop-word-list.txt', 'r') as f:
        for line in f:
            stops.append(line.translate(None, '\r\n'))
    return stops


filepath = sys.argv[1]

print 'Reading sentences...'
# all_sentences = sentences(filepath)
all_sentences = between_pairs(filepath)

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
