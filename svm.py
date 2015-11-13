import xml.etree.ElementTree as ET
import os
import collections
import sys

from sklearn import metrics
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.svm import LinearSVC



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

                            e1_offsets = get_offsets(e1)
                            e2_offsets = get_offsets(e2)

                            a = words_before(text, e1_offsets, e2_offsets)
                            b = words_between(text, e1_offsets, e2_offsets)
                            c = words_after(text, e1_offsets, e2_offsets)

                            print text
                            text1 = e1.get('text')
                            text2 = e2.get('text')
                            print "{} ({}) {} ({}) {}".format(a, text1, b, text2, c)
                            print

                            # pair_text = pair_text.strip(' ,.:!;?')
                            #
                            # sentences.append((pair_text, pair.get('ddi') == "true"))
                            # if filename == 'Aprepitant_ddi.xml':
                            #     print text
                            #     print e1.get('text')
                            #     print e2.get('text')
                            #     print pair_text
                            #     print begin, end

        return sentences

between_pairs(sys.argv[1])

# def loadStopWords():
#     stops = []
#     with open('stop-word-list.txt', 'r') as f:
#         for line in f:
#             stops.append(line.translate(None, '\r\n'))
#     return stops
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
