import xml.etree.ElementTree as ET
import os
import collections
import sys

from sklearn.feature_extraction.text import CountVectorizer
filepath = sys.argv[1]

vectorizer = CountVectorizer(min_df=1, stop_words=loadStopWords())
X = vectorizer.fit_transform(sentences(filepath))
print(X)



pair_types = ['effect', 'mechanism', 'advise', 'int']
entity_types = ['drug', 'drug_n', 'group', 'brand']

def find_sentence(filename):
    sentences = []

    tree = ET.parse(filename)
    root = tree.getroot()

    for document in root.iter('document'):
        for sentence in document.findall('sentence'):
            # tokens = []
            for pair in sentence.findall('pair'):
                ddi = pair.get('ddi')
                if (ddi == "true") and (pair.get('type') == sys.argv[2]):
                    text = sentence.get('text')
                    # text = sentence.get('text').split(' ')
                    # for w in text:
                    #     w = w.lower()
                    #     w = w.translate(None, '.,%;:*!@#$&*()+=')
                    #     tokens.append(w)
                    sentences.append(text)
    return sentences


def sentences(filepath):
    sentences = []
    for filename in os.listdir(filepath):
       if (".xml" in filename):
           sentences.extend(find_sentence(filename))

    if sys.argv[2] == 'stop':
        stops = loadStopWords()
        for sentence in sentences:
            for word in sentence:
                if word in stops:
                    sentence.remove(word)

    return sentences

def loadStopWords():
    stops = []
    with open('stop-word-list.txt', 'r') as f:
        for line in f:
        stops.append(line.translate(None, '\r\n'))
    return stops
