import xml.etree.ElementTree as ET
import os
import json
import collections
import sys
import matplotlib.pyplot as plt; plt.rcdefaults()
import numpy as np
import matplotlib.pyplot as plt


filepath = sys.argv[1]
files = []
sentences = []

pair_types = ['effect', 'mechanism', 'advise', 'int']
entity_types = ['drug', 'drug_n', 'group', 'brand']

def find_sentence(filename):
    tree = ET.parse(filename)
    root = tree.getroot()

    for document in root.iter('document'):
        for sentence in document.findall('sentence'):
            tokens = []
            for pair in sentence.findall('pair'):
                ddi = pair.get('ddi')
                if (ddi == "true") and (pair.get('type') == sys.argv[2]):
                    text = sentence.get('text').split(' ')
                    for w in text:
                        w = w.lower()
                        w = w.translate(None, '.,%;:*!@#$&*()+=')
                        tokens.append(w)
            sentences.append(tokens)


def tokenLists(filepath):
    for filename in os.listdir(filepath):
       if (".xml" in filename):
           files.append(filepath+filename)
           find_sentence(filename)

    if sys.argv[2] == 'stop':
        stops = loadStopWords()


        for stop in stops:
            try:
                del counted_words[stop]
            except(KeyError): pass

def loadStopWords():
    stops = []
    with open('stop-word-list.txt', 'r') as f:
        for line in f:
        stops.append(line.translate(None, '\r\n'))
    return stops
