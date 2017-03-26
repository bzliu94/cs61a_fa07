# 2017-03-24

import nltk

# sudo pip install -U nltk

# nltk.download()

"""

file_ids = nltk.corpus.shakespeare.fileids()

for file_id in file_ids:
  print file_id
  # XMLCorpusReader
  print nltk.corpus.shakespeare.words(file_id)

"""

import re

import sys

# file_ids = nltk.corpus.gutenberg.fileids()

file_ids = ["shakespeare-caesar.txt", "shakespeare-hamlet.txt", "shakespeare-macbeth.txt"]

# file_ids = ["shakespeare-caesar.txt"]

file_short_names = ["caesar", "hamlet", "macbeth"]

print "(define shakespeare-short"

sys.stdout.write("  '(")

lines = []

for i in xrange(len(file_ids)):
  file_id = file_ids[i]
  file_short_name = file_short_names[i]
  # print file_id
  # PlaintextCorpusReader
  sentences = nltk.corpus.gutenberg.sents(file_id)
  next_sentences = []
  for sentence in sentences:
    next_sentence = []
    for element in sentence:
      next_element = re.sub(r'\W+', '', element)
      # next_element = element
      if len(next_element) != 0:
        next_sentence.append(next_element)
    if len(next_sentence) != 0:
      next_sentences.append(next_sentence)
  lower_sentences = [[x.lower() for x in s] for s in next_sentences]
  k_v_pairs = [(file_short_name, lower_sentence) for lower_sentence in lower_sentences]
  expressions = ["(" + p[0] + " " + reduce(lambda x, y: x + " " + y, p[1]) + ")" for p in k_v_pairs]
  line = reduce(lambda x, y: x + " " + y, expressions)
  lines.append(line)

# print "a", lines[0]

for i in xrange(len(lines)):

  # print i

  if i != 0:
    sys.stdout.write("    ")

  line = lines[i]

  sys.stdout.write(line)

  if i != len(lines) - 1:
    print

sys.stdout.write("))")

# tokenizers/punkt


