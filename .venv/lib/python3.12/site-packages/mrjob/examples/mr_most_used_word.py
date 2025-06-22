#!/usr/bin/python
# Copyright 2009-2010 Yelp
# Copyright 2013 David Marin
# Copyright 2018 Yelp
# Copyright 2019 Yelp
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""Determine the most used word in the input, ignoring common "stop" words.

Shows how to do a multi-step job, and how to load a support file
from the same directory.
"""
import re

from mrjob.job import MRJob
from mrjob.protocol import JSONValueProtocol
from mrjob.step import MRStep

WORD_RE = re.compile(r"[\w']+")


class MRMostUsedWord(MRJob):
    FILES = ['stop_words.txt']

    OUTPUT_PROTOCOL = JSONValueProtocol

    def configure_args(self):
        super(MRMostUsedWord, self).configure_args()

        # allow for alternate stop words file
        self.add_file_arg(
            '--stop-words-file',
            dest='stop_words_file',
            default=None,
            help='alternate stop words file. lowercase words, one per line',
        )

    def mapper_init(self):
        stop_words_path = self.options.stop_words_file or 'stop_words.txt'

        with open(stop_words_path) as f:
            self.stop_words = set(line.strip() for line in f)

    def mapper_get_words(self, _, line):
        # yield each word in the line
        for word in WORD_RE.findall(line):
            word = word.lower()
            if word not in self.stop_words:
                yield (word, 1)

    def combiner_count_words(self, word, counts):
        # sum the words we've seen so far
        yield (word, sum(counts))

    def reducer_count_words(self, word, counts):
        # send all (num_occurrences, word) pairs to the same reducer.
        # num_occurrences is so we can easily use Python's max() function.
        yield None, (sum(counts), word)

    # discard the key; it is just None
    def reducer_find_max_word(self, _, word_count_pairs):
        # each item of word_count_pairs is (count, word),
        # so yielding one results in key=counts, value=word
        try:
            yield max(word_count_pairs)
        except ValueError:
            pass

    def steps(self):
        return [
            MRStep(mapper_init=self.mapper_init,
                   mapper=self.mapper_get_words,
                   combiner=self.combiner_count_words,
                   reducer=self.reducer_count_words),
            MRStep(reducer=self.reducer_find_max_word)
        ]


if __name__ == '__main__':
    MRMostUsedWord.run()
