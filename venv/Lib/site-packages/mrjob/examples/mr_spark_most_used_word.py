#!/usr/bin/python
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
"""Like mr_most_used_word.py, but on Spark.

To make this work on the local[*] master, pass in your own --stop-words-file
(it won't be able to see stop_words.txt because --files doesn't work
on local[*] master)
"""
import json
import re
from operator import add

from mrjob.job import MRJob

WORD_RE = re.compile(r"[\w']+")


class MRSparkMostUsedWord(MRJob):

    FILES = ['stop_words.txt']

    def configure_args(self):
        super(MRSparkMostUsedWord, self).configure_args()

        # allow for alternate stop words file
        self.add_file_arg(
            '--stop-words-file',
            dest='stop_words_file',
            default=None,
            help='alternate stop words file. lowercase words, one per line',
        )

    def spark(self, input_path, output_path):
        from pyspark import SparkContext

        sc = SparkContext()

        lines = sc.textFile(input_path)

        # do a word frequency count
        words_and_ones = lines.mapPartitions(self.get_words)
        word_counts = words_and_ones.reduceByKey(add)

        # pick pair with highest count (now in count, word format)
        max_count, word = word_counts.map(lambda w_c: (w_c[1], w_c[0])).max()

        # output our word
        output = sc.parallelize([json.dumps(word)])
        output.saveAsTextFile(output_path)

        sc.stop()

    def get_words(self, line_iterator):
        # this only happens once per partition
        stop_words = self.load_stop_words()

        for line in line_iterator:
            for word in WORD_RE.findall(line):
                word = word.lower()
                if word not in stop_words:
                    yield (word, 1)

    def load_stop_words(self):
        # this should only be called inside executors (i.e. inside functions
        # passed to RDDs)
        stop_words_path = self.options.stop_words_file or 'stop_words.txt'

        with open(stop_words_path) as f:
            return set(line.strip() for line in f)


if __name__ == '__main__':
    MRSparkMostUsedWord.run()
