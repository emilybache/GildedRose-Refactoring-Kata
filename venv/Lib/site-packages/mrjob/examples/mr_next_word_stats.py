# Copyright 2011 Yelp
# Copyright 2013 David Marin
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
"""For any word that appears in a document, compute stats about which
words come next (including percentage).

This is meant as a simple demonstration of why SORT_VALUES is useful.
"""
from mrjob.job import MRJob
from mrjob.step import MRStep
import re


WORD_RE = re.compile(r"[\w']+")


class MRNextWordStats(MRJob):

    SORT_VALUES = True

    def steps(self):
        return [MRStep(mapper=self.m_find_words,
                       combiner=self.c_combine_counts,
                       reducer=self.r_sum_counts),
                MRStep(reducer=self.r_compute_stats)]

    def m_find_words(self, _, line):
        """Tokenize lines, and look for pairs of adjacent words.

        Yield (prev_word, word), 1 and (prev_word, '*'), 1 for each pair
        """
        prev_word = None

        for word in WORD_RE.findall(line):
            word = word.lower()

            if prev_word is not None:
                # total up the number of times prev_word appears
                # and the number of times next_word appears after it
                yield (prev_word, '*'), 1
                yield (prev_word, word), 1

            prev_word = word

    def c_combine_counts(self, key, counts):
        """Sum up all those 1s before passing data off to the reducer"""
        yield key, sum(counts)

    def r_sum_counts(self, key, counts):
        """Compute the number of times each pair of words appears, and the
        number of times the first word in a pair appears, and send it to
        a reducer that keys on the first word in the pair.
        """
        count = sum(counts)

        prev_word, word = key

        if word == '*':
            # we want total to arrive at r_compute_stats first, so
            # prefix it with "A", which comes before "B"
            yield prev_word, ('A: total', count)
        else:
            yield prev_word, ('B: stats', (word, count))

    def r_compute_stats(self, prev_word, value):
        """For each pair of words, compute how many times it appears,
        how many times the first word appears in a pair, and the percentage
        of time the second word follows the first.

        This relies on values appearing in sorted order; we need the total
        number of times the first word appears before we can compute the
        percentage for each second word.
        """
        total = None

        for value_type, data in value:
            if value_type == 'A: total':
                total = data
            else:
                assert value_type == 'B: stats'
                word, count = data
                # A comes before B, so total should already be set
                percent = 100.0 * count / total
                yield (prev_word, word), (total, count, percent)


if __name__ == '__main__':
    MRNextWordStats.run()
