# Copyright 2011 Yelp
# Copyright 2017 Yelp
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
"""
MapReduce job to sample n lines from a file. The mapper iterates over each
line and yields them to the reducer, combined with a random seed, so that
Hadoop will resort the lines. Then, the reducer yields the first n lines.
"""
__author__ = 'Benjamin Goldenberg <benjamin@yelp.com>'

import random
import sys

from mrjob.job import MRJob
from mrjob.protocol import RawValueProtocol, ReprProtocol

SAMPLING_FUDGE_FACTOR = 1.2


class MRLogSampler(MRJob):
    # We use RawValueProtocol for input to be format agnostic
    # and avoid any type of parsing errors
    INPUT_PROTOCOL = RawValueProtocol

    # We use RawValueProtocol for output so we can output raw lines
    # instead of (k, v) pairs
    OUTPUT_PROTOCOL = RawValueProtocol

    # Encode the intermediate records using repr() instead of JSON, so the
    # record doesn't get Unicode-encoded
    INTERNAL_PROTOCOL = ReprProtocol

    # random seed doesn't do any good unless we sort by it
    SORT_VALUES = True

    def configure_args(self):
        super(MRLogSampler, self).configure_args()
        self.add_passthru_arg(
            '--sample-size',
            type=int,
            help='Number of entries to sample.'
        )
        self.add_passthru_arg(
            '--expected-length',
            type=int,
            help=("Number of entries you expect in the log. If not specified,"
                  " we'll pass every line to the reducer.")
        )

    def load_args(self, args):
        super(MRLogSampler, self).load_args(args)

        if self.options.sample_size is None:
            self.arg_parser.error('You must specify the --sample-size')
        else:
            self.sample_size = self.options.sample_size

        # If we have an expected length, we can estimate the sampling
        # probability for the mapper, so that the reducer doesn't have to
        # process all records. Otherwise, pass everything thru to the reducer.
        if self.options.expected_length is None:
            self.sampling_probability = 1
        else:
            # We should be able to bound this probability by using the binomial
            # distribution, but I haven't figured it out yet. So, let's just
            # fudge it.
            self.sampling_probability = (float(self.sample_size) *
                                         SAMPLING_FUDGE_FACTOR /
                                         self.options.expected_length)

    def mapper(self, _, line):
        """
        For each log line, with probability self.sampling_probability,
        yield a None key, and (random seed, line) as the value, so that
        the values get sorted randomly and fed into a single reducer.

        Args:
            line - raw log line

        Yields:
            key - None
            value - (random seed, line)
        """
        if random.random() < self.sampling_probability:
            seed = '%20i' % random.randint(0, sys.maxsize)
            yield None, (seed, line)

    def reducer(self, _, values):
        """
        Now that the values have a random number attached,
        they'll come in in random order, so we yield the
        first n lines, and return early.

        Args:
            values - generator of (random_seed, line) pairs

        Yields:
            key - None
            value - random sample of log lines
        """
        for line_num, (seed, line) in enumerate(values):
            yield None, line

            # enumerate() is 0-indexed, so add 1
            if line_num + 1 >= self.sample_size:
                break


if __name__ == '__main__':
    MRLogSampler.run()
