# Copyright 2016 Yelp
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
"""Word frequency count with output arranged into subdirectories by
first letter, using nicknack.MultipleValueOutputFormat
"""
import json
import re

from mrjob.job import MRJob
from mrjob.protocol import RawValueProtocol

# restrict to a-z so we don't get odd filenames
WORD_RE = re.compile(r"[A-Za-z]+")


class MRNickNack(MRJob):

    # tell hadoop to massage our mrjob output using this output format
    HADOOP_OUTPUT_FORMAT = 'nicknack.MultipleValueOutputFormat'

    # use the nicknack JAR in this directory
    #
    # This JAR was downloaded from https://github.com/empiricalresults/nicknack
    # and is also under the Apache 2.0 license.
    LIBJARS = ['nicknack-1.0.1.jar']

    # tell mrjob not to format our output -- leave that to hadoop
    OUTPUT_PROTOCOL = RawValueProtocol

    def mapper(self, _, line):
        for word in WORD_RE.findall(line):
            yield (word.lower(), 1)

    def reducer(self, word, counts):
        total = sum(counts)
        yield None, '\t'.join([word[0], json.dumps(word), json.dumps(total)])


if __name__ == '__main__':
    MRNickNack.run()
