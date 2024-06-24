# Copyright 2016 Yelp
# Copyright 2017 Yelp and Contributors
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
"""The classic MapReduce job, but only for words containing "u"
"""
from mrjob.job import MRJob
import re

WORD_RE = re.compile(r"[\w']*u[\w']*", re.I)


class MRWordsContainingUFreqCount(MRJob):

    def mapper_pre_filter(self):
        # no need to account for grep exiting with status 1 if no matches,
        # since pre-filters are piped into another process. Compare to
        # mr_grep.py
        return 'grep -i u'

    def mapper(self, _, line):
        for word in WORD_RE.findall(line):
            yield (word.lower(), 1)

    def combiner(self, word, counts):
        yield (word, sum(counts))

    def reducer(self, word, counts):
        yield (word, sum(counts))


if __name__ == '__main__':
    MRWordsContainingUFreqCount.run()
