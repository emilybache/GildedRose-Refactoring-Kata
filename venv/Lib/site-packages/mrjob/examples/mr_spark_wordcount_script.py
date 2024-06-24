# Copyright 2016 Yelp
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
from os.path import dirname
from os.path import join

from mrjob.job import MRJob
from mrjob.step import INPUT
from mrjob.step import OUTPUT
from mrjob.step import SparkScriptStep


class MRSparkScriptWordcount(MRJob):

    def steps(self):
        return [
            SparkScriptStep(
                script=join(dirname(__file__), 'spark_wordcount_script.py'),
                args=[INPUT, OUTPUT],
            ),
        ]


if __name__ == '__main__':
    MRSparkScriptWordcount.run()
