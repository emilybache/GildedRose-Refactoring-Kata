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
import re
import sys
from operator import add

from pyspark import SparkContext


WORD_RE = re.compile(r"[\w']+")


def main():
    # read in input, output path
    args = sys.argv[1:]

    if len(args) != 2:
        raise ValueError

    inputPath, outputPath = args

    sc = SparkContext(appName='mrjob Spark wordcount script')

    lines = sc.textFile(inputPath)

    # lines.flatMap(WORD_RE.findall) doesn't work on Spark 1.6.2; apparently
    # it can't serialize instance methods?
    counts = (
        lines.flatMap(lambda line: [w.lower() for w in WORD_RE.findall(line)])
        .map(lambda word: (word, 1))
        .reduceByKey(add))

    counts.saveAsTextFile(outputPath)

    sc.stop()


if __name__ == '__main__':
    main()
