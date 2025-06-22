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
from mrjob.job import MRJob


class MRSparKaboom(MRJob):

    def spark(self, input_path, output_path):
        # Spark may not be available where script is launched
        from pyspark import SparkContext

        sc = SparkContext(appName='mrjob Spark wordcount script')

        lines = sc.textFile(input_path)

        def kaboom(line):
            raise Exception('KABOOM')

        # make sure the exception happens inside Spark, not just at the
        # top-level client

        # strangely, Spark 1.2 thinks this is all good. Probably not something
        # we can fix.
        kaboomed_lines = lines.flatMap(kaboom)
        kaboomed_lines.saveAsTextFile(output_path)

        sc.stop()


if __name__ == '__main__':
    MRSparKaboom.run()
