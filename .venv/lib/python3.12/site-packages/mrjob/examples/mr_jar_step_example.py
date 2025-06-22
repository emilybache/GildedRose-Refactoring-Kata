# Copyright 2013 David Marin
# Copyright 2016 Yelp
# Copyright 2018 Yelp and Google Inc.
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
"""A simple example of linking a hadoop jar with a Python step. This
calculates the frequency of various word frequencies in a text file.

This example works out-of-the box on EMR and Google Cloud Dataproc.

This also only works on a single input path/directory, due to limitations
of the example jar.
"""
from mrjob.job import MRJob
from mrjob.protocol import RawProtocol
from mrjob.step import GENERIC_ARGS
from mrjob.step import INPUT
from mrjob.step import JarStep
from mrjob.step import MRStep
from mrjob.step import OUTPUT

# use the file:// trick to access a jar hosted on the cloud
_EXAMPLES_JAR_URI = (
    'file:///usr/lib/hadoop-mapreduce/hadoop-mapreduce-examples.jar')

_EMR_HADOOP_PRE_4_X_JAR_URI = (
    'file:///home/hadoop/hadoop-examples.jar')

_WORDCOUNT_MAIN_CLASS = 'org.apache.hadoop.examples.WordCount'


class MRJarStepExample(MRJob):
    """A contrived example that runs wordcount from the hadoop example
    jar, and then does a frequency count of the frequencies."""

    def configure_args(self):
        super(MRJarStepExample, self).configure_args()

        self.add_passthru_arg(
            '--use-main-class', dest='use_main_class',
            default=False, action='store_true')

        self.add_passthru_arg(
            '--examples-jar', dest='examples_jar',
            default=_EXAMPLES_JAR_URI,
            help=('URI of Hadoop example jar. Usually the default'
                  ' is fine, but on EMR, you should use '
                  ' file:///home/hadoop/hadoop-examples.jar'
                  ' with 3.x AMIs and earlier')
        )

    def steps(self):
        jar = self.options.examples_jar

        if self.options.use_main_class:
            jar_step = JarStep(
                jar=jar,
                args=[GENERIC_ARGS, INPUT, OUTPUT],
                main_class=_WORDCOUNT_MAIN_CLASS,
            )
        else:
            jar_step = JarStep(
                jar=jar,
                args=['wordcount', GENERIC_ARGS, INPUT, OUTPUT],
            )

        return [
            jar_step,
            MRStep(
                mapper=self.mapper,
                combiner=self.reducer,
                reducer=self.reducer,
            )
        ]

    def mapper(self, key, freq):
        yield int(freq), 1

    def reducer(self, freq, counts):
        yield freq, sum(counts)

    def pick_protocols(self, step_num, step_type):
        """Use RawProtocol to read output from the jar."""
        read, write = super(MRJarStepExample, self).pick_protocols(
            step_num, step_type)
        if (step_num, step_type) == (1, 'mapper'):
            read = RawProtocol().read

        return read, write


if __name__ == '__main__':
    MRJarStepExample.run()
