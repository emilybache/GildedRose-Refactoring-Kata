# Copyright 2012 Yelp
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
"""Return all input lines matching the given expression."""
from mrjob.job import MRJob
from mrjob.util import cmd_line


class MRGrepJob(MRJob):

    def configure_args(self):
        super(MRGrepJob, self).configure_args()

        self.add_passthru_arg(
            '-e', '--expression',
            required=True,
            help=('Expression to search for. Required.'))

    def mapper_cmd(self):
        # grep will return exit status 1 if no matching lines are found
        return cmd_line([
            'sh', '-c',
            'grep -e %s || RC=$?; if [ $RC -ne 1 ]; then (exit $RC); fi' % (
                cmd_line([self.options.expression]))])


if __name__ == '__main__':
    MRGrepJob.run()
