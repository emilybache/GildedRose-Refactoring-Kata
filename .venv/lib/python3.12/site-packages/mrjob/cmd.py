# Copyright 2012 Yelp
# Copyright 2014 Yelp and Contributors
# Copyright 2015-2018 Yelp
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
from __future__ import print_function

from sys import argv
from sys import stderr

# map from command name to function to call
commands = {}

# map from command name to description for help
descriptions = {}

usage = """usage: mrjob {subcommand|--help}"

subcommands:"""


def _error(msg=None):
    if msg:
        print(msg, file=stderr)

    longest_name = max(len(name) for name in descriptions)

    def subcommand_line(name):
        spaces = ' ' * (longest_name - len(name))
        return '  %s: %s%s' % (
            name, spaces, descriptions[name])
    print(usage, file=stderr)
    print('\n'.join(
        subcommand_line(name) for name in sorted(descriptions)), file=stderr)


def _command(name, description=None):
    """Decorate a function used to call a command.

    If you don't set *description*, it won't be included in help
    (useful for deprecated commands)."""
    def decorator(f):
        commands[name] = f
        if description:
            descriptions[name] = description
        return f
    return decorator


def main(args=None):
    args = args or argv
    if not args[1:] or args[1] in ('-h', '--help'):
        _error()
    elif args[1] not in commands:
        _error('"%s" is not a command' % args[1])
    else:
        commands[args[1]](args[2:])


@_command('audit-emr-usage', 'Audit EMR usage')
def _audit_usage(args):
    from mrjob.tools.emr.audit_usage import main
    main(args)


@_command('create-cluster', 'Create a persistent EMR cluster')
def _create_cluster(args):
    from mrjob.tools.emr.create_cluster import main
    main(args)


@_command('diagnose', 'Diagnose cause of job failure')
def _diagnose(args):
    from mrjob.tools.diagnose import main
    main(args)


@_command('boss', 'Run a command on every node of a cluster.')
def _mrboss(args):
    from mrjob.tools.emr.mrboss import main
    main(args)


@_command('report-long-jobs', 'Report EMR jobs which have been running for a'
          ' long time')
def _report_long_jobs(args):
    from mrjob.tools.emr.report_long_jobs import main
    main(args)


@_command('s3-tmpwatch', 'Delete S3 keys older than a specified time')
def _s3_tmpwatch(args):
    from mrjob.tools.emr.s3_tmpwatch import main
    main(args)


@_command('spark-submit', 'Submit Spark jobs')
def _spark_submit(args):
    from mrjob.tools.spark_submit import main
    main(args)


@_command('terminate-idle-clusters', 'Terminate idle EMR clusters')
def _terminate_idle_clusters(args):
    from mrjob.tools.emr.terminate_idle_clusters import main
    main(args)


@_command('terminate-cluster', 'Terminate a single EMR cluster')
def _terminate_cluster(args):
    from mrjob.tools.emr.terminate_cluster import main
    main(args)


if __name__ == '__main__':
    main()
