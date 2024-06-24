# Copyright 2018 Yelp
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
"""A drop-in replacement for :command:`spark-submit` that can use mrjob's
runners. For example, you can submit your spark job to EMR just by adding
``-r emr``.

This also adds a few mrjob features that are not standard with
:command:`spark-submit`, such as ``--cmdenv``, ``--dirs``, and ``--setup``.

.. versionadded:: 0.6.7

.. versionchanged:: 0.6.8

   added ``local``, ``spark`` runners, made ``spark`` the default (was
   ``hadoop``)

.. versionchanged:: 0.7.1

   ``--archives`` and ``--dirs`` are supported on all masters (except local)

Usage::

    mrjob spark-submit [-r <runner>] [options] <python file | app jar>
    [app arguments]

Options::

 All runners:
  -r {emr,hadoop,local,spark}, --runner {emr,hadoop,local,spark}
                        Where to run the job (default: "spark")
  --class MAIN_CLASS    Your application's main class (for Java / Scala apps).
  --name NAME           The name of your application.
  --jars LIBJARS        Comma-separated list of jars to include on the
                        driverand executor classpaths.
  --packages PACKAGES   Comma-separated list of maven coordinates of jars to
                        include on the driver and executor classpaths. Will
                        search the local maven repo, then maven central and
                        any additional remote repositories given by
                        --repositories. The format for the coordinates should
                        be groupId:artifactId:version.
  --exclude-packages EXCLUDE_PACKAGES
                        Comma-separated list of groupId:artifactId, to exclude
                        while resolving the dependencies provided in
                        --packages to avoid dependency conflicts.
  --repositories REPOSITORIES
                        Comma-separated list of additional remote repositories
                        to search for the maven coordinates given with
                        --packages.
  --py-files PY_FILES   Comma-separated list of .zip, .egg, or .py files to
                        placed on the PYTHONPATH for Python apps.
  --files UPLOAD_FILES  Comma-separated list of files to be placed in the
                        working directory of each executor. Ignored on
                        local[*] master.
  --archives UPLOAD_ARCHIVES
                        Comma-separated list of archives to be extracted into
                        the working directory of each executor.
  --dirs UPLOAD_DIRS    Comma-separated list of directors to be archived and
                        then extracted into the working directory of each
                        executor.
  --cmdenv CMDENV       Arbitrary environment variable to set inside Spark, in
                        the format NAME=VALUE.
  --conf JOBCONF        Arbitrary Spark configuration property, in the format
                        PROP=VALUE.
  --setup SETUP         A command to run before each Spark executor in the
                        shell ("touch foo"). In cluster mode, runs before the
                        Spark driver as well. You may interpolate files
                        available via URL or on your local filesystem using
                        Hadoop Distributed Cache syntax (". setup.sh#"). To
                        interpolate archives (YARN only), use #/: "cd
                        foo.tar.gz#/; make.
  --properties-file PROPERTIES_FILE
                        Path to a file from which to load extra properties. If
                        not specified, this will look for conf/spark-
                        defaults.conf.
  --driver-memory DRIVER_MEMORY
                        Memory for driver (e.g. 1000M, 2G) (Default: 1024M).
  --driver-java-options DRIVER_JAVA_OPTIONS
                        Extra Java options to pass to the driver.
  --driver-library-path DRIVER_LIBRARY_PATH
                        Extra library path entries to pass to the driver.
  --driver-class-path DRIVER_CLASS_PATH
                        Extra class path entries to pass to the driver. Note
                        that jars added with --jars are automatically included
                        in the classpath.
  --executor-memory EXECUTOR_MEMORY
                        Memory per executor (e.g. 1000M, 2G) (Default: 1G).
  --proxy-user PROXY_USER
                        User to impersonate when submitting the application.
                        This argument does not work with --principal /
                        --keytab.
  -c CONF_PATHS, --conf-path CONF_PATHS
                        Path to alternate mrjob.conf file to read from
  --no-conf             Don't load mrjob.conf even if it's available
  -q, --quiet           Don't print anything to stderr
  -v, --verbose         print more messages to stderr
  -h, --help            show this message and exit

Spark and Hadoop runners only:
  --master SPARK_MASTER
                        spark://host:port, mesos://host:port,
                        yarn,k8s://https://host:port, or local. Defaults to
                        local[*] on spark runner, yarn on hadoop runner.
  --deploy-mode SPARK_DEPLOY_MODE
                        Whether to launch the driver program locally
                        ("client") or on one of the worker machines inside the
                        cluster ("cluster") (Default: client).

Cluster deploy mode only:
  --driver-cores DRIVER_CORES
                        Number of cores used by the driver (Default: 1).

Spark standalone or Mesos with cluster deploy mode only:
  --supervise           If given, restarts the driver on failure.

Spark standalone and Mesos only:
  --total-executor-cores TOTAL_EXECUTOR_CORES
                        Total cores for all executors.

Spark standalone and YARN only:
  --executor-cores EXECUTOR_CORES
                        Number of cores per executor. (Default: 1 in YARN
                        mode, or all available cores on the worker in
                        standalone mode)

YARN-only:
  --queue QUEUE_NAME    The YARN queue to submit to (Default: "default").
  --num-executors NUM_EXECUTORS
                        Number of executors to launch (Default: 2). If dynamic
                        allocation is enabled, the initial number of executors
                        will be at least NUM.
  --principal PRINCIPAL
                        Principal to be used to login to KDC, while running
                        onsecure HDFS.
  --keytab KEYTAB       The full path to the file that contains the keytab for
                        the principal specified above. This keytab will be
                        copied to the node running the Application Master via
                        the Secure Distributed Cache, for renewing the login
                        tickets and the delegation tokens periodically.

This also supports the same runner-specific switches as
:py:class:`~mrjob.job.MRJob`\\s (e.g. ``--hadoop-bin``, ``--region``).

"""
from __future__ import print_function

import os
import sys
from argparse import ArgumentParser
from argparse import REMAINDER
from argparse import SUPPRESS
from logging import getLogger

from mrjob.job import MRJob
from mrjob.options import _RUNNER_OPTS
from mrjob.options import _add_basic_args
from mrjob.options import _add_runner_args
from mrjob.options import _parse_raw_args
from mrjob.runner import _runner_class
from mrjob.step import SparkJarStep
from mrjob.step import SparkScriptStep

log = getLogger(__name__)


_USAGE = ('%(prog)s spark-submit [-r <runner>] [options]'
          ' <python file | app jar> [app arguments]')

_DESCRIPTION = 'Submit a spark job to Hadoop or the cloud'

_BASIC_HELP_EPILOG = (
    'To see help for a specific runner, use --help -r <runner name>')

_DEPRECATED_OPT_HELP = (
    'To include help for deprecated options, add --deprecated')


# for spark-submit args, just need switches and help message
# (which can be patched into runner opts with same dest name)

# then add runner opts (other than check_input_paths) but don't
# display in default help message

# the only runners that support spark scripts/jars
_SPARK_RUNNERS = ('emr', 'hadoop', 'local', 'spark')

# the default spark runner to use
_DEFAULT_RUNNER = 'spark'  # just find spark-submit and use it


# our mostly similar version of spark-submit's args, arranged in to groups
# for the --help message. Differences:
#
# spark_master (--master) is in its own "Spark and Hadoop runners only" group
# added upload_dirs (--dirs) which is similar to --archives
#
# --runner and other basic options are patched into the first ("None")
# argument group in _make_basic_help_parser(), below
_SPARK_SUBMIT_ARG_GROUPS = [
    (None, [
        'main_class',
        'name',
        'libjars',
        'packages',
        'exclude_packages',
        'repositories',
        'py_files',
        'upload_files',
        'upload_archives',
        'upload_dirs',
        'cmdenv',
        'jobconf',
        'setup',
        'properties_file',
        'driver_memory',
        'driver_java_options',
        'driver_library_path',
        'driver_class_path',
        'executor_memory',
        'proxy_user',
    ]),
    ('Spark and Hadoop runners only', [
        'spark_master',
        'spark_deploy_mode',
    ]),
    ('Cluster deploy mode only', [
        'driver_cores',
    ]),
    ('Spark standalone or Mesos with cluster deploy mode only', [
        'supervise',
        # --kill and --status aren't for launching jobs
    ]),
    ('Spark standalone and Mesos only', [
        'total_executor_cores',
    ]),
    ('Spark standalone and YARN only', [
        'executor_cores',
    ]),
    ('YARN-only', [
        'queue_name',
        'num_executors',
        'principal',
        'keytab',
    ]),
]

# lightly modified versions of help messages from spark-submit
_SPARK_SUBMIT_ARG_HELP = dict(
    cmdenv=('Arbitrary environment variable to set inside Spark, in the'
            ' format NAME=VALUE.'),
    driver_class_path=('Extra class path entries to pass to the driver. Note'
                       ' that jars added with --jars are automatically'
                       ' included in the classpath.'),
    driver_cores='Number of cores used by the driver (Default: 1).',
    driver_java_options='Extra Java options to pass to the driver.',
    driver_library_path='Extra library path entries to pass to the driver.',
    driver_memory='Memory for driver (e.g. 1000M, 2G) (Default: 1024M).',
    exclude_packages=('Comma-separated list of groupId:artifactId, to exclude'
                      ' while resolving the dependencies provided in'
                      ' --packages to avoid dependency conflicts.'),
    executor_cores=('Number of cores per executor. (Default: 1 in YARN mode,'
                    ' or all available cores on the worker in standalone'
                    ' mode)'),
    executor_memory='Memory per executor (e.g. 1000M, 2G) (Default: 1G).',
    jobconf=('Arbitrary Spark configuration property, in the format'
             ' PROP=VALUE.'),
    keytab=('The full path to the file that contains the keytab for the'
            ' principal specified above. This keytab will be copied to'
            ' the node running the Application Master via the Secure'
            ' Distributed Cache, for renewing the login tickets and the'
            ' delegation tokens periodically.'),
    libjars=('Comma-separated list of jars to include on the driver'
             'and executor classpaths.'),
    main_class="Your application's main class (for Java / Scala apps).",
    name='The name of your application.',
    num_executors=('Number of executors to launch (Default: 2).'
                   ' If dynamic allocation is enabled, the initial number of'
                   ' executors will be at least NUM.'),
    packages=('Comma-separated list of maven coordinates of jars to include'
              ' on the driver and executor classpaths. Will search the local'
              ' maven repo, then maven central and any additional remote'
              ' repositories given by --repositories. The format for the'
              ' coordinates should be groupId:artifactId:version.'),
    principal=('Principal to be used to login to KDC, while running on'
               'secure HDFS.'),
    properties_file=('Path to a file from which to load extra properties. If'
                     ' not specified, this will look for'
                     ' conf/spark-defaults.conf.'),
    proxy_user=('User to impersonate when submitting the application.'
                ' This argument does not work with --principal / --keytab.'),
    py_files=('Comma-separated list of .zip, .egg, or .py files to place'
              'on the PYTHONPATH for Python apps.'),
    queue_name='The YARN queue to submit to (Default: "default").',
    repositories=('Comma-separated list of additional remote repositories to'
                  ' search for the maven coordinates given with --packages.'),
    setup=('A command to run before each Spark executor in the'
           ' shell ("touch foo"). In cluster mode, runs before the Spark'
           ' driver as well. You may interpolate files'
           ' available via URL or on your local filesystem using'
           ' Hadoop Distributed Cache syntax (". setup.sh#"). To'
           ' interpolate archives (YARN only), use'
           ' #/: "cd foo.tar.gz#/; make.'),
    spark_deploy_mode=('Whether to launch the driver program locally'
                       ' ("client") or on one of the worker machines inside'
                       ' the cluster ("cluster") (Default: client).'),
    spark_master=('spark://host:port, mesos://host:port, yarn,'
                  'k8s://https://host:port, or local. Defaults'
                  ' to local[*] on spark runner, yarn on hadoop runner.'),
    supervise='If given, restarts the driver on failure.',
    total_executor_cores='Total cores for all executors.',
    upload_archives=('Comma-separated list of archives to be extracted into'
                     ' the working directory of each executor. Ignored on'
                     ' local[*] master.'),
    upload_dirs=('Comma-separated list of directors to be archived and then'
                 ' extracted into the working directory of each executor.'
                 ' Ignored on local[*] master.'),
    upload_files=('Comma-separated list of files to be placed in the working'
                  ' directory of each executor. Ignored on local[*] master.'),
)

_SPARK_SUBMIT_OPT_NAMES = {
    opt_name for _, opt_names in _SPARK_SUBMIT_ARG_GROUPS
    for opt_name in opt_names
}

_SPARK_SUBMIT_SWITCHES = dict(
    cmdenv='--cmdenv',
    driver_class_path='--driver-class-path',
    driver_cores='--driver-cores',
    driver_java_options='--driver-java-options',
    driver_library_path='--driver-library-path',
    driver_memory='--driver-memory',
    exclude_packages='--exclude-packages',
    executor_cores='--executor-cores',
    executor_memory='--executor-memory',
    jobconf='--conf',
    keytab='--keytab',
    libjars='--jars',
    main_class='--class',
    name='--name',
    num_executors='--num-executors',
    packages='--packages',
    principal='--principal',
    properties_file='--properties-file',
    proxy_user='--proxy-user',
    py_files='--py-files',
    queue_name='--queue',
    repositories='--repositories',
    setup='--setup',
    spark_deploy_mode='--deploy-mode',
    spark_master='--master',
    supervise='--supervise',
    total_executor_cores='--total-executor-cores',
    upload_archives='--archives',
    upload_dirs='--dirs',
    upload_files='--files',
)

# things that are different about specific spark submit args
_SPARK_SUBMIT_ARG_KWARGS = dict(
    supervise=dict(action='store_true'),
)

# not a runner opt or one that's passed straight through to spark
_STEP_OPT_NAMES = {'main_class'}

# arguments that are passed straight through to spark-submit
_SPARK_ARG_OPT_NAMES = (
    set(_SPARK_SUBMIT_SWITCHES) - set(_RUNNER_OPTS) - _STEP_OPT_NAMES)

_SWITCH_ALIASES = {
    '--master': '--spark-master',
    '--deploy-mode': '--spark-deploy-mode',
    '--jars': '--libjars',
    '--conf': '--jobconf',
}

# these options don't make any sense with Spark scripts
_HARD_CODED_OPTS = dict(
    check_input_paths=False,
    output_dir=None,
)


def main(cl_args=None):
    parser = _make_arg_parser()
    options = parser.parse_args(cl_args)

    runner_alias = options.runner or _DEFAULT_RUNNER
    runner_class = _runner_class(runner_alias)

    if options.help or not options.script_or_jar:
        _print_help(options, runner_class)
        sys.exit(0)

    MRJob.set_up_logging(
        quiet=options.quiet,
        verbose=options.verbose,
    )

    kwargs = _get_runner_opt_kwargs(options, runner_class)
    kwargs.update(_HARD_CODED_OPTS)

    kwargs['input_paths'] = [os.devnull]

    step = _get_step(options, parser, cl_args)
    kwargs['steps'] = [step.description()]

    runner = runner_class(**kwargs)

    try:
        runner.run()
    finally:
        runner.cleanup()


def _get_runner_opt_kwargs(options, runner_class):
    """Extract the options for the given runner class from *options*."""
    return {opt_name: getattr(options, opt_name)
            for opt_name in runner_class.OPT_NAMES
            if hasattr(options, opt_name)}


def _get_step(options, parser, cl_args):
    """Extract the step from the runner options."""
    args = options.args
    main_class = options.main_class
    spark_args = _get_spark_args(parser, cl_args)
    script_or_jar = options.script_or_jar

    if script_or_jar.lower().endswith('.jar'):
        return SparkJarStep(args=args,
                            jar=script_or_jar,
                            main_class=main_class,
                            spark_args=spark_args)
    elif script_or_jar.lower().split('.')[-1].startswith('py'):
        return SparkScriptStep(args=args,
                               script=script_or_jar,
                               spark_args=spark_args)
    else:
        raise ValueError('%s appears not to be a JAR or Python script' %
                         options.script_or_jar)


def _get_spark_args(parser, cl_args):
    raw_args = _parse_raw_args(parser, cl_args)

    spark_args = []

    for dest, option_string, args in raw_args:
        if dest in _SPARK_ARG_OPT_NAMES:
            spark_args.append(option_string)
            spark_args.extend(args)

    return spark_args


def _add_spark_submit_arg(parser, opt_name):
    opt_string = _SPARK_SUBMIT_SWITCHES[opt_name]

    kwargs = dict(dest=opt_name)

    # if opt_name is a mrjob opt, parse args like a MRJob would
    if opt_name in _RUNNER_OPTS:
        opt_alias = _SWITCH_ALIASES.get(opt_string, opt_string)

        for opt_strings, opt_kwargs in _RUNNER_OPTS[opt_name]['switches']:
            if opt_alias in opt_strings:
                kwargs.update(opt_kwargs)

    kwargs['help'] = _SPARK_SUBMIT_ARG_HELP[opt_name]
    kwargs.update(_SPARK_SUBMIT_ARG_KWARGS.get(opt_name) or {})

    parser.add_argument(opt_string, **kwargs)


def _make_arg_parser():
    # this parser is never used for help messages, but
    # will show usage on error
    parser = ArgumentParser(usage=_USAGE, add_help=False)

    # add positional arguments
    parser.add_argument(dest='script_or_jar', nargs='?')
    parser.add_argument(dest='args', nargs=REMAINDER)

    _add_basic_args(parser)
    _add_runner_alias_arg(parser)
    _add_help_arg(parser)
    _add_deprecated_arg(parser)

    # add runner opts
    runner_opt_names = set(_RUNNER_OPTS) - set(_HARD_CODED_OPTS)
    _add_runner_args(parser, opt_names=runner_opt_names)

    # add spark-specific opts (without colliding with runner opts)
    for opt_name, switch in _SPARK_SUBMIT_SWITCHES.items():
        if opt_name in _RUNNER_OPTS and switch not in _SWITCH_ALIASES:
            continue
        _add_spark_submit_arg(parser, opt_name)

    return parser


def _add_runner_alias_arg(parser):
    # we can't set default here because -r also affects help
    parser.add_argument(
        '-r', '--runner', dest='runner',
        choices=_SPARK_RUNNERS,
        help=('Where to run the job (default: "%s")'
              % _DEFAULT_RUNNER))


def _add_help_arg(parser):
    parser.add_argument(
        '-h', '--help', dest='help', action='store_true',
        help='show this message and exit')


def _add_deprecated_arg(parser):
    parser.add_argument(
        '--deprecated', dest='deprecated', action='store_true',
        help='include help for deprecated options')


def _print_help(options, runner_class):
    if options.help and options.runner:
        # if user specifies -r without -h, show basic help
        _print_help_for_runner(runner_class,
                               include_deprecated=options.deprecated)
    else:
        _print_basic_help(include_deprecated=options.deprecated)


def _print_help_for_runner(runner_class, include_deprecated=False):
    help_parser = ArgumentParser(usage=SUPPRESS, add_help=False)

    arg_group = help_parser.add_argument_group(
        'optional arguments for %s runner' % runner_class.alias)

    # don't include hard-coded opts or opts in basic help
    opt_names = runner_class.OPT_NAMES - set(_HARD_CODED_OPTS)

    # don't include switches already in basic help
    suppress_switches = set(_SPARK_SUBMIT_SWITCHES.values())

    # simplify description of aliases of switches in basic help
    customize_switches = {
        v: dict(help='Alias for %s' % k)
        for k, v in _SWITCH_ALIASES.items()
    }

    _add_runner_args(arg_group, opt_names,
                     include_deprecated=include_deprecated,
                     customize_switches=customize_switches,
                     suppress_switches=suppress_switches)

    help_parser.print_help()


def _print_basic_help(include_deprecated=False):
    _make_basic_help_parser(include_deprecated).print_help()

    if not include_deprecated:
        print()
        print(_DEPRECATED_OPT_HELP)


def _make_basic_help_parser(include_deprecated=False):
    """Make an arg parser that's used only for printing basic help.

    This prints help very similar to spark-submit itself. Runner args
    are not included unless they are also spark-submit args (e.g. --py-files)
    """
    help_parser = ArgumentParser(usage=_USAGE, description=_DESCRIPTION,
                                 epilog=_BASIC_HELP_EPILOG, add_help=False)

    _add_runner_alias_arg(help_parser)

    for group_desc, opt_names in _SPARK_SUBMIT_ARG_GROUPS:
        if group_desc is None:
            parser_or_group = help_parser
        else:
            parser_or_group = help_parser.add_argument_group(group_desc)

        for opt_name in opt_names:
            _add_spark_submit_arg(parser_or_group, opt_name)

        if group_desc is None:
            _add_basic_args(help_parser)
            _add_help_arg(help_parser)
            if include_deprecated:
                _add_deprecated_arg(help_parser)

    return help_parser


if __name__ == '__main__':
    main()
