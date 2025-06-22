# Copyright 2009-2012 Yelp and Contributors
# Copyright 2013 David Marin
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
import logging
import os.path
import re
from io import BytesIO
from subprocess import Popen
from subprocess import PIPE
from subprocess import CalledProcessError

from mrjob.cat import decompress
from mrjob.compat import uses_yarn
from mrjob.fs.base import Filesystem
from mrjob.py2 import to_unicode
from mrjob.parse import is_uri
from mrjob.parse import urlparse
from mrjob.util import cmd_line
from mrjob.util import unique
from mrjob.util import which


log = logging.getLogger(__name__)

# used by mkdir()
_HADOOP_FILE_EXISTS_RE = re.compile(br'.*File exists.*')

# used by ls() and exists()
_HADOOP_LS_NO_SUCH_FILE = re.compile(br'^lsr?: .*No such file.*$')

# used by rm() (see below)
_HADOOP_RM_NO_SUCH_FILE = re.compile(br'^rmr?: .*No such file.*$')

# find version string in "Hadoop 0.20.203" etc.
_HADOOP_VERSION_RE = re.compile(br'^.*?(?P<version>(\d|\.)+).*?$')


class HadoopFilesystem(Filesystem):
    """Filesystem for URIs accepted by ``hadoop fs``. Typically you will get
    one of these via ``HadoopJobRunner().fs``, composed with
    :py:class:`~mrjob.fs.local.LocalFilesystem`.

    This also helps with other invocations of the ``hadoop`` binary, such
    as ``hadoop version`` (see :py:meth:`invoke_hadoop`).
    """

    def __init__(self, hadoop_bin=None):
        """Create a Hadoop filesystem

        :param hadoop_bin: ``hadoop`` binary, as a list of args. If set to
                           ``None``, we'll auto-detect the Hadoop binary.
                           If set to ``[]``, this FS will be disabled
                           until you call :py:meth:`set_hadoop_bin`.
        """
        super(HadoopFilesystem, self).__init__()
        self._hadoop_bin = hadoop_bin
        self._hadoop_version = None  # cache for get_hadoop_version()

    def can_handle_path(self, path):
        if not (self._hadoop_bin or self._hadoop_bin is None):
            return False

        return is_uri(path)

    def get_hadoop_bin(self):
        """Return the hadoop binary, searching for it if need be."""
        if self._hadoop_bin is None:
            self._hadoop_bin = self._find_hadoop_bin()
        return self._hadoop_bin

    def set_hadoop_bin(self, hadoop_bin):
        """Manually set the hadoop binary, as a list of args."""
        self._hadoop_bin = hadoop_bin

    def _find_hadoop_bin(self):
        """Look for the hadoop binary in any plausible place. If all
        else fails, return ``['hadoop']``.
        """
        def yield_paths():
            for name in 'HADOOP_PREFIX', 'HADOOP_HOME', 'HADOOP_INSTALL':
                path = os.environ.get(name)
                if path:
                    yield os.path.join(path, 'bin')

            # They use $HADOOP_INSTALL/hadoop/bin here:
            # https://wiki.apache.org/hadoop/GettingStartedWithHadoop
            if os.environ.get('HADOOP_INSTALL'):
                yield os.path.join(
                    os.environ['HADOOP_INSTALL'], 'hadoop', 'bin')

            yield None  # use $PATH

            # Maybe it's in $HADOOP_MAPRED_HOME? $HADOOP_YARN_HOME? Don't give
            # up. Don't worry about duplicates; they're de-duplicated below
            for name, path in sorted(os.environ.items()):
                if name.startswith('HADOOP_') and name.endswith('_HOME'):
                    yield os.path.join(path, 'bin')

        for path in unique(yield_paths()):
            log.info('Looking for hadoop binary in %s...' % (path or '$PATH'))

            hadoop_bin = which('hadoop', path=path)

            if hadoop_bin:
                log.info('Found hadoop binary: %s' % hadoop_bin)
                return [hadoop_bin]
        else:
            log.info("Falling back to 'hadoop'")
            return ['hadoop']

    def get_hadoop_version(self):
        """Invoke the hadoop executable to determine its version"""
        # mkdir() needs this
        if not self._hadoop_version:
            stdout = self.invoke_hadoop(['version'], return_stdout=True)
            if stdout:
                first_line = stdout.split(b'\n')[0]
                m = _HADOOP_VERSION_RE.match(first_line)
                if m:
                    self._hadoop_version = to_unicode(m.group('version'))
                    log.info("Using Hadoop version %s" % self._hadoop_version)
                else:
                    raise Exception('Unable to determine Hadoop version.')

        return self._hadoop_version

    def invoke_hadoop(self, args, ok_returncodes=None, ok_stderr=None,
                      return_stdout=False):
        """Run the given hadoop command, raising an exception on non-zero
        return code. This only works for commands whose output we don't
        care about.

        Args:
        ok_returncodes -- a list/tuple/set of return codes we expect to
            get back from hadoop (e.g. [0,1]). By default, we only expect 0.
            If we get an unexpected return code, we raise a CalledProcessError.
        ok_stderr -- don't log STDERR or raise CalledProcessError if stderr
            matches a regex in this list (even if the returncode is bad)
        return_stdout -- return the stdout from the hadoop command rather
            than logging it. If this is False, we return the returncode
            instead.
        """
        args = self.get_hadoop_bin() + args

        log.debug('> %s' % cmd_line(args))

        proc = Popen(args, stdout=PIPE, stderr=PIPE)
        stdout, stderr = proc.communicate()

        log_func = log.debug if proc.returncode == 0 else log.error
        if not return_stdout:
            for line in BytesIO(stdout):
                log_func('STDOUT: ' + to_unicode(line.rstrip(b'\r\n')))

        # check if STDERR is okay
        stderr_is_ok = False
        if ok_stderr:
            for stderr_re in ok_stderr:
                if stderr_re.match(stderr):
                    stderr_is_ok = True
                    break

        if not stderr_is_ok:
            for line in BytesIO(stderr):
                log_func('STDERR: ' + to_unicode(line.rstrip(b'\r\n')))

        ok_returncodes = ok_returncodes or [0]

        if not stderr_is_ok and proc.returncode not in ok_returncodes:
            raise CalledProcessError(proc.returncode, args)

        if return_stdout:
            return stdout
        else:
            return proc.returncode

    def du(self, path_glob):
        """Get the size of a file or directory (recursively), or 0
        if it doesn't exist."""
        try:
            stdout = self.invoke_hadoop(['fs', '-du', path_glob],
                                        return_stdout=True,
                                        ok_returncodes=[0, 1, 255])
        except CalledProcessError:
            return 0

        try:
            return sum(int(line.split()[0])
                       for line in stdout.split(b'\n')
                       if line.strip())
        except (ValueError, TypeError, IndexError):
            raise IOError(
                'Unexpected output from hadoop fs -du: %r' % stdout)

    def ls(self, path_glob):
        components = urlparse(path_glob)
        hdfs_prefix = '%s://%s' % (components.scheme, components.netloc)

        version = self.get_hadoop_version()

        # use ls -R on Hadoop 2 (see #1152)
        if uses_yarn(version):
            args = ['fs', '-ls', '-R', path_glob]
        else:
            args = ['fs', '-lsr', path_glob]

        try:
            stdout = self.invoke_hadoop(args, return_stdout=True,
                                        ok_stderr=[_HADOOP_LS_NO_SUCH_FILE])
        except CalledProcessError:
            raise IOError("Could not ls %s" % path_glob)

        for line in BytesIO(stdout):
            line = line.rstrip(b'\r\n')

            # ignore total item count
            if line.startswith(b'Found '):
                continue

            fields = line.split(b' ')

            # Throw out directories
            if fields[0].startswith(b'd'):
                continue

            # Try to figure out which part of the line is the path
            # Expected lines:
            #
            # HDFS:
            # -rw-r--r--   3 dave users       3276 2010-01-13 14:00 /foo/bar
            #
            # S3:
            # -rwxrwxrwx   1          3276 010-01-13 14:00 /foo/bar
            path_index = None
            for index, field in enumerate(fields):
                # look for time field, and pick one after that
                # (can't use field[2] because that's an int in Python 3)
                if len(field) == 5 and field[2:3] == b':':
                    path_index = (index + 1)
            if not path_index:
                raise IOError("Could not locate path in string %r" % line)

            path = to_unicode(line.split(b' ', path_index)[-1])
            # handle fully qualified URIs from newer versions of Hadoop ls
            # (see Pull Request #577)
            if is_uri(path):
                yield path
            else:
                yield hdfs_prefix + path

    def _cat_file(self, path):
        # stream from HDFS
        cat_args = self.get_hadoop_bin() + ['fs', '-cat', path]
        log.debug('> %s' % cmd_line(cat_args))

        cat_proc = Popen(cat_args, stdout=PIPE, stderr=PIPE)

        for chunk in decompress(cat_proc.stdout, path):
            yield chunk

        # this does someties happen; see #1396
        for line in cat_proc.stderr:
            log.error('STDERR: ' + to_unicode(line.rstrip(b'\r\n')))

        cat_proc.stdout.close()
        cat_proc.stderr.close()

        returncode = cat_proc.wait()

        if returncode != 0:
            raise IOError("Could not stream %s" % path)

    def mkdir(self, path):
        version = self.get_hadoop_version()

        # use -p on Hadoop 2 (see #991, #845)
        if uses_yarn(version):
            args = ['fs', '-mkdir', '-p', path]
        else:
            args = ['fs', '-mkdir', path]

        try:
            self.invoke_hadoop(args, ok_stderr=[_HADOOP_FILE_EXISTS_RE])
        except CalledProcessError:
            raise IOError("Could not mkdir %s" % path)

    def exists(self, path_glob):
        """Does the given path exist?

        If dest is a directory (ends with a "/"), we check if there are
        any files starting with that path.
        """
        try:
            return_code = self.invoke_hadoop(
                ['fs', '-ls', path_glob],
                ok_returncodes=[0, -1, 255],
                ok_stderr=[_HADOOP_LS_NO_SUCH_FILE])

            return (return_code == 0)
        except CalledProcessError:
            raise IOError("Could not check path %s" % path_glob)

    def put(self, src, path):
        # don't inadvertently support cp syntax
        if path.endswith('/'):
            raise ValueError('put() destination may not be a directory')

        self.invoke_hadoop(['fs', '-put', src, path])

    def rm(self, path_glob):
        if not is_uri(path_glob):
            super(HadoopFilesystem, self).rm(path_glob)

        version = self.get_hadoop_version()
        if uses_yarn(version):
            args = ['fs', '-rm', '-R', '-f', '-skipTrash', path_glob]
        else:
            args = ['fs', '-rmr', '-skipTrash', path_glob]

        try:
            self.invoke_hadoop(
                args,
                return_stdout=True, ok_stderr=[_HADOOP_RM_NO_SUCH_FILE])
        except CalledProcessError:
            raise IOError("Could not rm %s" % path_glob)

    def touchz(self, path):
        try:
            self.invoke_hadoop(['fs', '-touchz', path])
        except CalledProcessError:
            raise IOError("Could not touchz %s" % path)
