# Copyright 2009-2016 Yelp and Contributors
# Copyright 2017-2018 Yelp
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
"""Utility functions for MRJob
"""
# don't add imports here that aren't part of the standard Python library,
# since MRJobs need to run in Amazon's generic EMR environment
import logging
import os
import os.path
import pipes
import random
import shlex
import shutil
import sys
import tarfile
from contextlib import contextmanager
from datetime import timedelta
from distutils.spawn import find_executable
from logging import getLogger
from zipfile import ZIP_DEFLATED
from zipfile import ZIP_STORED
from zipfile import ZipFile
from zipfile import is_zipfile

from mrjob.py2 import PY2


log = getLogger(__name__)


class NullHandler(logging.Handler):
    def emit(self, record):
        pass


def cmd_line(args):
    """build a command line that works in a shell.
    """
    args = [str(x) for x in args]
    return ' '.join(pipes.quote(x) for x in args)


def expand_path(path):
    """Resolve ``~`` (home dir) and environment variables in *path*.

    If *path* is ``None``, return ``None``.
    """
    if path is None:
        return None
    else:
        return os.path.expanduser(os.path.expandvars(path))


def file_ext(filename):
    """return the file extension, including the ``.``

    >>> file_ext('foo.tar.gz')
    '.tar.gz'

    >>> file_ext('.emacs')
    ''

    >>> file_ext('.mrjob.conf')
    '.conf'
    """
    stripped_name = filename.lstrip('.')
    dot_index = stripped_name.find('.')

    if dot_index == -1:
        return ''
    return stripped_name[dot_index:]


def log_to_null(name=None):
    """Set up a null handler for the given stream, to suppress
    "no handlers could be found" warnings."""
    logger = logging.getLogger(name)
    logger.addHandler(NullHandler())


def log_to_stream(name=None, stream=None, format=None, level=None,
                  debug=False):
    """Set up logging.

    :type name: str
    :param name: name of the logger, or ``None`` for the root logger
    :type stream: file object
    :param stream:  stream to log to (default is ``sys.stderr``)
    :type format: str
    :param format: log message format (default is '%(message)s')
    :param level: log level to use
    :type debug: bool
    :param debug: quick way of setting the log level: if true, use
                  ``logging.DEBUG``, otherwise use ``logging.INFO``
    """
    if level is None:
        level = logging.DEBUG if debug else logging.INFO

    if format is None:
        format = '%(message)s'

    if stream is None:
        stream = sys.stderr

    handler = logging.StreamHandler(stream)
    handler.setLevel(level)
    handler.setFormatter(logging.Formatter(format))

    logger = logging.getLogger(name)
    logger.setLevel(level)
    logger.addHandler(handler)


def random_identifier():
    """A random 16-digit hex string."""
    return '%016x' % random.randint(0, 2 ** 64 - 1)


# Thanks to http://lybniz2.sourceforge.net/safeeval.html for
# explaining how to do this!
def safeeval(expr, globals=None, locals=None):
    """Like eval, but with nearly everything in the environment
    blanked out, so that it's difficult to cause mischief.

    *globals* and *locals* are optional dictionaries mapping names to
    values for those names (just like in :py:func:`eval`).
    """
    # blank out builtins, but keep None, True, and False
    safe_globals = {
        'False': False,
        'None': None,
        'True': True,
        '__builtin__': None,
        '__builtins__': None,
        'set': set
    }

    # xrange is range in Python 3
    if PY2:
        safe_globals['xrange'] = xrange
    else:
        safe_globals['range'] = range

    # PyPy needs special magic
    def open(*args, **kwargs):
        raise NameError("name 'open' is not defined")
    safe_globals['open'] = open

    # add the user-specified global variables
    if globals:
        safe_globals.update(globals)

    return eval(expr, safe_globals, locals)


@contextmanager
def save_current_environment():
    """ Context manager that saves os.environ and loads
        it back again after execution
    """
    original_environ = os.environ.copy()

    try:
        yield

    finally:
        os.environ.clear()
        os.environ.update(original_environ)


@contextmanager
def save_cwd():
    """Context manager that saves the current working directory,
    and chdir's back to it after execution."""
    original_cwd = os.getcwd()

    try:
        yield

    finally:
        os.chdir(original_cwd)


@contextmanager
def save_sys_std():
    """Context manager that saves the current values of `sys.stdin`,
    `sys.stdout`, and `sys.stderr`, and flushes these filehandles before
    and after switching them out."""

    stdin, stdout, stderr = sys.stdin, sys.stdout, sys.stderr

    try:
        sys.stdout.flush()
        sys.stderr.flush()

        yield

        # at this point, sys.stdout/stderr may have been patched. Don't
        # raise an exception if flush() fails
        try:
            sys.stdout.flush()
        except:
            pass

        try:
            sys.stderr.flush()
        except:
            pass
    finally:
        sys.stdin, sys.stdout, sys.stderr = stdin, stdout, stderr


@contextmanager
def save_sys_path():
    """Context manager that saves sys.path and restores it after execution."""
    original_sys_path = list(sys.path)

    try:
        yield

    finally:
        sys.path = original_sys_path


def shlex_split(s):
    """Wrapper around shlex.split(), but convert to str if Python version <
    2.7.3 when unicode support was added.
    """
    if sys.version_info < (2, 7, 3):
        return shlex.split(str(s))
    else:
        return shlex.split(s)


def strip_microseconds(delta):
    """Return the given :py:class:`datetime.timedelta`, without microseconds.

    Useful for printing :py:class:`datetime.timedelta` objects.
    """
    return timedelta(delta.days, delta.seconds)


def to_lines(chunks):
    """Take in data as a sequence of bytes, and yield it, one line at a time.

    Only breaks lines on ``\\n`` (not ``\\r``), and does not add
    a trailing newline.

    For efficiency, passes through anything with a ``readline()`` attribute.
    """
    # hopefully this is good enough for anything mrjob will encounter
    if hasattr(chunks, 'readline'):
        return chunks
    else:
        return _to_lines(chunks)


def _to_lines(chunks):
    """Take in data as a sequence of bytes, and yield it, one line at a time.

    Only breaks lines on ``\\n`` (not ``\\r``), and does not add
    a trailing newline.

    Exception: if we encounter an empty bytestring ``b''``, immediately yield
    what we have so far rather than joining it to the next chunk. This allows
    us to handle bytes from multiple files without joining the end of one
    file to the beginning of the next one.

    Optimizes for:

    * chunks bigger than lines (e.g. reading test files)
    * chunks that are lines (idempotency)
    """
    # list of chunks with no final newline
    leftovers = []

    for chunk in chunks:
        # special case for b'' standing for EOF
        if chunk == b'':
            if leftovers:
                yield b''.join(leftovers)
                leftovers = []

            continue

        start = 0

        while start < len(chunk):
            end = chunk.find(b'\n', start) + 1

            if end == 0:  # no newlines found
                leftovers.append(chunk[start:])
                break

            if leftovers:
                leftovers.append(chunk[start:end])
                yield b''.join(leftovers)
                leftovers = []
            else:
                yield chunk[start:end]

            start = end

    if leftovers:
        yield b''.join(leftovers)


def unique(items):
    """Yield items from *item* in order, skipping duplicates."""
    seen = set()

    for item in items:
        if item in seen:
            continue
        else:
            yield item
            seen.add(item)


def unarchive(archive_path, dest):
    """Extract the contents of a tar or zip file at *archive_path* into the
    directory *dest*.

    :type archive_path: str
    :param archive_path: path to archive file
    :type dest: str
    :param dest: path to directory where archive will be extracted

    *dest* will be created if it doesn't already exist.

    tar files can be gzip compressed, bzip2 compressed, or uncompressed. Files
    within zip files can be deflated or stored.
    """
    if tarfile.is_tarfile(archive_path):
        with tarfile.open(archive_path, 'r') as archive:
            archive.extractall(dest)
    elif is_zipfile(archive_path):
        with ZipFile(archive_path, 'r') as archive:
            for name in archive.namelist():
                # the zip spec specifies that front slashes are always
                # used as directory separators
                dest_path = os.path.join(dest, *name.split('/'))

                # now, split out any dirname and filename and create
                # one and/or the other
                dirname, filename = os.path.split(dest_path)
                if dirname and not os.path.exists(dirname):
                    os.makedirs(dirname)
                if filename:
                    with open(dest_path, 'wb') as dest_file:
                        dest_file.write(archive.read(name))
    else:
        raise IOError('Unknown archive type: %s' % (archive_path,))


def which(cmd, path=None):
    """Like the UNIX which command: search in *path* for the executable named
    *cmd*. *path* defaults to :envvar:`PATH`. Returns ``None`` if no
    such executable found.

    This is basically ``shutil.which()`` (which was introduced in Python 3.3)
    without the *mode* argument. Best practice is to always specify *path*
    as a keyword argument.
    """
    if hasattr(shutil, 'which'):
        # added in Python 3.3
        return shutil.which(cmd, path=path)
    elif path is None and os.environ.get('PATH') is None:
        # find_executable() errors if neither path nor $PATH is set
        return None
    else:
        return find_executable(cmd, path=path)


def zip_dir(dir, out_path, filter=None, prefix=''):
    """Compress the given *dir* into a zip file at *out_path*.

    If we encounter symlinks, include the actual file, not the symlink.

    :type dir: str
    :param dir: dir to tar up
    :type out_path: str
    :param out_path: where to write the tarball too
    :param filter: if defined, a function that takes paths (relative to *dir*
                   and returns ``True`` if we should keep them
    :type prefix: str
    :param prefix: subdirectory inside the tarball to put everything into (e.g.
                   ``'mrjob'``)
    """
    if not os.path.isdir(dir):
        raise IOError('Not a directory: %r' % (dir,))

    if not filter:
        filter = lambda path: True

    with _create_zip_file(out_path) as zip_file:
        for dirpath, dirnames, filenames in os.walk(dir, followlinks=True):
            for filename in filenames:
                path = os.path.join(dirpath, filename)
                rel_path = os.path.relpath(path, dir)

                if filter(rel_path):
                    # copy over real files, not symlinks
                    real_path = os.path.realpath(path)
                    path_in_zip_file = os.path.join(prefix, rel_path)
                    zip_file.write(real_path, arcname=path_in_zip_file)


# this is also used by spark runner
def _create_zip_file(path):
    try:
        return ZipFile(path, mode='w', compression=ZIP_DEFLATED)
    except RuntimeError:  # zlib not available
        return ZipFile(path, mode='w', compression=ZIP_STORED)
