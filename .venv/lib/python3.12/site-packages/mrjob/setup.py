# Copyright 2012 Yelp and Contributors
# Copyright 2013 David Marin and Lyft
# Copyright 2014-2016 Yelp and Contributors
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
"""Utilities for setting up the environment jobs run in by uploading files
and running setup scripts.

The general idea is to use Hadoop DistributedCache-like syntax to find and
parse expressions like ``/path/to/file#name_in_working_dir`` into "path
dictionaries" like
``{'type': 'file', 'path': '/path/to/file', 'name': 'name_in_working_dir'}}``.

You can then pass these into a :py:class:`WorkingDirManager` to keep
track of which files need to be uploaded, catch name collisions, and assign
names to unnamed paths (e.g. ``/path/to/file#``). Note that
:py:meth:`WorkingDirManager.name` can take a path dictionary as keyword
arguments.

If you need to upload files from the local filesystem to a place where
Hadoop can see them (HDFS or S3), we provide :py:class:`UploadDirManager`.

Path dictionaries are meant to be immutable; all state is handled by
manager classes.
"""
import itertools
import logging
import os
import os.path
import posixpath
import re

from mrjob.parse import is_uri
from mrjob.py2 import string_types
from mrjob.util import expand_path
from mrjob.util import file_ext


log = logging.getLogger(__name__)


_SUPPORTED_TYPES = ('archive', 'dir', 'file')


_SETUP_CMD_RE = re.compile(
    r"((?P<single_quoted>'[^']*')|"
    r'(?P<double_quoted>"([^"\\]|\\.)*")|'
    r'(?P<hash_path>'
        r'(?P<path>([A-Za-z][A-Za-z0-9\.-]*://([^\'"\s\\]|\\.)+)|'  # noqa
            r'([^\'":=\s\\]|\\.)*([^\'":=\s\\/]|\\.))'  # noqa
            r'(?P<path_slash>/)?'
        r'#(?P<name>([^\'":;><|=/#\s\\]|\\.)*)'
            r'(?P<name_slash>/)?)|'
    r'(?P<unquoted>([^\'":=\s\\]|\\.)+)|'
    r'(?P<colon_or_equals>[:=])|'  # hash path could come next; stop capturing
    r'(?P<whitespace>\s+)|'
    r'(?P<error>.+))')


_ESCAPE_RE = re.compile(
    r'(\\(?P<escaped>.)|(?P<unescaped>[^\\]+)|(?P<error>.+))')


def parse_setup_cmd(cmd):
    """Parse a setup/bootstrap command, finding and pulling out Hadoop
    Distributed Cache-style paths ("hash paths").

    :param string cmd: shell command to parse
    :return: a list containing dictionaries (parsed hash paths) and strings
             (parts of the original command, left unparsed)

    Hash paths look like ``path#name``, where *path* is either a local path
    or a URI pointing to something we want to upload to Hadoop/EMR, and *name*
    is the name we want it to have when we upload it; *name* is optional
    (no name means to pick a unique one).

    If *name* is followed by a trailing slash, that indicates *path* is an
    archive (e.g. a tarball), and should be unarchived into a directory on the
    remote system. The trailing slash will *also* be kept as part of the
    original command.

    If *path* is followed by a trailing slash, that indicates *path* is a
    directory and should be tarballed and later unarchived into a directory
    on the remote system. The trailing slash will also be kept as part of
    the original command. You may optionally include a slash after *name* as
    well (this will only result in a single slash in the final command).

    Parsed hash paths are dicitionaries with the keys ``path``, ``name``, and
    ``type`` (either ``'file'``, ``'archive'``, or ``'dir'``).

    Most of the time, this function will just do what you expect. Rules for
    finding hash paths:

    * we only look for hash paths outside of quoted strings
    * *path* may not contain quotes or whitespace
    * *path* may not contain `:` or `=` unless it is a URI (starts with
      ``<scheme>://``); this allows you to do stuff like
      ``export PYTHONPATH=$PYTHONPATH:foo.egg#``.
    * *name* may not contain whitespace or any of the following characters:
      ``'":;><|=/#``, so you can do stuff like
      ``sudo dpkg -i fooify.deb#; fooify bar``

    If you really want to include forbidden characters, you may use backslash
    escape sequences in *path* and *name*. (We can't guarantee Hadoop/EMR
    will accept them though!). Also, remember that shell syntax allows you
    to concatenate strings ``like""this``.

    Environment variables and ``~`` (home dir) in *path* will be resolved
    (use backslash escapes to stop this). We don't resolve *name* because it
    doesn't make sense. Environment variables and ``~`` elsewhere in the
    command are considered to be part of the script and will be resolved
    on the remote system.
    """
    tokens = []

    for m in _SETUP_CMD_RE.finditer(cmd):
        keep_as_is = (m.group('single_quoted') or
                      m.group('double_quoted') or
                      m.group('unquoted') or
                      m.group('whitespace') or
                      m.group('colon_or_equals'))

        if keep_as_is:
            if tokens and isinstance(tokens[-1], string_types):
                tokens[-1] += keep_as_is
            else:
                tokens.append(keep_as_is)
        elif m.group('hash_path'):
            if m.group('path_slash'):
                token_type = 'dir'
            elif m.group('name_slash'):
                token_type = 'archive'
            else:
                token_type = 'file'

            tokens.append({
                'path': _resolve_path(m.group('path')),
                'name': m.group('name') or None,
                'type': token_type
            })

            if m.group('path_slash') or m.group('name_slash'):
                tokens.append('/')
        elif m.group('error'):
            # these match the error messages from shlex.split()
            if m.group('error').startswith('\\'):
                raise ValueError('No escaped character')
            else:
                raise ValueError('No closing quotation')

    return tokens


def _resolve_path(path):
    """Helper for :py:func:`parse_setup_cmd`.

    Resolve ``~`` (home dir) and environment variables in the
    given path, and unescape backslashes."""
    result = ''

    for m in _ESCAPE_RE.finditer(path):
        if m.group('escaped'):
            result += m.group('escaped')
        elif m.group('unescaped'):
            result += expand_path(m.group('unescaped'))
        else:
            raise ValueError('No escaped character')

    return result


def parse_legacy_hash_path(type, path, must_name=None):
    """Parse hash paths from old setup/bootstrap options.

    This is similar to parsing hash paths out of shell commands (see
    :py:func:`parse_setup_cmd`) except that we pass in
    path type explicitly, and we don't always require the ``#`` character.

    :param type: Type of the path (``'archive'`` or ``'file'``)
    :param path: Path to parse, possibly with a ``#``
    :param must_name: If set, use *path*'s filename as its name if there
                      is no ``'#'`` in *path*, and raise an exception
                      if there is just a ``'#'`` with no name. Set *must_name*
                      to the name of the relevant option so we can print
                      a useful error message. This is intended for options
                      like ``upload_files`` that merely upload a file
                      without tracking it.
    """
    if type not in _SUPPORTED_TYPES:
        raise ValueError('bad path type %r, must be one of %s' % (
            type, ', '.join(sorted(_SUPPORTED_TYPES))))

    if '#' in path:
        path, name = path.split('#', 1)

        # allow a slash after the name of an archive or dir because that's
        # the new-way of specifying archive paths
        if type in ('archive', 'dir'):
            name = name.rstrip('/' + os.sep)

        if '/' in name or '#' in name:
            raise ValueError(
                'Bad path %r; name must not contain # or /' % (path,))
    else:
        if must_name:
            if type == 'dir':
                # handle trailing slash on directory names
                name = os.path.basename(path.rstrip('/' + os.sep))
            else:
                name = os.path.basename(path)
        else:
            name = None

    if not path:
        raise ValueError('Path may not be empty!')

    if not name:
        if must_name:
            raise ValueError(
                'Empty name makes no sense for %s: %r' % (must_name, path))
        else:
            name = None

    return {'path': path, 'name': name, 'type': type}


def name_uniquely(path, names_taken=(), proposed_name=None, unhide=False,
                  strip_ext=False, suffix=''):
    """Come up with a unique name for *path*.

    :param names_taken: a dictionary or set of names not to use.
    :param proposed_name: name to use if it is not taken. If this is not set,
                          we propose a name based on the filename.
    :param unhide: make sure final name doesn't start with periods or
                   underscores
    :param strip_ext: if we propose a name, it shouldn't have a file extension
    :param suffix: if set to a string, add this to the end of any filename
                    we propose. Should include the ``.``.

    If the proposed name is taken, we add a number to the end of the
    filename, keeping the extension the same. For example:

    >>> name_uniquely('foo.txt', {'foo.txt'})
    'foo-1.txt'
    >>> name_uniquely('bar.tar.gz', {'bar'}, strip_ext=True)
    'bar-1'
    """
    filename = proposed_name or os.path.basename(path.rstrip('/' + os.sep))
    ext = file_ext(filename)
    prefix = filename[:-len(ext) or None]

    if strip_ext and not proposed_name:
        ext = ''

    if suffix and not proposed_name:
        ext += suffix

    if unhide:
        prefix = prefix.lstrip('.').lstrip('_')

    # is our proposed name taken?
    name = prefix + ext
    if prefix and name not in names_taken:
        return name

    # add 1, 2, etc. to the name until it's not taken
    for i in itertools.count(1):
        if prefix:
            name = '%s-%d%s' % (prefix, i, ext)
        else:
            # if no prefix is left (due to empty filename or unhiding)
            # just use numbers; don't start filenames with '-'
            name = '%d%s' % (i, ext)
        if name not in names_taken:
            return name


class UploadDirManager(object):
    """Represents a directory on HDFS or S3 where we want to upload
    local files for consumption by Hadoop.

    :py:class:`UploadDirManager` tries to give files the same name as their
    filename in the path (for ease of debugging), but handles collisions
    gracefully.

    :py:class:`UploadDirManager` assumes URIs to not need to be uploaded
    and thus does not store them. :py:meth:`uri` maps URIs to themselves.
    """
    def __init__(self, prefix):
        """Make an :py:class`UploadDirManager`.

        :param string prefix: The URI for the directory (e.g.
                              `s3://bucket/dir/`). It doesn't matter if
                              *prefix* has a trailing slash; :py:meth:`uri`
                              will do the right thing.
        """
        self.prefix = prefix

        self._path_to_name = {}
        self._names_taken = set()

    def add(self, path):
        """Add a path. If *path* hasn't been added before, assign it a name.
                       If *path* is a URI don't add it; just return the URI.

        :return: the URI assigned to the path"""
        if is_uri(path):
            return path

        if path not in self._path_to_name:
            # use unhide so that input files won't be hidden from Hadoop,
            # see #1200
            name = name_uniquely(
                path, names_taken=self._names_taken, unhide=True)
            self._names_taken.add(name)
            self._path_to_name[path] = name

        return self.uri(path)

    def uri(self, path):
        """Get the URI for the given path. If *path* is a URI, just return it.
        """
        if is_uri(path):
            return path

        if path in self._path_to_name:
            return posixpath.join(self.prefix, self._path_to_name[path])
        else:
            raise ValueError('%r is not a URI or a known local file' % (path,))

    def path_to_uri(self):
        """Get a map from path to URI for all paths that were added,
        so we can figure out which files we need to upload."""
        return dict((path, self.uri(path))
                    for path in self._path_to_name)


class WorkingDirManager(object):
    """Represents the working directory of hadoop/Spark tasks (or bootstrap
    commands in the cloud).

    To support Hadoop's distributed cache, paths can be for ordinary
    files, or for archives (which are automatically uncompressed into
    a directory by Hadoop).

    When adding a file, you may optionally assign it a name; if you don't;
    we'll lazily assign it a name as needed. Name collisions are not allowed,
    so being lazy makes it easier to avoid unintended collisions.

    If you wish, you may assign multiple names to the same file, or add
    a path as both a file and an archive (though not mapped to the same name).
    """
    # dirs are not supported directly; runners need to archive them
    # and add that archive.
    #
    # archive_file is just a way of reserving a spot to copy an archive
    # into, in case we need to un-archive it ourselves
    _SUPPORTED_TYPES = ('archive', 'archive_file', 'file')

    def __init__(self, archive_file_suffix=''):
        # map from paths added without a name to None or lazily chosen name
        self._typed_path_to_auto_name = {}
        self._name_to_typed_path = {}
        self._archive_file_suffix = archive_file_suffix

    def add(self, type, path, name=None):
        """Add a path as either a file or an archive, optionally
        assigning it a name.

        :param type: either ``'archive'`` or ``'file'``
        :param path: path/URI to add
        :param name: optional name that this path *must* be assigned, or
                     None to assign this file a name later.

        if *type* is ``archive``, we'll also add *path* as an
        auto-named ``archive_file``. This reserves space in the working dir
        in case we need to copy the archive into the working dir and
        un-archive it ourselves.
        """
        self._check_name(name)
        self._check_type(type)

        # stop name collisions
        if name in self._name_to_typed_path:
            current_type, current_path = self._name_to_typed_path[name]

            if (type, path) == (current_type, current_path):
                return  # already added
            else:
                raise ValueError(
                    "%s %s#%s won't work because we already have %s %s#%s" % (
                        type, path, name, current_type, current_path, name))

        # if a name was specified, reserve it
        if name:
            self._name_to_typed_path[name] = (type, path)
        # otherwise, get ready to auto-name the file
        else:
            self._typed_path_to_auto_name.setdefault((type, path), None)

        # reserve a name for the archive to be copied to, in case we need
        # to un-archive it ourselves
        if type == 'archive':
            self.add('archive_file', path)

    def name(self, type, path, name=None):
        """Get the name for a path previously added to this
        :py:class:`WorkingDirManager`, assigning one as needed.

        This is primarily for getting the name of auto-named files. If
        the file was added with an assigned name, you must include it
        (and we'll just return *name*).

        We won't ever give an auto-name that's the same an assigned name
        (even for the same path and type).

        :param type: either ``'archive'`` or ``'file'``
        :param path: path/URI
        :param name: known name of the file
        """
        self._check_name(name)
        self._check_type(type)

        if name:
            if name not in self._name_to_typed_path:
                raise ValueError('unknown name: %r' % name)

            return name

        if (type, path) not in self._typed_path_to_auto_name:
            # print useful error message
            if (type, path) in self._name_to_typed_path.values():
                raise ValueError('%s %r was never added without a name!' %
                                 (type, path))
            else:
                raise ValueError('%s %r was never added!' % (type, path))

        if not self._typed_path_to_auto_name[(type, path)]:
            strip_ext = False
            suffix = ''

            if type == 'archive':
                # weird to have .tar.gz etc. in a directory name
                strip_ext = True
            elif type == 'archive_file':
                # keep Spark from auto-untarring by adding .file
                suffix = self._archive_file_suffix

            name = name_uniquely(
                path, names_taken=self._name_to_typed_path,
                strip_ext=strip_ext, suffix=suffix)

            self._name_to_typed_path[name] = (type, path)
            self._typed_path_to_auto_name[(type, path)] = name

        return self._typed_path_to_auto_name[(type, path)]

    def name_to_path(self, type=None):
        """Get a map from name (in the setup directory) to path for
        all known files/archives, so we can build :option:`-file` and
        :option:`-archive` options to Hadoop (or fake them in a bootstrap
        script).

        :param type: either ``'archive'`` or ``'file'``
        """
        if type is not None:
            self._check_type(type)

        for path_type, path in self._typed_path_to_auto_name:
            if type is None or path_type == type:
                self.name(path_type, path)

        return dict((name, typed_path[1])
                    for name, typed_path
                    in self._name_to_typed_path.items()
                    if (type is None or typed_path[0] == type))

    def paths(self, type=None):
        """Get a set of all paths tracked by this WorkingDirManager."""
        paths = set()

        for path_type, path in self._typed_path_to_auto_name:
            if type is None or path_type == type:
                paths.add(path)

        for path_type, path in self._name_to_typed_path.values():
            if type is None or path_type == type:
                paths.add(path)

        return paths

    def _check_name(self, name):
        if name is None:
            return

        if not isinstance(name, string_types):
            raise TypeError('name must be a string or None: %r' % (name,))

        if '/' in name:
            raise ValueError('names may not contain slashes: %r' % (name,))

    def _check_type(self, type):
        if type not in self._SUPPORTED_TYPES:
            raise ValueError('bad path type %r, must be one of: %s' % (
                type, ', '.join(sorted(self._SUPPORTED_TYPES))))
