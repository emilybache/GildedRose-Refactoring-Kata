# Copyright 2009-2012 Yelp and Contributors
# Copyright 2015 Yelp
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
import logging

from mrjob.fs.base import Filesystem

log = logging.getLogger(__name__)


class CompositeFilesystem(Filesystem):
    """Use one of several filesystems depending on the path/URI.

    This only implements the core :py:class:`~mrjob.fs.base.Filesystem`
    interface; access extensions by calling the sub-filsystem directly
    (e.g. ``fs.s3.create_bucket(...)``).
    """
    def __init__(self):
        # names of sub-filesystems, in the order to call them. (The filesystems
        # themselves are stored in the attribute with that name.)
        self._fs_names = []

        # map from fs name to *disable_if* method (see :py:meth:`add`).
        self._disable_if = {}

        # set of names of filesystems that have been disabled
        self._disabled = set()

    def __getattr__(self, name):
        # don't confuse pickling (e.g. __getstate__())
        if name.startswith('__'):
            raise AttributeError(name)

        # go through non-disabled filesystems and pick the first
        # attribute with a matching name
        for fs_name in self._fs_names:
            if fs_name in self._disabled:
                continue

            fs = getattr(self, fs_name)
            if hasattr(fs, name):
                log.warning(
                    'passing %s() through to the top-level filesystem is'
                    ' deprecated and going away in v0.7.0. Try'
                    ' fs.%s.%s(...) instead' % (name, fs_name, name))
                return getattr(fs, name)

        raise AttributeError(name)

    def add_fs(self, name, fs, disable_if=None):
        """Add a filesystem.

        :param fs: a :py:class:~mrjob.fs.base.Filesystem to forward calls to.
        :param name string: Name of this filesystem. It will be directly
                            accessible through that the attribute with that
                            name. Recommended usage is the same name as
                            the module that contains the fs class.
        :param disable_if: A function called with a single argument, an
                           exception raised by ``fs``. If it returns true,
                           futher calls will not be forwarded to ``fs``.
        """
        if name in self._fs_names:
            raise ValueError('name %r is already taken' % name)

        setattr(self, name, fs)
        self._fs_names.append(name)

        if disable_if:
            self._disable_if[name] = disable_if

    def can_handle_path(self, path):
        """We can handle any path handled by any (non-disabled) filesystem."""
        for fs_name in self._fs_names:
            if fs_name in self._disabled:
                continue

            fs = getattr(self, fs_name)
            if fs.can_handle_path(path):
                return True

        return False

    def _handle(self, name, path_to_handle, *args, **kwargs):
        """Call method named *name* on the first (non-disabled) filesystem
        that says it can handle every path in *paths_to_handle*. If it raises
        an exception, either disable the filesystem and continue, or
        re-raise the exception."""
        for fs_name in self._fs_names:
            if fs_name in self._disabled:
                continue

            fs = getattr(self, fs_name)
            if not fs.can_handle_path(path_to_handle):
                continue

            try:
                return getattr(fs, name)(*args, **kwargs)
            except Exception as ex:
                if (fs_name in self._disable_if and
                        self._disable_if[fs_name](ex)):
                    log.debug('disabling %s fs: %r' % (fs_name, ex))

                    self._disabled.add(fs_name)
                else:
                    raise

        raise IOError("Can't handle path: %s" % path_to_handle)

    def _do(self, name, path):
        """Handle the common case, where a method operates on a single path."""
        return self._handle(name, path, path)

    # explicitly implement Filesystem interface. this will come in handy

    def cat(self, path_glob):
        return self._do('cat', path_glob)

    def _cat_file(self, path):
        # mrjob/runner.py accesses this directly for efficiency
        return self._do('_cat_file', path)

    def du(self, path_glob):
        return self._do('du', path_glob)

    def ls(self, path_glob):
        return self._do('ls', path_glob)

    def exists(self, path_glob):
        return self._do('exists', path_glob)

    def mkdir(self, path):
        return self._do('mkdir', path)

    def join(self, path, *paths):
        return self._handle('join', path, path, *paths)

    def put(self, src, path):
        return self._handle('put', path, src, path)

    def rm(self, path_glob):
        return self._do('rm', path_glob)

    def touchz(self, path):
        return self._do('touchz', path)

    def md5sum(self, path):
        return self._do('md5sum', path)
