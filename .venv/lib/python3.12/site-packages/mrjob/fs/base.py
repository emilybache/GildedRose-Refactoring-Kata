# Copyright 2009-2015 Yelp and Contributors
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
import os.path
import posixpath

from mrjob.parse import is_uri
from mrjob.parse import urlparse

log = logging.getLogger(__name__)


class Filesystem(object):
    """Some simple filesystem operations that are common across the local
    filesystem, S3, GCS, HDFS, and remote machines via SSH.

    Different runners provide functionality for different filesystems via their
    :py:attr:`~mrjob.runner.MRJobRunner.fs` attribute. Generally a runner will
    wrap one or more filesystems with
    :py:class:`mrjob.fs.composite.CompositeFilesystem`.

    Schemes supported:

    * :py:class:`mrjob.fs.gcs.GCSFilesystem`: ``gs://``
    * :py:class:`mrjob.fs.hadoop.HadoopFilesystem`: ``hdfs://`` and other URIs
    * :py:class:`mrjob.fs.local.LocalFilesystem`: paths and ``file://`` URIs
    * :py:class:`mrjob.fs.s3.S3Filesystem`: ``s3://``, ``s3a://``, ``s3n://``,
    * :py:class:`mrjob.fs.ssh.SSHFilesystem`: ``ssh://``

    .. versionchanged:: 0.6.12

       `LocalFilesystem` added support for ``file://`` URIs
    """
    # Note: currently, we're not very consistent about the names of arguments
    # to these methods in subclasses, which we'll fix in v0.7.0 (see #1979).

    def can_handle_path(self, path):
        """Can we handle this path at all?"""
        False

    def cat(self, path_glob):
        """cat all files matching **path_glob**, decompressing if necessary

        This yields bytes, which don't necessarily correspond to lines
        (see #1544). If multiple files are catted, yields ``b''`` between
        each file.
        """
        for i, filename in enumerate(self.ls(path_glob)):
            if i > 0:
                yield b''  # mark end of previous file

            for line in self._cat_file(filename):
                yield line

    def du(self, path_glob):
        """Get the total size of files matching ``path_glob``

        Corresponds roughly to: ``hadoop fs -du path_glob``
        """
        raise NotImplementedError

    def ls(self, path_glob):
        """Recursively list all files in the given path.

        We don't return directories for compatibility with S3 (which
        has no concept of them)

        Corresponds roughly to: ``hadoop fs -ls -R path_glob``
        """
        raise NotImplementedError

    def _cat_file(self, path):
        """Yield the contents of the file at *path* as a series of ``bytes``,
        not necessarily respecting line boundaries."""
        raise NotImplementedError

    def exists(self, path_glob):
        """Does the given path/URI exist?

        Corresponds roughly to: ``hadoop fs -test -e path_glob``
        """
        raise NotImplementedError

    def join(self, path, *paths):
        """Join *paths* onto *path* (which may be a URI)"""
        all_paths = (path,) + paths

        # if there's a URI, we only care about it and what follows
        for i in range(len(all_paths), 0, -1):
            if is_uri(all_paths[i - 1]):
                scheme, netloc, uri_path = urlparse(all_paths[i - 1])[:3]
                return '%s://%s%s' % (
                    scheme, netloc, posixpath.join(
                        uri_path or '/', *all_paths[i:]))
        else:
            return os.path.join(*all_paths)

    def mkdir(self, path):
        """Create the given dir and its subdirs (if they don't already
        exist). On cloud filesystems (e.g. S3), also create the corresponding
        bucket as needed

        Corresponds roughly to: ``hadoop fs -mkdir -p path``

        .. versionadded:: 0.6.8 creates buckets on cloud filesystems
        """
        raise NotImplementedError

    def put(self, src, path):
        """Upload a file on the local filesystem (*src*) to *path*.
        Like with :py:func:`shutil.copyfile`, *path* should be the full path
        of the new file, not a directory which should contain it.

        Corresponds roughly to ``hadoop fs -put src path``.

        .. versionadded:: 0.6.8
        """
        raise NotImplementedError

    def rm(self, path_glob):
        """Recursively delete the given file/directory, if it exists

        Corresponds roughly to: ``hadoop fs -rm -R path_glob``
        """
        raise NotImplementedError

    def touchz(self, path):
        """Make an empty file in the given location. Raises an error if
        a non-zero length file already exists in that location.

        Correponds to: ``hadoop fs -touchz path``
        """
        raise NotImplementedError

    def md5sum(self, path):
        """Generate the md5 sum of the file at *path*"""
        raise NotImplementedError
