# Copyright 2017 Yelp
# Copyright 2018 Google, Inc.
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
"""Emulating the way Hadoop handles input files, decompressing compressed
files based on their file extension.

This module also functions as a :command:`cat` substitute that can handle
compressed files. It it used by :py:mod:`local <mrjob.local>` mode and can
function without the rest of the mrjob library.
"""
import zlib

try:
    import bz2
    bz2  # redefine bz2 for pepflakes
except ImportError:
    bz2 = None


def bunzip2_stream(fileobj, bufsize=1024):
    """Decompress gzipped data on the fly.

    :param fileobj: object supporting ``read()``
    :param bufsize: number of bytes to read from *fileobj* at a time.

    .. warning::

        This yields decompressed chunks; it does *not* split on lines. To get
        lines, wrap this in :py:func:`to_lines`.
    """
    if bz2 is None:
        raise Exception(
            'bz2 module was not successfully imported (likely not installed).')

    d = bz2.BZ2Decompressor()

    for chunk in to_chunks(fileobj):
        part = d.decompress(chunk)
        if part:
            yield part


def gunzip_stream(fileobj, bufsize=1024):
    """Decompress gzipped data on the fly.

    :param fileobj: object supporting ``read()``
    :param bufsize: number of bytes to read from *fileobj* at a time. The
                    default is the same as in :py:mod:`gzip`.

    .. warning::

        This yields decompressed chunks; it does *not* split on lines. To get
        lines, wrap this in :py:func:`to_lines`.
    """
    # see Issue #601 for why we need this.

    # we need this flag to read gzip rather than raw zlib, but it's not
    # actually defined in zlib, so we define it here.
    READ_GZIP_DATA = 16
    d = zlib.decompressobj(READ_GZIP_DATA | zlib.MAX_WBITS)
    for chunk in to_chunks(fileobj, bufsize):
        data = d.decompress(chunk)
        if data:
            yield data


def decompress(readable, path, bufsize=1024):
    """Take a *readable* which supports the ``.read()`` method correponding to
    the given path and returns an iterator that yields chunks of bytes,
    possibly decompressing based on *path*.

    if *readable* appears to be a fileobj, pass it through as-is.

    if *readable* does not have a ``read()`` method, assume that it's
    a generator that yields chunks of bytes
    """
    if path.endswith('.gz'):
        return gunzip_stream(readable)
    elif path.endswith('.bz2'):
        if bz2 is None:
            raise Exception('bz2 module was not successfully imported'
                            ' (likely not installed).')

        return bunzip2_stream(readable)
    elif hasattr(readable, '__iter__'):
        return readable
    else:
        # not a real readable (e.g. boto3 StreamingBody)
        return to_chunks(readable, bufsize=bufsize)


def is_compressed(path):
    return path.endswith('.bz2') or path.endswith('.gz')


def to_chunks(readable, bufsize=1024):
    """Convert *readable*, which is any object supporting ``read()``
    (e.g. fileobjs) to a stream of non-empty ``bytes``.

    If *readable* has an ``__iter__`` method but not a ``read`` method,
    pass through as-is.
    """
    if hasattr(readable, '__iter__') and not hasattr(readable, 'read'):
        for chunk in readable:
            yield chunk
        return

    while True:
        chunk = readable.read(bufsize)
        if chunk:
            yield chunk
        else:
            return
