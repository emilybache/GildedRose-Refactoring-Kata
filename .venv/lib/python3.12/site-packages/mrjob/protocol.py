# Copyright 2009-2013 Yelp and Contributors
# Copyright 2015-2017 Yelp
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

"""Protocols translate raw bytes into key, value pairs.

Typically, protocols encode a key and value into bytes, and join them together
with a tab character.

However, protocols with ``Value`` in their name ignore
keys and simply read/write values (with key read in as ``None``), allowing
you to read and write data in arbitrary formats.

For more information, see :ref:`job-protocols` and :ref:`writing-protocols`.
"""
# This is one of the few places where efficiency really matters; to that end,
# we maintain separate code for Python 2 and 3 where necessary. Tests of
# protocols should *not* have different code for different versions of Python.

# don't add imports here that aren't part of the standard Python library,
# since MRJobs need to run in Amazon's generic EMR environment
import json

try:
    import cPickle as pickle  # Python 2 only
except ImportError:
    import pickle

from mrjob.py2 import PY2
from mrjob.util import safeeval


try:
    import rapidjson
    rapidjson
except ImportError:
    rapidjson = None

try:
    import simplejson
    simplejson  # quiet "redefinition of unused ..." warning from pyflakes
except ImportError:
    simplejson = None

try:
    import ujson
    ujson  # quiet "redefinition of unused ..." warning from pyflakes
except ImportError:
    ujson = None


class _KeyCachingProtocol(object):
    """Protocol that caches the last decoded key.

    We're not currently exposing this class; inheriting from this class
    will result in almost as much code as simply writing your own read/write
    methods. You should probably cache keys, but in a way that makes sense for
    your use case.
    """
    _last_key_encoded = None
    _last_key_decoded = None

    def _loads(self, value):
        """Decode a single key/value, and return it."""
        raise NotImplementedError

    def _dumps(self, value):
        """Encode a single key/value, and return it."""
        raise NotImplementedError

    def read(self, line):
        """Decode a line of input.

        :type line: str
        :param line: A line of raw input to the job, without trailing newline.

        :return: A tuple of ``(key, value)``."""

        raw_key, raw_value = line.split(b'\t', 1)

        if raw_key != self._last_key_encoded:
            self._last_key_encoded = raw_key
            self._last_key_decoded = self._loads(raw_key)
        return (self._last_key_decoded, self._loads(raw_value))

    def write(self, key, value):
        """Encode a key and value.

        :param key: A key (of any type) yielded by a mapper/reducer
        :param value: A value (of any type) yielded by a mapper/reducer

        :rtype: str
        :return: A line, without trailing newline."""
        return self._dumps(key) + b'\t' + self._dumps(value)


# JSONProtocol (below) is just an alias, but we treat it as a class for the
# purpose of documentation. It encodes key and value as two JSONs separated
# by a tab.
#
# Same for JSONValueProtocol, except it encodes only the value (key
# is read as ``None``).

class StandardJSONProtocol(_KeyCachingProtocol):
    """Implements :py:class:`JSONProtocol` using Python's built-in JSON
    library.

    .. note::

        The built-in ``json`` library is (appropriately) strict about the JSON
        standard; it won't accept dictionaries with non-string keys, sets, or
        (on Python 3) bytestrings.
    """
    if PY2:
        def _loads(self, value):
            return json.loads(value)

        def _dumps(self, value):
            return json.dumps(value)
    else:
        def _loads(self, value):
            # Python 3's json module does not accept bytes
            return json.loads(value.decode('utf_8'))

        def _dumps(self, value):
            return json.dumps(value).encode('utf_8')


class StandardJSONValueProtocol(object):
    """Implements :py:class:`JSONValueProtocol` using Python's built-in JSON
    library.
    """
    if PY2:
        def read(self, line):
            return (None, json.loads(line))

        def write(self, key, value):
            return json.dumps(value)
    else:
        def read(self, line):
            # Python 3's json module does not accept bytes
            return (None, json.loads(line.decode('utf_8')))

        def write(self, key, value):
            return json.dumps(value).encode('utf_8')


class RapidJSONProtocol(_KeyCachingProtocol):
    """Implements :py:class:`JSONProtocol` using the :py:mod:`rapidjson`
    library.
    """
    # rapidjson only exists in Python 3, so no special cases for Python 3

    def _loads(self, value):
        return rapidjson.loads(value)

    def _dumps(self, value):
        return rapidjson.dumps(value).encode('utf_8')


class RapidJSONValueProtocol(object):
    """Implements :py:class:`JSONValueProtocol` using the :py:mod:`rapidjson`
    library.
    """
    # rapidjson only exists in Python 3, so no special cases for Python 3

    def read(self, line):
        return (None, rapidjson.loads(line))

    def write(self, key, value):
        return rapidjson.dumps(value).encode('utf_8')


class SimpleJSONProtocol(_KeyCachingProtocol):
    """Implements :py:class:`JSONProtocol` using the :py:mod:`simplejson`
    library."""
    def _loads(self, value):
        # simplejson can handle bytes even in Python 3
        return simplejson.loads(value)

    if PY2:
        def _dumps(self, value):
            return simplejson.dumps(value)
    else:
        def _dumps(self, value):
            return simplejson.dumps(value).encode('utf_8')


class SimpleJSONValueProtocol(object):
    """Implements :py:class:`JSONValueProtocol` using the :py:mod:`simplejson`
    library.
    """
    def read(self, line):
        # simplejson can handle bytes even in Python 3
        return (None, simplejson.loads(line))

    if PY2:
        def write(self, key, value):
            return simplejson.dumps(value)
    else:
        def write(self, key, value):
            return simplejson.dumps(value).encode('utf_8')


class UltraJSONProtocol(_KeyCachingProtocol):
    """Implements :py:class:`JSONProtocol` using the :py:mod:`ujson` library.

    .. warning::

        :py:mod:`ujson` is about five times faster than the standard
        implementation, but is more willing to encode things that aren't
        strictly JSON-encodable, including sets, dictionaries with
        tuples as keys, UTF-8 encoded bytes, and objects (!). Relying on this
        behavior won't stop your job from working, but it can
        make your job *dependent* on :py:mod:`ujson`, rather than just using
        it as a speedup.

    .. note::

        :py:mod:`ujson` also differs from the standard implementation in that
        it doesn't  add spaces to its JSONs (``{"foo":"bar"}`` versus
        ``{"foo": "bar"}``). This probably won't affect anything but test
        cases and readability.
    """
    def _loads(self, value):
        # ujson can handle bytes even in Python 3
        return ujson.loads(value)

    if PY2:
        def _dumps(self, value):
            return ujson.dumps(value)
    else:
        def _dumps(self, value):
            return ujson.dumps(value).encode('utf_8')


class UltraJSONValueProtocol(object):
    """Implements :py:class:`JSONValueProtocol` using the :py:mod:`ujson`
    library.
    """
    def read(self, line):
        # ujson can handle bytes even in Python 3
        return (None, ujson.loads(line))

    if PY2:
        def write(self, key, value):
            return ujson.dumps(value)
    else:
        def write(self, key, value):
            return ujson.dumps(value).encode('utf_8')


# use ujson by default if available
if ujson:
    JSONProtocol = UltraJSONProtocol
    JSONValueProtocol = UltraJSONValueProtocol
# otherwise, try rapidjson. This library is supposed to be Python 3+
# only, so don't try to use it on Python 2
elif rapidjson and not PY2:
    JSONProtocol = RapidJSONProtocol
    JSONValueProtocol = RapidJSONValueProtocol
# otherwise, try simplejson
elif simplejson:
    JSONProtocol = SimpleJSONProtocol
    JSONValueProtocol = SimpleJSONValueProtocol
# fall back to the built-in JSON module
else:
    JSONProtocol = StandardJSONProtocol
    JSONValueProtocol = StandardJSONValueProtocol


class PickleProtocol(_KeyCachingProtocol):
    """Encode ``(key, value)`` as two string-escaped pickles separated
    by a tab.

    We string-escape the pickles to avoid having to deal with stray
    ``\\t`` and ``\\n`` characters, which would confuse Hadoop
    Streaming.

    Ugly, but should work for any type.

    .. warning::

        Pickling is only *backwards*-compatible across Python versions. If your
        job uses this as an output protocol, you should use at least the same
        version of Python to parse the job's output. Vice versa for using this
        as an input protocol.
    """

    # string_escape doesn't exist on Python 3 (you can't .decode() bytes).
    # Since efficiency matters for protocols, keeping separate code
    # for Python 2 and 3
    if PY2:
        def _loads(self, value):
            return pickle.loads(value.decode('string_escape'))

        def _dumps(self, value):
            return pickle.dumps(value).encode('string_escape')
    else:
        def _loads(self, value):
            return pickle.loads(
                value.decode('unicode_escape').encode('latin_1'))

        def _dumps(self, value):
            return pickle.dumps(value).decode(
                'latin_1').encode('unicode_escape')


class PickleValueProtocol(object):
    """Encode ``value`` as a string-escaped pickle and discard ``key``
    (``key`` is read in as ``None``).

    See :py:class:`PickleProtocol` for details.
    """
    if PY2:
        def read(self, line):
            return (None, pickle.loads(line.decode('string_escape')))

        def write(self, key, value):
            return pickle.dumps(value).encode('string_escape')
    else:
        def read(self, line):
            return (None, pickle.loads(
                line.decode('unicode_escape').encode('latin_1')))

        def write(self, key, value):
            return pickle.dumps(value).decode(
                'latin_1').encode('unicode_escape')


# RawValueProtocol (below) is just an alias, but we treat it as a class for the
# purpose of documentation. All it does is output the value (key is read as
# ``None``).
#
# Same for RawProtocol, except it encodes key and value, separated by a tab.

class BytesProtocol(object):
    """Encode ``(key, value)`` (bytestrings) as ``key`` and ``value``
    separated by a tab.

    If ``key`` or ``value`` is ``None``, don't include a tab. When decoding a
    line with no tab in it, ``value`` will be ``None``.

    When reading from a line with multiple tabs, we break on the first one.

    Your key should probably not be ``None`` or have tab characters in it, but
    we don't check.
    """
    def read(self, line):
        key_value = line.split(b'\t', 1)
        if len(key_value) == 1:
            key_value.append(None)

        return tuple(key_value)

    def write(self, key, value):
        return b'\t'.join(x for x in (key, value) if x is not None)


class BytesValueProtocol(object):
    """Read line (without trailing newline) directly into ``value`` (``key``
    is always ``None``). Output ``value`` (bytes) directly, discarding ``key``.

    **This is the default protocol used by jobs to read input on Python 2.**

    .. warning::

        Typical usage on Python 2 is to have your mapper parse (byte) strings
        out of your input files, and then include them in the output to the
        reducer. Since this output is then (by default) JSON-encoded, encoding
        will fail if the bytestrings are not UTF-8 decodable. If this is an
        issue, consider using :py:class:`TextValueProtocol` instead.
    """
    def read(self, line):
        return (None, line)

    def write(self, key, value):
        return value


class TextProtocol(object):
    """UTF-8 encode ``key`` and ``value`` (unicode strings) and join them
    with a tab character. When reading input, we fall back to latin-1 if
    we can't UTF-8 decode the line.

    If ``key`` or ``value`` is ``None``, don't include a tab. When decoding a
    line with no tab in it, ``value`` will be ``None``.

    When reading from a line with multiple tabs, we break on the first one.

    Your key should probably not be ``None`` or have tab characters in it, but
    we don't check.
    """
    def read(self, line):
        try:
            line = line.decode('utf_8')
        except UnicodeDecodeError:
            line = line.decode('latin_1')

        key_value = line.split(u'\t', 1)
        if len(key_value) == 1:
            key_value.append(None)

        return tuple(key_value)

    def write(self, key, value):
        return b'\t'.join(
            x.encode('utf_8') for x in (key, value) if x is not None)


class TextValueProtocol(object):
    """Attempt to UTF-8 decode line (without trailing newline) into ``value``,
    falling back to latin-1. (``key`` is always ``None``). Output ``value``
    UTF-8 encoded, discarding ``key``.

    **This is the default protocol used by jobs to read input on Python 3.**

    This is a good solution for reading text files which are mostly ASCII but
    may have some other bytes of unknown encoding (e.g. logs).

    If you wish to enforce a particular encoding, use
    :py:class:`BytesValueProtocol` instead::

        class MREncodingEnforcer(MRJob):

            INPUT_PROTOCOL = BytesValueProtocol

            def mapper(self, _, value):
                value = value.decode('utf_8')
                ...
    """
    def read(self, line):
        try:
            return (None, line.decode('utf_8'))
        except UnicodeDecodeError:
            return (None, line.decode('latin_1'))

    def write(self, key, value):
        return value.encode('utf_8')


# RawValueProtocol is the default way of reading input. Historically
# (in Python 2), it's always read raw bytes, but Python 3 is pickier about
# bytes, so we use TextValueProcotol (unicode) instead.
if PY2:
    RawProtocol = BytesProtocol
    RawValueProtocol = BytesValueProtocol
else:
    RawProtocol = TextProtocol
    RawValueProtocol = TextValueProtocol


class ReprProtocol(_KeyCachingProtocol):
    """Encode ``(key, value)`` as two reprs separated by a tab.

    This only works for basic types (we use :py:func:`mrjob.util.safeeval`).

    .. warning::

        The repr format changes between different versions of Python (for
        example, braces for sets in Python 2.7, and different string contants
        in Python 3). Plan accordingly.
    """

    def _loads(self, value):
        return safeeval(value)

    if PY2:
        def _dumps(self, value):
            return repr(value)
    else:
        def _dumps(self, value):
            return repr(value).encode('utf_8')


class ReprValueProtocol(object):
    """Encode ``value`` as a repr and discard ``key`` (``key`` is read
    in as None).

    See :py:class:`ReprProtocol` for details.
    """
    def read(self, line):
        return (None, safeeval(line))

    if PY2:
        def write(self, key, value):
            return repr(value)
    else:
        def write(self, key, value):
            return repr(value).encode('utf_8')
