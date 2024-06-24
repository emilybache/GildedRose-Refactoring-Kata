# Copyright 2009-2012 Yelp
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
""""mrjob.conf" is the name of both this module, and the global config file
for :py:mod:`mrjob`.
"""
import glob
import json
import logging
import os
import os.path

# yaml is nice to have, but we can fall back on JSON if need be
try:
    import yaml
    yaml  # quiet "redefinition of unused ..." warning from pyflakes
except ImportError:
    yaml = None

from mrjob.py2 import string_types
from mrjob.util import expand_path
from mrjob.util import shlex_split

log = logging.getLogger(__name__)


### finding config files ###

def find_mrjob_conf():
    """Look for :file:`mrjob.conf`, and return its path. Places we look:

    - The location specified by :envvar:`MRJOB_CONF`
    - :file:`~/.mrjob.conf`
    - :file:`/etc/mrjob.conf`

    Return ``None`` if we can't find it.
    """
    def candidates():
        if 'MRJOB_CONF' in os.environ:
            yield expand_path(os.environ['MRJOB_CONF'])

        # $HOME isn't necessarily set on Windows, but ~ works
        # use os.path.join() so we don't end up mixing \ and /
        yield expand_path(os.path.join('~', '.mrjob.conf'))

        # this only really makes sense on Unix, so no os.path.join()
        yield '/etc/mrjob.conf'

    for path in candidates():
        log.debug('Looking for configs in %s' % path)
        if os.path.exists(path):
            log.info('Using configs in %s' % path)
            return path
    else:
        log.info('No configs found; falling back on auto-configuration')
        return None


def _expanded_mrjob_conf_path(conf_path=None):
    """Return the path of a single conf file. If *conf_path* is ``False``,
    return ``None``, and if it's ``None``, return :py:func:`find_mrjob_conf`.
    Otherwise, expand environment variables and ``~`` in *conf_path* and
    return it.

    Confusingly, this function doesn't actually return a "real" path according
    to ``os.path.realpath()``; it just resolves environment variables and
    ``~``.
    """
    if conf_path is False:
        return None
    elif conf_path is None:
        return find_mrjob_conf()
    else:
        return expand_path(conf_path)


### !clear tag ###

class ClearedValue(object):
    """Wrap a value tagged with !clear in mrjob.conf"""

    def __init__(self, value):
        self.value = value

    def __eq__(self, other):
        if isinstance(other, ClearedValue):
            return self.value == other.value
        else:
            return False

    def __hash__(self):
        return hash(self.value)

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, repr(self.value))


def _cleared_value_constructor(loader, node):
    # tried construct_object(), got an unconstructable recursive node warning
    if isinstance(node, yaml.MappingNode):
        value = loader.construct_mapping(node)
    elif isinstance(node, yaml.ScalarNode):
        # resolve null as None, not u'null'
        value = yaml.safe_load(node.value)
    elif isinstance(node, yaml.SequenceNode):
        value = loader.construct_sequence(node)
    else:
        raise TypeError

    return ClearedValue(value)


def _load_yaml_with_clear_tag(stream):
    """Like yaml.safe_load(), but everything with a !clear tag before it
    will be wrapped in ClearedValue()."""
    loader = yaml.SafeLoader(stream)
    loader.add_constructor('!clear', _cleared_value_constructor)
    try:
        return loader.get_single_data()
    finally:
        if hasattr(loader, 'dispose'):  # it doesn't in PyYAML 3.09
            loader.dispose()


def _cleared_value_representer(dumper, data):
    if not isinstance(data, ClearedValue):
        raise TypeError
    node = dumper.represent_data(data.value)
    node.tag = '!clear'
    return node


def _dump_yaml_with_clear_tags(data, stream=None, **kwds):
    class ClearedValueSafeDumper(yaml.SafeDumper):
        pass

    ClearedValueSafeDumper.add_representer(
        ClearedValue, _cleared_value_representer)

    return yaml.dump_all([data], stream, Dumper=ClearedValueSafeDumper, **kwds)


def _fix_clear_tags(x):
    """Recursively resolve :py:class:`ClearedValue` wrappers so that
    ``ClearedValue(...)`` can only wrap values in dicts (and in the top-level
    value we return).

    In dicts, we treat ``ClearedValue(k): v`` or
    ``ClearedValue(k): ClearedValue(v)`` as equivalent to
    ``k: ClearedValue(v)``. ``ClearedValue(k): v1`` overrides ``k: v2``.

    In lists, any ClearedValue wrappers are simply stripped.
    """
    _fix = _fix_clear_tags

    if isinstance(x, list):
        return [_fix(_strip_clear_tag(item)) for item in x]

    elif isinstance(x, dict):
        d = dict((_fix(k), _fix(v)) for k, v in x.items())

        # handle cleared keys
        for k, v in list(d.items()):
            if isinstance(k, ClearedValue):
                del d[k]
                d[_strip_clear_tag(k)] = ClearedValue(_strip_clear_tag(v))

        return d

    elif isinstance(x, ClearedValue):
        return ClearedValue(_fix(x.value))

    else:
        return x


def _resolve_clear_tags_in_list(items):
    """Create a list from *items*. If we encounter a :py:class:`ClearedValue`,
    unwrap it and ignore previous values. Used by ``combine_*()`` functions
    to combine lists of values.
    """
    result = []

    for item in items:
        if isinstance(item, ClearedValue):
            result = [item.value]
        else:
            result.append(item)

    return result


def _strip_clear_tag(v):
    """remove the clear tag from the given value."""
    if isinstance(v, ClearedValue):
        return v.value
    else:
        return v


### reading mrjob.conf ###

def _conf_object_at_path(conf_path):
    if conf_path is None:
        return None

    with open(conf_path) as f:
        if yaml:
            return _fix_clear_tags(_load_yaml_with_clear_tag(f))
        else:
            try:
                return json.load(f)
            except ValueError as e:
                raise ValueError(
                    'Could not read JSON from %s\n  %s\n\n'
                    'If your conf file is in YAML, you need to'
                    ' `pip install PyYAML` to read it' % (conf_path, str(e)))


def load_opts_from_mrjob_conf(runner_alias, conf_path=None,
                              already_loaded=None):
    """Load a list of dictionaries representing the options in a given
    mrjob.conf for a specific runner, resolving includes. Returns
    ``[(path, values)]``. If *conf_path* is not found, return ``[(None, {})]``.

    :type runner_alias: str
    :param runner_alias: String identifier of the runner type, e.g. ``emr``,
                         ``local``, etc.
    :type conf_path: str
    :param conf_path: location of the file to load
    :type already_loaded: list
    :param already_loaded: list of real (according to ``os.path.realpath()``)
                           conf paths that have already
                           been loaded (used by
                           :py:func:`load_opts_from_mrjob_confs`).

    Relative ``include:`` paths are relative to the real (after resolving
    symlinks) path of the including conf file

    This will only load each config file once, even if it's referenced
    from multiple paths due to symlinks.
    """
    if already_loaded is None:
        already_loaded = []

    conf_path = _expanded_mrjob_conf_path(conf_path)
    return _load_opts_from_mrjob_conf(runner_alias, conf_path, already_loaded)


def _load_opts_from_mrjob_conf(runner_alias, conf_path, already_loaded):
    """Helper for :py:func:`load_opts_from_mrjob_conf` for recursive use.
    This doesn't expand or default *conf_path*.
    """
    conf = _conf_object_at_path(conf_path)

    if conf is None:
        return [(None, {})]

    # don't load same conf file twice
    real_conf_path = os.path.realpath(conf_path)

    if real_conf_path in already_loaded:
        return []
    else:
        already_loaded.append(real_conf_path)

    # get configs for our runner out of conf file
    try:
        values = conf['runners'][runner_alias] or {}
    except (KeyError, TypeError, ValueError):
        values = {}

    inherited = []
    if conf.get('include', None):
        includes = conf['include']
        if isinstance(includes, string_types):
            includes = [includes]

        # handle includes in reverse order so that include order takes
        # precedence over inheritance
        for include in reversed(includes):
            # make include relative to (real) conf_path (see #1166)
            # expand ~ *before* joining to dir of including file (see #1308)
            include = os.path.join(os.path.dirname(real_conf_path),
                                   expand_path(include))

            inherited = _load_opts_from_mrjob_conf(
                runner_alias, include, already_loaded) + inherited

    return inherited + [(conf_path, values)]


def load_opts_from_mrjob_confs(runner_alias, conf_paths=None):
    """Load a list of dictionaries representing the options in a given
    list of mrjob config files for a specific runner. Returns
    ``[(path, values), ...]``. If a path is not found, use ``(None, {})`` as
    its value.

    If *conf_paths* is ``None``, look for a config file in the default
    locations (see :py:func:`find_mrjob_conf`).

    :type runner_alias: str
    :param runner_alias: String identifier of the runner type, e.g. ``emr``,
                         ``local``, etc.
    :type conf_paths: list or ``None``
    :param conf_path: locations of the files to load

    This will only load each config file once, even if it's referenced
    from multiple paths due to symlinks.
    """
    if conf_paths is None:
        results = load_opts_from_mrjob_conf(runner_alias)
    else:
        # don't include conf files that were loaded earlier in conf_paths
        already_loaded = []

        # load configs in reversed order so that order of conf paths takes
        # precedence over inheritance
        results = []

        for path in reversed(conf_paths):
            results = load_opts_from_mrjob_conf(
                runner_alias, path, already_loaded=already_loaded) + results

    if runner_alias and not any(conf for path, conf in results):
        log.warning('No configs specified for %s runner' % runner_alias)

    return results


### writing mrjob.conf ###

def dump_mrjob_conf(conf, f):
    """Write out configuration options to a file.

    Useful if you don't want to bother to figure out YAML.

    *conf* should look something like this:

        {'runners':
            'local': {'OPTION': VALUE, ...}
            'emr': {'OPTION': VALUE, ...}
            'hadoop: {'OPTION': VALUE, ...}
        }

    :param f: a file object to write to (e.g. ``open('mrjob.conf', 'w')``)
    """
    if yaml:
        _dump_yaml_with_clear_tags(conf, f, default_flow_style=False)
    else:
        json.dump(conf, f, indent=2)
    f.flush()


### COMBINING OPTIONS ###

# combiners generally consider earlier values to be defaults, and later
# options to override or add on to them.

# combiners assume that the list of values passed to them has already been
# passed through _fix_clear_tags() (that is, the only place ClearedValue
# appears is values in dicts).


def combine_values(*values):
    """Return the last value in *values* that is not ``None``.

    The default combiner; good for simple values (booleans, strings, numbers).
    """
    for v in reversed(values):
        if v is not None:
            return v
    else:
        return None


def combine_lists(*seqs):
    """Concatenate the given sequences into a list. Ignore ``None`` values.

    Generally this is used for a list of commands we want to run; the
    "default" commands get run before any commands specific to your job.

    Strings, bytes, and non-sequence objects (e.g. numbers) are treated as
    single-item lists.
    """
    result = []

    for seq in seqs:
        if seq is None:
            continue

        if isinstance(seq, (bytes, string_types, dict)):
            result.append(seq)
        else:
            try:
                result.extend(seq)
            except:
                result.append(seq)

    return result


def combine_cmds(*cmds):
    """Take zero or more commands to run on the command line, and return
    the last one that is not ``None``. Each command should either be a list
    containing the command plus switches, or a string, which will be parsed
    with :py:func:`shlex.split`. The string must either be a byte string or a
    unicode string containing no non-ASCII characters.

    Returns either ``None`` or a list containing the command plus arguments.
    """
    cmd = combine_values(*cmds)

    if cmd is None:
        return None
    elif isinstance(cmd, string_types):
        return shlex_split(cmd)
    else:
        return list(cmd)


def combine_dicts(*dicts):
    """Combine zero or more dictionaries. Values from dicts later in the list
    take precedence over values earlier in the list.

    If you pass in ``None`` in place of a dictionary, it will be ignored.
    """
    result = {}

    for d in dicts:
        if d:
            for k, v in d.items():
                # delete cleared key
                if isinstance(v, ClearedValue) and v.value is None:
                    result.pop(k, None)

                # just set the value
                else:
                    result[k] = _strip_clear_tag(v)

    return result


def combine_envs(*envs):
    """Combine zero or more dictionaries containing environment variables.
    Environment variable values may be wrapped in :py:class:`ClearedValue`.

    Environment variables later from dictionaries later in the list take
    priority over those earlier in the list.

    For variables ending with ``PATH``, we prepend (and add a colon) rather
    than overwriting. Wrapping a path value in :py:class:`ClearedValue`
    disables this behavior.

    Environment set to ``ClearedValue(None)`` will *delete* environment
    variables earlier in the list, rather than setting them to ``None``.

    If you pass in ``None`` in place of a dictionary in **envs**, it will be
    ignored.
    """
    return _combine_envs_helper(envs, local=False)


def combine_local_envs(*envs):
    """Same as :py:func:`combine_envs`, except that paths are combined
    using the local path separator (e.g ``;`` on Windows rather than ``:``).
    """
    return _combine_envs_helper(envs, local=True)


def _combine_envs_helper(envs, local):
    if local:
        pathsep = os.pathsep
    else:
        pathsep = ':'

    result = {}
    for env in envs:
        if env:
            for k, v in env.items():
                # delete cleared keys
                if isinstance(v, ClearedValue) and v.value is None:
                    result.pop(k, None)

                # append paths
                elif (k.endswith('PATH') and result.get(k) and
                      not isinstance(v, ClearedValue)):
                    result[k] = v + pathsep + result[k]

                # just set the value
                else:
                    result[k] = _strip_clear_tag(v)

    return result


def combine_jobconfs(*jobconfs):
    """Like combine_dicts(), but non-string values are converted to
    Java-readable string (e.g. True becomes 'true'). Keys whose
    value is None are blanked out."""
    j = combine_dicts(*jobconfs)

    return {k: _to_java_str(v) for k, v in j.items() if v is not None}


def combine_paths(*paths):
    """Returns the last value in *paths* that is not ``None``.
    Resolve ``~`` (home dir) and environment variables."""
    return expand_path(combine_values(*paths))


def combine_path_lists(*path_seqs):
    """Concatenate the given sequences into a list. Ignore None values.
    Resolve ``~`` (home dir) and environment variables, and expand globs
    that refer to the local filesystem.

    Can take single strings as well as lists.
    """
    results = []

    for path in combine_lists(*path_seqs):
        expanded = expand_path(path)
        # if we can't expand a glob, leave as-is (maybe it refers to
        # S3 or HDFS)
        paths = sorted(glob.glob(expanded)) or [expanded]

        results.extend(paths)

    return results


def combine_opts(combiners, *opts_list):
    """The master combiner, used to combine dictionaries of options with
    appropriate sub-combiners.

    :param combiners: a map from option name to a combine_*() function to
                      combine options by that name. By default, we combine
                      options using :py:func:`combine_values`.
    :param opts_list: one or more dictionaries to combine

    The dict in *opts_list* may not be wrapped in :py:class:`ClearedValue`,
    but their values may, in which case values of that key from previous
    opt dicts will be ignored.
    """
    final_opts = {}

    keys = set()
    for opts in opts_list:
        if isinstance(opts, ClearedValue):
            raise TypeError
        elif opts:
            keys.update(opts)

    for key in keys:
        values = _resolve_clear_tags_in_list(
            opts[key] for opts in opts_list if opts and key in opts)

        combine_func = combiners.get(key) or combine_values
        final_opts[key] = combine_func(*values)

    return final_opts


def _to_java_str(x):
    """Convert a value (usually for a configuration property) into its
    Java string representation, falling back to the Python representation
    if None is available."""
    # e.g. True -> 'true', None -> 'null'. See #323
    if isinstance(x, string_types):
        return x
    elif x is None:
        # Note: combine_jobconfs() blanks out keys with None values
        return 'null'
    elif isinstance(x, bool):
        return 'true' if x else 'false'
    else:
        return str(x)
