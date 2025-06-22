# Copyright 2009-2012 Yelp and Contributors
# Copyright 2015-2017 Yelp
# Copyright 2019 Yelp
# Copyright 2020 Affirm, Inc.
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
import os
import re
from subprocess import Popen
from subprocess import PIPE

from mrjob.cat import decompress
from mrjob.fs.base import Filesystem
from mrjob.py2 import to_unicode
from mrjob.util import cmd_line


_SSH_URI_RE = re.compile(
    r'^ssh://(?P<hostname>[^/]+)?(?P<filesystem_path>/.*)$')

log = logging.getLogger(__name__)


class SSHFilesystem(Filesystem):
    """Filesystem for remote systems accessed via SSH. Typically you will get
    one of these via ``EMRJobRunner().fs``, composed with
    :py:class:`~mrjob.fs.s3.S3Filesystem` and
    :py:class:`~mrjob.fs.local.LocalFilesystem`.
    """

    # adding default to ssh_add_bin only because SSHFilesystem is technically
    # user-facing. remove this in the release after v0.7.x
    def __init__(self, ssh_bin, ec2_key_pair_file, ssh_add_bin=None):
        """
        :param ssh_bin: path to ``ssh`` binary
        :param ec2_key_pair_file: path to an SSH keyfile
        :param ssh_add_bin: path to ``ssh-add`` binary if any
        """
        super(SSHFilesystem, self).__init__()
        self._ssh_bin = ssh_bin
        self._ec2_key_pair_file = ec2_key_pair_file
        if self._ec2_key_pair_file is None:
            raise ValueError('ec2_key_pair_file must be a path')

        self._ssh_add_bin = ssh_add_bin or ['ssh-add']

        # keep track of hosts we've already copied the key pair to
        self._hosts_with_key_pair_file = set()

        # keep track of which hosts we've copied our key to, and
        # what the (random) name of the key file is on that host
        self._host_to_key_filename = {}

        # should we use sudo (for EMR)? Enable with use_sudo_over_ssh().
        self._sudo = False

    def _ssh_cmd_args(self, address, cmd_args):
        """Return an ssh command that would run the given command on
        the given *address*.

        Address consists of one or most hosts, joined by '!' (so that
        we can reach hosts only accessible through an internal network).

        We assume that any host we SSH into is a UNIX system, and that
        we don't need sudo to run ssh itself. We also assume the username
        is always ``hadoop``.
        """
        args = []

        for i, host in enumerate(address.split('!')):

            args.extend(self._ssh_bin)

            if i == 0:
                args.extend(['-i', self._ec2_key_pair_file])

            known_hosts_file = os.devnull if i == 0 else '/dev/null'

            args.extend(
                [
                    '-o', 'UserKnownHostsFile=' + known_hosts_file,
                    '-o', 'StrictHostKeyChecking=no',
                    '-o', 'VerifyHostKeyDNS=no',
                    '-A',
                    'hadoop@' + host,
                ]
            )

        if self._sudo:
            args.append('sudo')

        args.extend(cmd_args)

        return args

    def _ssh_launch(self, address, cmd_args):
        """Copy SSH keys if necessary, then launch the given command
        over SSH and return a Popen."""
        if '!' in address:
            self._ssh_add_key()

        args = self._ssh_cmd_args(address, cmd_args)

        log.debug('  > ' + cmd_line(args))
        try:
            return Popen(args, stdout=PIPE, stderr=PIPE)
        except OSError as ex:
            raise IOError(ex.strerror)

    def _ssh_run(self, address, cmd_args):
        """Run the given SSH command, and raise an IOError if it fails.
        Return ``(stdout, stderr)``

        Use this for commands with a bounded amount of output.
        """
        p = self._ssh_launch(address, cmd_args)

        stdout, stderr = p.communicate()

        if p.returncode != 0:
            raise IOError(to_unicode(stderr))

        return stdout, stderr

    def _ssh_finish_run(self, p):
        """Close file handles and do error handling on a ``Popen``
        who we've read stdout from but done nothing else."""
        stderr = p.stderr.read()

        p.stdout.close()
        p.stderr.close()

        returncode = p.wait()

        if returncode != 0:
            raise IOError(stderr)

    def _ssh_add_key(self):
        """Add ``self._ec2_key_pair_file`` to the ssh agent with ``ssh-add``.
        """
        args = self._ssh_add_bin + [
            '-t', '60', self._ec2_key_pair_file]

        log.debug('  > ' + cmd_line(args))

        try:
            p = Popen(args, stdout=PIPE, stderr=PIPE)
        except OSError as ex:
            raise IOError(ex.strerror)

        stdout, stderr = p.communicate()

        if p.returncode != 0:
            raise IOError(to_unicode(stderr))

    def can_handle_path(self, path):
        return _SSH_URI_RE.match(path) is not None

    def du(self, path_glob):
        raise IOError()  # not implemented

    def ls(self, path_glob):
        m = _SSH_URI_RE.match(path_glob)
        addr = m.group('hostname')
        path_to_ls = m.group('filesystem_path')

        p = self._ssh_launch(
            addr, ['find', '-L', path_to_ls, '-type', 'f'])

        for line in p.stdout:
            path = to_unicode(line).rstrip('\n')
            yield 'ssh://%s%s' % (addr, path)

        self._ssh_finish_run(p)

    def md5sum(self, path):
        raise IOError()  # not implemented

    def _cat_file(self, path):
        m = _SSH_URI_RE.match(path)
        addr = m.group('hostname')
        fs_path = m.group('filesystem_path')

        p = self._ssh_launch(addr, ['cat', fs_path])

        for chunk in decompress(p.stdout, fs_path):
            yield chunk

        self._ssh_finish_run(p)

    def mkdir(self, path):
        raise IOError()  # not implemented

    def exists(self, path_glob):
        # just fall back on ls(); it's smart
        try:
            return any(self.ls(path_glob))
        except IOError:
            return False

    def rm(self, path_glob):
        raise IOError()  # not implemented

    def touchz(self, path):
        raise IOError()  # not implemented

    def use_sudo_over_ssh(self, sudo=True):
        """Use this to turn on *sudo* (we do this depending on the AMI
        version on EMR)."""
        self._sudo = sudo
