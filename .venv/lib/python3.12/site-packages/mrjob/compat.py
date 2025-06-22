# -*- coding: utf-8 -*-
# Copyright 2009-2012 Yelp
# Copyright 2013-2014 Yelp and Contributors
# Copyright 2015-2016 Yelp
# Copyright 2018 Ben Dalling
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

"""Utility functions for compatibility with different version of hadoop."""
from distutils.version import LooseVersion
import logging
import os

from mrjob.py2 import string_types

# lists alternative names for jobconf variables
# full listing thanks to translation table in
# http://hadoop.apache.org/docs/current/hadoop-project-dist/hadoop-common/DeprecatedProperties.html # noqa

log = logging.getLogger(__name__)

_JOBCONF_DICT_LIST = [
    {'1.0': 'StorageId',
     '2.0': 'dfs.datanode.StorageId'},
    {'1.0': 'create.empty.dir.if.nonexist',
     '2.0': 'mapreduce.jobcontrol.createdir.ifnotexist'},
    {'1.0': 'dfs.access.time.precision',
     '2.0': 'dfs.namenode.accesstime.precision'},
    {'1.0': 'dfs.backup.address',
     '2.0': 'dfs.namenode.backup.address'},
    {'1.0': 'dfs.backup.http.address',
     '2.0': 'dfs.namenode.backup.http-address'},
    {'1.0': 'dfs.balance.bandwidthPerSec',
     '2.0': 'dfs.datanode.balance.bandwidthPerSec'},
    {'1.0': 'dfs.block.size',
     '2.0': 'dfs.blocksize'},
    {'1.0': 'dfs.client.buffer.dir',
     '2.0': 'fs.client.buffer.dir'},
    {'1.0': 'dfs.data.dir',
     '2.0': 'dfs.datanode.data.dir'},
    {'1.0': 'dfs.datanode.max.xcievers',
     '2.0': 'dfs.datanode.max.transfer.threads'},
    {'1.0': 'dfs.df.interval',
     '2.0': 'fs.df.interval'},
    {'1.0': 'dfs.http.address',
     '2.0': 'dfs.namenode.http-address'},
    {'1.0': 'dfs.https.address',
     '2.0': 'dfs.namenode.https-address'},
    {'1.0': 'dfs.https.client.keystore.resource',
     '2.0': 'dfs.client.https.keystore.resource'},
    {'1.0': 'dfs.https.need.client.auth',
     '2.0': 'dfs.client.https.need-auth'},
    {'1.0': 'dfs.max-repl-streams',
     '2.0': 'dfs.namenode.replication.max-streams'},
    {'1.0': 'dfs.max.objects',
     '2.0': 'dfs.namenode.max.objects'},
    {'1.0': 'dfs.name.dir',
     '2.0': 'dfs.namenode.name.dir'},
    {'1.0': 'dfs.name.dir.restore',
     '2.0': 'dfs.namenode.name.dir.restore'},
    {'1.0': 'dfs.name.edits.dir',
     '2.0': 'dfs.namenode.edits.dir'},
    {'1.0': 'dfs.permissions',
     '2.0': 'dfs.permissions.enabled'},
    {'1.0': 'dfs.permissions.supergroup',
     '2.0': 'dfs.permissions.superusergroup'},
    {'1.0': 'dfs.read.prefetch.size',
     '2.0': 'dfs.client.read.prefetch.size'},
    {'1.0': 'dfs.replication.considerLoad',
     '2.0': 'dfs.namenode.replication.considerLoad'},
    {'1.0': 'dfs.replication.interval',
     '2.0': 'dfs.namenode.replication.interval'},
    {'1.0': 'dfs.replication.min',
     '2.0': 'dfs.namenode.replication.min'},
    {'1.0': 'dfs.replication.pending.timeout.sec',
     '2.0': 'dfs.namenode.replication.pending.timeout-sec'},
    {'1.0': 'dfs.safemode.extension',
     '2.0': 'dfs.namenode.safemode.extension'},
    {'1.0': 'dfs.safemode.threshold.pct',
     '2.0': 'dfs.namenode.safemode.threshold-pct'},
    {'1.0': 'dfs.secondary.http.address',
     '2.0': 'dfs.namenode.secondary.http-address'},
    {'1.0': 'dfs.socket.timeout',
     '2.0': 'dfs.client.socket-timeout'},
    {'1.0': 'dfs.upgrade.permission',
     '2.0': 'dfs.namenode.upgrade.permission'},
    {'1.0': 'dfs.write.packet.size',
     '2.0': 'dfs.client-write-packet-size'},
    {'1.0': 'fs.checkpoint.dir',
     '2.0': 'dfs.namenode.checkpoint.dir'},
    {'1.0': 'fs.checkpoint.edits.dir',
     '2.0': 'dfs.namenode.checkpoint.edits.dir'},
    {'1.0': 'fs.checkpoint.period',
     '2.0': 'dfs.namenode.checkpoint.period'},
    {'1.0': 'fs.default.name',
     '2.0': 'fs.defaultFS'},
    {'1.0': 'hadoop.configured.node.mapping',
     '2.0': 'net.topology.configured.node.mapping'},
    {'1.0': 'hadoop.job.history.location',
     '2.0': 'mapreduce.jobtracker.jobhistory.location'},
    {'1.0': 'hadoop.native.lib',
     '2.0': 'io.native.lib.available'},
    {'1.0': 'hadoop.net.static.resolutions',
     '2.0': 'mapreduce.tasktracker.net.static.resolutions'},
    {'1.0': 'hadoop.pipes.command-file.keep',
     '2.0': 'mapreduce.pipes.commandfile.preserve'},
    {'1.0': 'hadoop.pipes.executable',
     '2.0': 'mapreduce.pipes.executable'},
    {'1.0': 'hadoop.pipes.executable.interpretor',
     '2.0': 'mapreduce.pipes.executable.interpretor'},
    {'1.0': 'hadoop.pipes.java.mapper',
     '2.0': 'mapreduce.pipes.isjavamapper'},
    {'1.0': 'hadoop.pipes.java.recordreader',
     '2.0': 'mapreduce.pipes.isjavarecordreader'},
    {'1.0': 'hadoop.pipes.java.recordwriter',
     '2.0': 'mapreduce.pipes.isjavarecordwriter'},
    {'1.0': 'hadoop.pipes.java.reducer',
     '2.0': 'mapreduce.pipes.isjavareducer'},
    {'1.0': 'hadoop.pipes.partitioner',
     '2.0': 'mapreduce.pipes.partitioner'},
    {'1.0': 'heartbeat.recheck.interval',
     '2.0': 'dfs.namenode.heartbeat.recheck-interval'},
    {'1.0': 'io.bytes.per.checksum',
     '2.0': 'dfs.bytes-per-checksum'},
    {'1.0': 'io.sort.factor',
     '2.0': 'mapreduce.task.io.sort.factor'},
    {'1.0': 'io.sort.mb',
     '2.0': 'mapreduce.task.io.sort.mb'},
    {'1.0': 'io.sort.spill.percent',
     '2.0': 'mapreduce.map.sort.spill.percent'},
    {'1.0': 'job.end.notification.url',
     '2.0': 'mapreduce.job.end-notification.url'},
    {'1.0': 'job.end.retry.attempts',
     '2.0': 'mapreduce.job.end-notification.retry.attempts'},
    {'1.0': 'job.end.retry.interval',
     '2.0': 'mapreduce.job.end-notification.retry.interval'},
    {'1.0': 'job.local.dir',
     '2.0': 'mapreduce.job.local.dir'},
    {'1.0': 'jobclient.completion.poll.interval',
     '2.0': 'mapreduce.client.completion.pollinterval'},
    {'1.0': 'jobclient.output.filter',
     '2.0': 'mapreduce.client.output.filter'},
    {'1.0': 'jobclient.progress.monitor.poll.interval',
     '2.0': 'mapreduce.client.progressmonitor.pollinterval'},
    {'1.0': 'keep.failed.task.files',
     '2.0': 'mapreduce.task.files.preserve.failedtasks'},
    {'1.0': 'keep.task.files.pattern',
     '2.0': 'mapreduce.task.files.preserve.filepattern'},
    {'1.0': 'key.value.separator.in.input.line',
     '2.0': 'mapreduce.input.keyvaluelinerecordreader.key.value.separator'},
    {'1.0': 'local.cache.size',
     '2.0': 'mapreduce.tasktracker.cache.local.size'},
    {'1.0': 'map.input.file',
     '2.0': 'mapreduce.map.input.file'},
    {'1.0': 'map.input.length',
     '2.0': 'mapreduce.map.input.length'},
    {'1.0': 'map.input.start',
     '2.0': 'mapreduce.map.input.start'},
    {'1.0': 'map.output.key.field.separator',
     '2.0': 'mapreduce.map.output.key.field.separator'},
    {'1.0': 'map.output.key.value.fields.spec',
     '2.0': 'mapreduce.fieldsel.map.output.key.value.fields.spec'},
    {'1.0': 'mapred.acls.enabled',
     '2.0': 'mapreduce.cluster.acls.enabled'},
    {'1.0': 'mapred.binary.partitioner.left.offset',
     '2.0': 'mapreduce.partition.binarypartitioner.left.offset'},
    {'1.0': 'mapred.binary.partitioner.right.offset',
     '2.0': 'mapreduce.partition.binarypartitioner.right.offset'},
    {'1.0': 'mapred.cache.archives',
     '2.0': 'mapreduce.job.cache.archives'},
    {'1.0': 'mapred.cache.archives.timestamps',
     '2.0': 'mapreduce.job.cache.archives.timestamps'},
    {'1.0': 'mapred.cache.files',
     '2.0': 'mapreduce.job.cache.files'},
    {'1.0': 'mapred.cache.files.timestamps',
     '2.0': 'mapreduce.job.cache.files.timestamps'},
    {'1.0': 'mapred.cache.localArchives',
     '2.0': 'mapreduce.job.cache.local.archives'},
    {'1.0': 'mapred.cache.localFiles',
     '2.0': 'mapreduce.job.cache.local.files'},
    {'1.0': 'mapred.child.tmp',
     '2.0': 'mapreduce.task.tmp.dir'},
    {'1.0': 'mapred.cluster.average.blacklist.threshold',
     '2.0': 'mapreduce.jobtracker.blacklist.average.threshold'},
    {'1.0': 'mapred.cluster.map.memory.mb',
     '2.0': 'mapreduce.cluster.mapmemory.mb'},
    {'1.0': 'mapred.cluster.max.map.memory.mb',
     '2.0': 'mapreduce.jobtracker.maxmapmemory.mb'},
    {'1.0': 'mapred.cluster.max.reduce.memory.mb',
     '2.0': 'mapreduce.jobtracker.maxreducememory.mb'},
    {'1.0': 'mapred.cluster.reduce.memory.mb',
     '2.0': 'mapreduce.cluster.reducememory.mb'},
    {'1.0': 'mapred.committer.job.setup.cleanup.needed',
     '2.0': 'mapreduce.job.committer.setup.cleanup.needed'},
    {'1.0': 'mapred.compress.map.output',
     '2.0': 'mapreduce.map.output.compress'},
    {'1.0': 'mapred.create.symlink',
     '2.0': 'mapreduce.job.cache.symlink.create'},
    {'1.0': 'mapred.data.field.separator',
     '2.0': 'mapreduce.fieldsel.data.field.separator'},
    {'1.0': 'mapred.debug.out.lines',
     '2.0': 'mapreduce.task.debugout.lines'},
    {'1.0': 'mapred.healthChecker.interval',
     '2.0': 'mapreduce.tasktracker.healthchecker.interval'},
    {'1.0': 'mapred.healthChecker.script.args',
     '2.0': 'mapreduce.tasktracker.healthchecker.script.args'},
    {'1.0': 'mapred.healthChecker.script.path',
     '2.0': 'mapreduce.tasktracker.healthchecker.script.path'},
    {'1.0': 'mapred.healthChecker.script.timeout',
     '2.0': 'mapreduce.tasktracker.healthchecker.script.timeout'},
    {'1.0': 'mapred.heartbeats.in.second',
     '2.0': 'mapreduce.jobtracker.heartbeats.in.second'},
    {'1.0': 'mapred.hosts',
     '2.0': 'mapreduce.jobtracker.hosts.filename'},
    {'1.0': 'mapred.hosts.exclude',
     '2.0': 'mapreduce.jobtracker.hosts.exclude.filename'},
    {'1.0': 'mapred.inmem.merge.threshold',
     '2.0': 'mapreduce.reduce.merge.inmem.threshold'},
    {'1.0': 'mapred.input.dir',
     '2.0': 'mapreduce.input.fileinputformat.inputdir'},
    {'1.0': 'mapred.input.dir.formats',
     '2.0': 'mapreduce.input.multipleinputs.dir.formats'},
    {'1.0': 'mapred.input.dir.mappers',
     '2.0': 'mapreduce.input.multipleinputs.dir.mappers'},
    {'1.0': 'mapred.input.pathFilter.class',
     '2.0': 'mapreduce.input.pathFilter.class'},
    {'1.0': 'mapred.jar',
     '2.0': 'mapreduce.job.jar'},
    {'1.0': 'mapred.job.classpath.archives',
     '2.0': 'mapreduce.job.classpath.archives'},
    {'1.0': 'mapred.job.classpath.files',
     '2.0': 'mapreduce.job.classpath.files'},
    {'1.0': 'mapred.job.id',
     '2.0': 'mapreduce.job.id'},
    {'1.0': 'mapred.job.map.memory.mb',
     '2.0': 'mapreduce.map.memory.mb'},
    {'1.0': 'mapred.job.name',
     '2.0': 'mapreduce.job.name'},
    {'1.0': 'mapred.job.priority',
     '2.0': 'mapreduce.job.priority'},
    {'1.0': 'mapred.job.queue.name',
     '2.0': 'mapreduce.job.queuename'},
    {'1.0': 'mapred.job.reduce.input.buffer.percent',
     '2.0': 'mapreduce.reduce.input.buffer.percent'},
    {'1.0': 'mapred.job.reduce.markreset.buffer.percent',
     '2.0': 'mapreduce.reduce.markreset.buffer.percent'},
    {'1.0': 'mapred.job.reduce.memory.mb',
     '2.0': 'mapreduce.reduce.memory.mb'},
    {'1.0': 'mapred.job.reduce.total.mem.bytes',
     '2.0': 'mapreduce.reduce.memory.totalbytes'},
    {'1.0': 'mapred.job.reuse.jvm.num.tasks',
     '2.0': 'mapreduce.job.jvm.numtasks'},
    {'1.0': 'mapred.job.shuffle.input.buffer.percent',
     '2.0': 'mapreduce.reduce.shuffle.input.buffer.percent'},
    {'1.0': 'mapred.job.shuffle.merge.percent',
     '2.0': 'mapreduce.reduce.shuffle.merge.percent'},
    {'1.0': 'mapred.job.tracker',
     '2.0': 'mapreduce.jobtracker.address'},
    {'1.0': 'mapred.job.tracker.handler.count',
     '2.0': 'mapreduce.jobtracker.handler.count'},
    {'1.0': 'mapred.job.tracker.history.completed.location',
     '2.0': 'mapreduce.jobtracker.jobhistory.completed.location'},
    {'1.0': 'mapred.job.tracker.http.address',
     '2.0': 'mapreduce.jobtracker.http.address'},
    {'1.0': 'mapred.job.tracker.jobhistory.lru.cache.size',
     '2.0': 'mapreduce.jobtracker.jobhistory.lru.cache.size'},
    {'1.0': 'mapred.job.tracker.persist.jobstatus.active',
     '2.0': 'mapreduce.jobtracker.persist.jobstatus.active'},
    {'1.0': 'mapred.job.tracker.persist.jobstatus.dir',
     '2.0': 'mapreduce.jobtracker.persist.jobstatus.dir'},
    {'1.0': 'mapred.job.tracker.persist.jobstatus.hours',
     '2.0': 'mapreduce.jobtracker.persist.jobstatus.hours'},
    {'1.0': 'mapred.job.tracker.retire.jobs',
     '2.0': 'mapreduce.jobtracker.retirejobs'},
    {'1.0': 'mapred.job.tracker.retiredjobs.cache.size',
     '2.0': 'mapreduce.jobtracker.retiredjobs.cache.size'},
    {'1.0': 'mapred.jobinit.threads',
     '2.0': 'mapreduce.jobtracker.jobinit.threads'},
    {'1.0': 'mapred.jobtracker.instrumentation',
     '2.0': 'mapreduce.jobtracker.instrumentation'},
    {'1.0': 'mapred.jobtracker.job.history.block.size',
     '2.0': 'mapreduce.jobtracker.jobhistory.block.size'},
    {'1.0': 'mapred.jobtracker.maxtasks.per.job',
     '2.0': 'mapreduce.jobtracker.maxtasks.perjob'},
    {'1.0': 'mapred.jobtracker.restart.recover',
     '2.0': 'mapreduce.jobtracker.restart.recover'},
    {'1.0': 'mapred.jobtracker.taskScheduler',
     '2.0': 'mapreduce.jobtracker.taskscheduler'},
    {'1.0': 'mapred.jobtracker.taskScheduler.maxRunningTasksPerJob',
     '2.0': 'mapreduce.jobtracker.taskscheduler.maxrunningtasks.perjob'},
    {'1.0': 'mapred.jobtracker.taskalloc.capacitypad',
     '2.0': 'mapreduce.jobtracker.taskscheduler.taskalloc.capacitypad'},
    {'1.0': 'mapred.join.expr',
     '2.0': 'mapreduce.join.expr'},
    {'1.0': 'mapred.join.keycomparator',
     '2.0': 'mapreduce.join.keycomparator'},
    {'1.0': 'mapred.lazy.output.format',
     '2.0': 'mapreduce.output.lazyoutputformat.outputformat'},
    {'1.0': 'mapred.line.input.format.linespermap',
     '2.0': 'mapreduce.input.lineinputformat.linespermap'},
    {'1.0': 'mapred.linerecordreader.maxlength',
     '2.0': 'mapreduce.input.linerecordreader.line.maxlength'},
    {'1.0': 'mapred.local.dir',
     '2.0': 'mapreduce.cluster.local.dir'},
    {'1.0': 'mapred.local.dir.minspacekill',
     '2.0': 'mapreduce.tasktracker.local.dir.minspacekill'},
    {'1.0': 'mapred.local.dir.minspacestart',
     '2.0': 'mapreduce.tasktracker.local.dir.minspacestart'},
    {'1.0': 'mapred.map.child.env',
     '2.0': 'mapreduce.map.env'},
    {'1.0': 'mapred.map.child.java.opts',
     '2.0': 'mapreduce.map.java.opts'},
    {'1.0': 'mapred.map.child.log.level',
     '2.0': 'mapreduce.map.log.level'},
    {'1.0': 'mapred.map.max.attempts',
     '2.0': 'mapreduce.map.maxattempts'},
    {'1.0': 'mapred.map.output.compression.codec',
     '2.0': 'mapreduce.map.output.compress.codec'},
    {'1.0': 'mapred.map.task.debug.script',
     '2.0': 'mapreduce.map.debug.script'},
    {'1.0': 'mapred.map.tasks',
     '2.0': 'mapreduce.job.maps'},
    {'1.0': 'mapred.map.tasks.speculative.execution',
     '2.0': 'mapreduce.map.speculative'},
    {'1.0': 'mapred.mapoutput.key.class',
     '2.0': 'mapreduce.map.output.key.class'},
    {'1.0': 'mapred.mapoutput.value.class',
     '2.0': 'mapreduce.map.output.value.class'},
    {'1.0': 'mapred.mapper.regex',
     '2.0': 'mapreduce.mapper.regex'},
    {'1.0': 'mapred.mapper.regex.group',
     '2.0': 'mapreduce.mapper.regexmapper..group'},
    {'1.0': 'mapred.max.map.failures.percent',
     '2.0': 'mapreduce.map.failures.maxpercent'},
    {'1.0': 'mapred.max.reduce.failures.percent',
     '2.0': 'mapreduce.reduce.failures.maxpercent'},
    {'1.0': 'mapred.max.split.size',
     '2.0': 'mapreduce.input.fileinputformat.split.maxsize'},
    {'1.0': 'mapred.max.tracker.blacklists',
     '2.0': 'mapreduce.jobtracker.tasktracker.maxblacklists'},
    {'1.0': 'mapred.max.tracker.failures',
     '2.0': 'mapreduce.job.maxtaskfailures.per.tracker'},
    {'1.0': 'mapred.merge.recordsBeforeProgress',
     '2.0': 'mapreduce.task.merge.progress.records'},
    {'1.0': 'mapred.min.split.size',
     '2.0': 'mapreduce.input.fileinputformat.split.minsize'},
    {'1.0': 'mapred.min.split.size.per.node',
     '2.0': 'mapreduce.input.fileinputformat.split.minsize.per.node'},
    {'1.0': 'mapred.min.split.size.per.rack',
     '2.0': 'mapreduce.input.fileinputformat.split.minsize.per.rack'},
    {'1.0': 'mapred.output.compress',
     '2.0': 'mapreduce.output.fileoutputformat.compress'},
    {'1.0': 'mapred.output.compression.codec',
     '2.0': 'mapreduce.output.fileoutputformat.compress.codec'},
    {'1.0': 'mapred.output.compression.type',
     '2.0': 'mapreduce.output.fileoutputformat.compress.type'},
    {'1.0': 'mapred.output.dir',
     '2.0': 'mapreduce.output.fileoutputformat.outputdir'},
    {'1.0': 'mapred.output.key.class',
     '2.0': 'mapreduce.job.output.key.class'},
    {'1.0': 'mapred.output.key.comparator.class',
     '2.0': 'mapreduce.job.output.key.comparator.class'},
    {'1.0': 'mapred.output.value.class',
     '2.0': 'mapreduce.job.output.value.class'},
    {'1.0': 'mapred.output.value.groupfn.class',
     '2.0': 'mapreduce.job.output.group.comparator.class'},
    {'1.0': 'mapred.permissions.supergroup',
     '2.0': 'mapreduce.cluster.permissions.supergroup'},
    {'1.0': 'mapred.pipes.user.inputformat',
     '2.0': 'mapreduce.pipes.inputformat'},
    {'1.0': 'mapred.reduce.child.env',
     '2.0': 'mapreduce.reduce.env'},
    {'1.0': 'mapred.reduce.child.java.opts',
     '2.0': 'mapreduce.reduce.java.opts'},
    {'1.0': 'mapred.reduce.child.log.level',
     '2.0': 'mapreduce.reduce.log.level'},
    {'1.0': 'mapred.reduce.max.attempts',
     '2.0': 'mapreduce.reduce.maxattempts'},
    {'1.0': 'mapred.reduce.parallel.copies',
     '2.0': 'mapreduce.reduce.shuffle.parallelcopies'},
    {'1.0': 'mapred.reduce.slowstart.completed.maps',
     '2.0': 'mapreduce.job.reduce.slowstart.completedmaps'},
    {'1.0': 'mapred.reduce.task.debug.script',
     '2.0': 'mapreduce.reduce.debug.script'},
    {'1.0': 'mapred.reduce.tasks',
     '2.0': 'mapreduce.job.reduces'},
    {'1.0': 'mapred.reduce.tasks.speculative.execution',
     '2.0': 'mapreduce.reduce.speculative'},
    {'1.0': 'mapred.seqbinary.output.key.class',
     '2.0': 'mapreduce.output.seqbinaryoutputformat.key.class'},
    {'1.0': 'mapred.seqbinary.output.value.class',
     '2.0': 'mapreduce.output.seqbinaryoutputformat.value.class'},
    {'1.0': 'mapred.shuffle.connect.timeout',
     '2.0': 'mapreduce.reduce.shuffle.connect.timeout'},
    {'1.0': 'mapred.shuffle.read.timeout',
     '2.0': 'mapreduce.reduce.shuffle.read.timeout'},
    {'1.0': 'mapred.skip.attempts.to.start.skipping',
     '2.0': 'mapreduce.task.skip.start.attempts'},
    {'1.0': 'mapred.skip.map.auto.incr.proc.count',
     '2.0': 'mapreduce.map.skip.proc-count.auto-incr'},
    {'1.0': 'mapred.skip.map.max.skip.records',
     '2.0': 'mapreduce.map.skip.maxrecords'},
    {'1.0': 'mapred.skip.on',
     '2.0': 'mapreduce.job.skiprecords'},
    {'1.0': 'mapred.skip.out.dir',
     '2.0': 'mapreduce.job.skip.outdir'},
    {'1.0': 'mapred.skip.reduce.auto.incr.proc.count',
     '2.0': 'mapreduce.reduce.skip.proc-count.auto-incr'},
    {'1.0': 'mapred.skip.reduce.max.skip.groups',
     '2.0': 'mapreduce.reduce.skip.maxgroups'},
    {'1.0': 'mapred.speculative.execution.slowNodeThreshold',
     '2.0': 'mapreduce.job.speculative.slownodethreshold'},
    {'1.0': 'mapred.speculative.execution.slowTaskThreshold',
     '2.0': 'mapreduce.job.speculative.slowtaskthreshold'},
    {'1.0': 'mapred.speculative.execution.speculativeCap',
     '2.0': 'mapreduce.job.speculative.speculativecap'},
    {'1.0': 'mapred.submit.replication',
     '2.0': 'mapreduce.client.submit.file.replication'},
    {'1.0': 'mapred.system.dir',
     '2.0': 'mapreduce.jobtracker.system.dir'},
    {'1.0': 'mapred.task.cache.levels',
     '2.0': 'mapreduce.jobtracker.taskcache.levels'},
    {'1.0': 'mapred.task.id',
     '2.0': 'mapreduce.task.attempt.id'},
    {'1.0': 'mapred.task.is.map',
     '2.0': 'mapreduce.task.ismap'},
    {'1.0': 'mapred.task.partition',
     '2.0': 'mapreduce.task.partition'},
    {'1.0': 'mapred.task.profile',
     '2.0': 'mapreduce.task.profile'},
    {'1.0': 'mapred.task.profile.maps',
     '2.0': 'mapreduce.task.profile.maps'},
    {'1.0': 'mapred.task.profile.params',
     '2.0': 'mapreduce.task.profile.params'},
    {'1.0': 'mapred.task.profile.reduces',
     '2.0': 'mapreduce.task.profile.reduces'},
    {'1.0': 'mapred.task.timeout',
     '2.0': 'mapreduce.task.timeout'},
    {'1.0': 'mapred.task.tracker.http.address',
     '2.0': 'mapreduce.tasktracker.http.address'},
    {'1.0': 'mapred.task.tracker.report.address',
     '2.0': 'mapreduce.tasktracker.report.address'},
    {'1.0': 'mapred.task.tracker.task-controller',
     '2.0': 'mapreduce.tasktracker.taskcontroller'},
    {'1.0': 'mapred.tasktracker.dns.interface',
     '2.0': 'mapreduce.tasktracker.dns.interface'},
    {'1.0': 'mapred.tasktracker.dns.nameserver',
     '2.0': 'mapreduce.tasktracker.dns.nameserver'},
    {'1.0': 'mapred.tasktracker.events.batchsize',
     '2.0': 'mapreduce.tasktracker.events.batchsize'},
    {'1.0': 'mapred.tasktracker.expiry.interval',
     '2.0': 'mapreduce.jobtracker.expire.trackers.interval'},
    {'1.0': 'mapred.tasktracker.indexcache.mb',
     '2.0': 'mapreduce.tasktracker.indexcache.mb'},
    {'1.0': 'mapred.tasktracker.instrumentation',
     '2.0': 'mapreduce.tasktracker.instrumentation'},
    {'1.0': 'mapred.tasktracker.map.tasks.maximum',
     '2.0': 'mapreduce.tasktracker.map.tasks.maximum'},
    {'1.0': 'mapred.tasktracker.memory_calculator_plugin',
     '2.0': 'mapreduce.tasktracker.resourcecalculatorplugin'},
    {'1.0': 'mapred.tasktracker.memorycalculatorplugin',
     '2.0': 'mapreduce.tasktracker.resourcecalculatorplugin'},
    {'1.0': 'mapred.tasktracker.reduce.tasks.maximum',
     '2.0': 'mapreduce.tasktracker.reduce.tasks.maximum'},
    {'1.0': 'mapred.tasktracker.taskmemorymanager.monitoring-interval',
     '2.0': 'mapreduce.tasktracker.taskmemorymanager.monitoringinterval'},
    {'1.0': 'mapred.tasktracker.tasks.sleeptime-before-sigkill',
     '2.0': 'mapreduce.tasktracker.tasks.sleeptimebeforesigkill'},
    {'1.0': 'mapred.temp.dir',
     '2.0': 'mapreduce.cluster.temp.dir'},
    {'1.0': 'mapred.text.key.comparator.options',
     '2.0': 'mapreduce.partition.keycomparator.options'},
    {'1.0': 'mapred.text.key.partitioner.options',
     '2.0': 'mapreduce.partition.keypartitioner.options'},
    {'1.0': 'mapred.textoutputformat.separator',
     '2.0': 'mapreduce.output.textoutputformat.separator'},
    {'1.0': 'mapred.tip.id',
     '2.0': 'mapreduce.task.id'},
    {'1.0': 'mapred.used.genericoptionsparser',
     '2.0': 'mapreduce.client.genericoptionsparser.used'},
    {'1.0': 'mapred.userlog.limit.kb',
     '2.0': 'mapreduce.task.userlog.limit.kb'},
    {'1.0': 'mapred.userlog.retain.hours',
     '2.0': 'mapreduce.job.userlog.retain.hours'},
    {'1.0': 'mapred.work.output.dir',
     '2.0': 'mapreduce.task.output.dir'},
    {'1.0': 'mapred.working.dir',
     '2.0': 'mapreduce.job.working.dir'},
    {'1.0': 'mapreduce.combine.class',
     '2.0': 'mapreduce.job.combine.class'},
    {'1.0': 'mapreduce.inputformat.class',
     '2.0': 'mapreduce.job.inputformat.class'},
    {'1.0': 'mapreduce.jobtracker.permissions.supergroup',
     '2.0': 'mapreduce.cluster.permissions.supergroup'},
    {'1.0': 'mapreduce.map.class',
     '2.0': 'mapreduce.job.map.class'},
    {'1.0': 'mapreduce.outputformat.class',
     '2.0': 'mapreduce.job.outputformat.class'},
    {'1.0': 'mapreduce.partitioner.class',
     '2.0': 'mapreduce.job.partitioner.class'},
    {'1.0': 'mapreduce.reduce.class',
     '2.0': 'mapreduce.job.reduce.class'},
    {'1.0': 'min.num.spills.for.combine',
     '2.0': 'mapreduce.map.combine.minspills'},
    {'1.0': 'reduce.output.key.value.fields.spec',
     '2.0': 'mapreduce.fieldsel.reduce.output.key.value.fields.spec'},
    {'1.0': 'security.job.submission.protocol.acl',
     '2.0': 'security.job.client.protocol.acl'},
    {'1.0': 'security.task.umbilical.protocol.acl',
     '2.0': 'security.job.task.protocol.acl'},
    {'1.0': 'sequencefile.filter.class',
     '2.0': 'mapreduce.input.sequencefileinputfilter.class'},
    {'1.0': 'sequencefile.filter.frequency',
     '2.0': 'mapreduce.input.sequencefileinputfilter.frequency'},
    {'1.0': 'sequencefile.filter.regex',
     '2.0': 'mapreduce.input.sequencefileinputfilter.regex'},
    {'1.0': 'session.id',
     '2.0': 'dfs.metrics.session-id'},
    {'1.0': 'slave.host.name',
     '2.0': 'dfs.datanode.hostname'},
    {'1.0': 'slave.host.name',
     '2.0': 'mapreduce.tasktracker.host.name'},
    {'1.0': 'tasktracker.contention.tracking',
     '2.0': 'mapreduce.tasktracker.contention.tracking'},
    {'1.0': 'tasktracker.http.threads',
     '2.0': 'mapreduce.tasktracker.http.threads'},
    {'1.0': 'topology.node.switch.mapping.impl',
     '2.0': 'net.topology.node.switch.mapping.impl'},
    {'1.0': 'topology.script.file.name',
     '2.0': 'net.topology.script.file.name'},
    {'1.0': 'topology.script.number.args',
     '2.0': 'net.topology.script.number.args'},
    {'1.0': 'user.name',
     '2.0': 'mapreduce.job.user.name'},
    {'1.0': 'webinterface.private.actions',
     '2.0': 'mapreduce.jobtracker.webinterface.trusted'},
]

# Handle compatibility for 0.x versions of Hadoop too
for jobconf_dict in _JOBCONF_DICT_LIST:
    jobconf_dict['0.20'] = jobconf_dict['1.0']
    jobconf_dict['0.21'] = jobconf_dict['2.0']


def _dict_list_to_compat_map(dict_list):
    # compat_map = {
    #   ...
    #   a: {'1.0': a, '2.0': b}
    #   ..
    # }
    compat_map = {}
    for version_dict in dict_list:
        for value in version_dict.values():
            compat_map[value] = version_dict
    return compat_map


_JOBCONF_MAP = _dict_list_to_compat_map(_JOBCONF_DICT_LIST)


def jobconf_from_env(variable, default=None):
    """Get the value of a jobconf variable from the runtime environment.

    For example, a :py:class:`~mrjob.job.MRJob` could use
    ``jobconf_from_env('map.input.file')`` to get the name of the file a
    mapper is reading input from.

    If the name of the jobconf variable is different in different versions of
    Hadoop (e.g. in Hadoop 2.0, ``map.input.file`` is
    ``mapreduce.map.input.file``), we'll automatically try all variants before
    giving up.

    Return *default* if that jobconf variable isn't set.
    """
    # try variable verbatim first
    name = variable.replace('.', '_')
    if name in os.environ:
        return os.environ[name]

    # try alternatives (arbitrary order)
    for var in _JOBCONF_MAP.get(variable, {}).values():
        name = var.replace('.', '_')
        if name in os.environ:
            return os.environ[name]

    return default


def jobconf_from_dict(jobconf, name, default=None):
    """Get the value of a jobconf variable from the given dictionary.

    :param dict jobconf: jobconf dictionary
    :param string name: name of the jobconf variable (e.g. ``'user.name'``)
    :param default: fallback value

    If the name of the jobconf variable is different in different versions of
    Hadoop (e.g. in Hadoop 2, ``map.input.file`` is
    ``mapreduce.map.input.file``), we'll automatically try all variants before
    giving up.

    Return *default* if that jobconf variable isn't set    """
    if name in jobconf:
        return jobconf[name]

    # try alternatives (arbitrary order)
    for alternative in _JOBCONF_MAP.get(name, {}).values():
        if alternative in jobconf:
            return jobconf[alternative]

    return default


def map_version(version, version_map):
    """Allows you to look up something by version (e.g. which jobconf variable
    to use, specifying only the versions where that value changed.

    *version* is a string

    *version_map* is a map from version (as a string) that a value changed
    to the new value.

    For efficiency, *version_map* can also be a list of tuples of
    ``(LooseVersion(version_as_string), value)``, with oldest versions first.

    If *version* is less than any version in *version_map*, use the value for
    the earliest version in *version_map*.
    """
    if version is None:
        raise TypeError

    if not version_map:
        raise ValueError

    if isinstance(version_map, dict):
        version_map = sorted((LooseVersion(k), v)
                             for k, v in version_map.items())

    req_version = LooseVersion(version)

    for min_version, value in reversed(version_map):
        if req_version >= min_version:
            return value
    else:
        return version_map[0][1]


def translate_jobconf(variable, version):
    """Translate *variable* to Hadoop version *version*. If it's not
    a variable we recognize, leave as-is.
    """
    if version is None:
        raise TypeError

    if variable in _JOBCONF_MAP:
        return map_version(version, _JOBCONF_MAP[variable])
    else:
        return variable


def translate_jobconf_for_all_versions(variable):
    """Get all known variants of the given jobconf variable.
    Unlike :py:func:`translate_jobconf`, returns a list."""
    return sorted(
        set([variable] + list(_JOBCONF_MAP.get(variable, {}).values())))


def translate_jobconf_dict(jobconf, hadoop_version=None):
    """Translates the configuration property name to match those that
    are accepted in hadoop_version. Prints a warning message if any
    configuration property name does not match the name in the hadoop
    version. Combines the original jobconf with the translated jobconf.

    :return: a map consisting of the original and translated configuration
             property names and values.
    """
    translated_jobconf = jobconf.copy()
    translation_warnings = {}

    for variable, value in jobconf.items():
        if hadoop_version:
            variants = [translate_jobconf(variable, hadoop_version)]
        else:
            variants = translate_jobconf_for_all_versions(variable)

        for variant in variants:
            if variant in jobconf:
                # this happens if variant == variable or
                # if the variant was in jobconf to start with
                continue

            translated_jobconf[variant] = value

            if hadoop_version:
                translation_warnings[variable] = variant

    if translation_warnings:
        log.warning("Detected hadoop configuration property names that"
                    " do not match hadoop version %s:"
                    "\nThe have been translated as follows\n %s",
                    hadoop_version,
                    '\n'.join([
                        "%s: %s" % (variable, variant) for variable, variant
                        in sorted(translation_warnings.items())]))

    return translated_jobconf


def uses_yarn(version):
    """Basically, is this Hadoop 2? This also handles versions in the
    zero series (0.23+) where YARN originated."""
    return (version_gte(version, '2') or
            version_gte(version, '0.23') and not version_gte(version, '1'))


def version_gte(version, cmp_version_str):
    """Return ``True`` if version >= *cmp_version_str*."""

    if not isinstance(version, string_types):
        raise TypeError('%r is not a string' % version)

    if not isinstance(cmp_version_str, string_types):
        raise TypeError('%r is not a string' % cmp_version_str)

    return LooseVersion(version) >= LooseVersion(cmp_version_str)
