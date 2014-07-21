---
title: Rolling Downgrades
project: riak
version: 1.4.8+
document: cookbook
toc: true
audience: advanced
keywords: [upgrading, downgrading]
---

Downgrades of Riak are tested and supported for two feature release versions, with the general procedure being similar to that of a [[rolling upgrade|Rolling Upgrades]].

You should perform the following actions on each node:

1.  Stop Riak
2.  Back up Riak's `etc` and `data` directories.
3.  Downgrade Riak
4.  Start Riak
5.  Verify Riak is running the downgraded version.
6.  Wait for the `riak_kv` service to start.

Depending on the versions involved in the downgrade, there are additional steps to be performed before, during, and after the upgrade on on each node.  These steps are related to changes or new features that are not present in the downgraded version.

<div class="note"><div class="title">A Note About the Following Instructions</div>
The below instructions describe the procedures required for a single feature release version downgrade. In a downgrade between two feature release versions, the steps for the in-between version must also be performed. For example, a downgrade from 1.4 to 1.2 requires that the downgrade steps for both 1.4 and 1.3 are performed.
</div>

## General Guidelines

- Riak Control should be disabled throughout the rolling downgrade process.
- The `app.config` and `vm.args` must be replaced with those of the version being downgraded to.
- Active Anti-Entropy should be disabled if downgrading to a version below 1.3.

## Before Stopping a Node

### Object Format

If the new, more compact object format introduced in Riak 1.4 is in use, the objects will need to be downgraded on each node prior to starting the rolling downgrade. You can determine which object format is in use by checking the `object_format` parameter under the `riak_kv` section of the `app.config`. If not specified, this defaults to `v0` which is the old format.

To downgrade the objects, run the below `riak-admin` command. This command must be run on each node.

```bash
riak-admin downgrade-objects <kill-handoffs> [<concurrency>]
```

The `<kill-handoffs>` parameter is required and is set to either `true` or `false`. If `false`, any ongoing handoff will be waited on before performing the reformat. Otherwise, all in-flight handoff, inbound to the node or outbound from it, will be killed. During and after the reformat the transfer-limit will be set to 0.

The optional `<concurrency>` argument must be an integer greater than zero. It determines how many partitions are reformatted on the node concurrently. By default the concurrency is two. Additionally, in anticipation that the entire cluster will be downgraded downgrade-objects sets the preferred format to v0. downgrade-objects can be run multiple times in the case of error or if the node crashes.

### Secondary Indexes

If you are using Secondary Indexes and have reformatted them with the `riak-admin reformat-indexes` command introduced in 1.3, these indexes will need to be downgraded before the rolling downgrade begins. 

This can be done using the --downgrade flag with `riak-admin reformat-indexes` More information on the `riak-admin reformat-indexes` command, and downgrading indexes can be found in the [[riak-admin|riak-admin Command Line#reformat-indexes]] documentation.

## Before Starting a Node

If LevelDB is in use and you are downgrading from 1.3, a change made to the LevelDB folder structure will need to be reverted. Prior to 1.3, each partition directory inside /var/lib/riak/leveldb contained the full set of .sst files that make up the LevelDB for that partition. Since 1.3, the levels have been separated into folders titled "sst_\*" like below:

```bash
cd 1004782375664995756265033322492444576013453623296/
ls -l 
-rw-r--r-- 1 riak riak        0 Jan  7 17:40 000014.log
-rw-r--r-- 1 riak riak       16 Jan  7 17:40 CURRENT
-rw-r--r-- 1 riak riak        0 Jan  7 13:59 LOCK
-rw-r--r-- 1 riak riak     1241 Jan  7 17:40 LOG
-rw-r--r-- 1 riak riak     1240 Jan  7 16:48 LOG.old
-rw-r--r-- 1 riak riak 20971520 Jan  7 17:40 MANIFEST-000013
drwxr-xr-x 2 riak riak     4096 Jan  7 14:25 sst_0
drwxr-xr-x 2 riak riak     4096 Jan  7 13:59 sst_1
drwxr-xr-x 2 riak riak     4096 Jan  7 13:59 sst_2
drwxr-xr-x 2 riak riak     4096 Jan  7 13:59 sst_3
drwxr-xr-x 2 riak riak     4096 Jan  7 13:59 sst_4
drwxr-xr-x 2 riak riak     4096 Jan  7 13:59 sst_5
drwxr-xr-x 2 riak riak     4096 Jan  7 13:59 sst_6
```

In a downgrade from 1.3, all .sst files in these folders will need to be moved from the "sst_\*" folders into the top level <PARTITION_ID> folder. A LevelDB repair will need to be run on each partition *before* starting the node. In the event a rolling downgrade needs to take place, the below escript, repair.erl can be used:

```erlang
-module(repair).
-compile(export_all).

main([Dir]) ->
  Opts = [{max_open_files, 2000},
            {use_bloomfilter, true},
            {write_buffer_size, 45 * 1024 * 1024},
            {compression,false}], 

  {Time,_} = timer:tc(eleveldb,repair,[Dir, Opts]),
  io:format("Done took ~p seconds~n", [Time / 1000000]);

main(_) ->
  usage().

usage() ->
  io:format("usage: repair PATH_TO_PARTITION \n"),
  halt(1).
```

This script, saved as repair.erl, can be called with:

```shell
for partition in $(ls /var/lib/riak/leveldb); do sudo riak escript /tmp/repair.erl /var/lib/riak/leveldb/$partition; done
```

## During the Rolling Downgrade

There is a known handoff issue that may occur when performing a rolling downgrade from 1.4. This is a result of the handoff data encoding cababilities not being negotiated correctly between nodes of mixed versions during a rolling downgrade.

If transfers are not progressing in the `riak-admin transfers` output, killing the Capability manager with the steps below on each node via `riak attach` may solve this issue if it occurs.

Check the currently used encoding (returns either `encode_raw` or `encode_zlib`):

```erlang
riak_core_capability:get({riak_kv, handoff_data_encoding}).
```

Kill the capability manager:

```erlang
exit(whereis(riak_core_capability), kill).
```

Check the encoding used after the capability manager restarts:

```erlang
riak_core_capability:get({riak_kv, handoff_data_encoding}).
```

### After the Rolling Downgrade

If Active Anti-Entropy was enabled, and the downgraded version is below 1.3, the anti-entropy directory on each node can be deleted after the rolling downgrade is complete.




## Rolling Upgrade to Enterprise

1. Back up your `etc` (`app.config` and `vm.args`) and `data` directories.
2. Shutdown the node you are going to upgrade.
3. Uninstall your Riak package.
4. Install the `riak_ee` package.
5. A standard package uninstall should not have removed your data directories. If it did, move your backup to where the data directory should be.
6. Copy any customizations from your backed-up `vm.args` to the `riak_ee` installed `vm.args` file, these files may be identical.
7. The `app.config` file from `riak_ee` will be significantly different from your backed-up file. While it will contain all of the same sections as your original, it will have many new ones. Copy the customizations from your original `app.config` file into the appropriate sections in the new one. Ensure that the following sections are present in `app.config`:
  * `riak_core` --- the `cluster_mgr` setting must be present. See [[MDC v3 Configuration|Multi Data Center Replication v3 Configuration]] for more information.
  * `riak_repl` --- See [[MDC v3 Configuration|Multi Data Center Replication v3 Configuration]] for more information.
  * `riak_jmx` --- See [[JMX Monitoring]] for more information.
  * `snmp` --- See [[SNMP]] for more information.
8. Start Riak on the upgraded node.

### Basho Patches

After upgrading to Enterprise, you should ensure that any custom patches contained in the
`basho-patches` directory are examined to determine their application to
the upgraded version. If you find that patches no longer apply to the upgraded
version, you should remove them from the `basho-patches` directory prior to
operating the node in production.

The following table lists locations of the `basho-patches` directory for each
supported operating system:

| OS | Directory
|----|-----------
| CentOS & RHEL Linux | `/usr/lib64/riak/lib/basho-patches`
| Debian & Ubuntu Linux | `/usr/lib/riak/lib/basho-patches`
| FreeBSD | `/usr/local/lib/riak/lib/basho-patches`
| SmartOS | `/opt/local/lib/riak/lib/basho-patches`
| Solaris 10 | `/opt/riak/lib/basho-patches`

### Riaknostic

It is a good idea to also verify some basic configuration and general health
of the Riak node after upgrading by using Riak's built-in diagnostic
utility *Riaknostic*.

Ensure that Riak is running on the node, and issue the following command:

```
riak-admin diag
```

Make the recommended changes from the command output to ensure optimal node
operation.
