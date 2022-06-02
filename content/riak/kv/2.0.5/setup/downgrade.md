---
title: "Downgrading"
description: ""
project: "riak_kv"
project_version: "2.0.5"
menu:
  riak_kv-2.0.5:
    name: "Downgrading"
    identifier: "downgrading"
    weight: 103
    parent: "setup_index"
toc: true
aliases:
  - /riak/2.0.5/ops/upgrading/rolling-downgrades/
  - /riak/kv/2.0.5/ops/upgrading/rolling-downgrades/
---

Downgrades of Riak are tested and supported for two feature release
versions, with the general procedure being similar to that of a
[rolling upgrade]({{<baseurl>}}riak/kv/2.0.5/setup/upgrading/cluster).

{{% note title="End Of Life Warning" %}}
We test downgrading for two feature release versions. However, all versions below KV 2.0 are End Of Life (EOL) and unsupported. Please be aware of that if you choose to downgrade.
{{% /note %}}

You should perform the following actions on each node:

1.  Stop Riak
2.  Back up Riak's `etc` and `data` directories.
3.  Downgrade Riak
4.  Start Riak
5.  Verify Riak is running the downgraded version.
6.  Wait for the `riak_kv` service to start.

Depending on the versions involved in the downgrade, there are
additional steps to be performed before, during, and after the upgrade
on on each node.  These steps are related to changes or new features
that are not present in the downgraded version.

{{% note title="A Note About the Following Instructions" %}}
The below instructions describe the procedures required for a single feature
release version downgrade. In a downgrade between two feature release
versions, the steps for the in-between version must also be performed. For
example, a downgrade from 1.4 to 1.2 requires that the downgrade steps for
both 1.4 and 1.3 are performed.
{{% /note %}}

## General Guidelines

* Riak Control should be disabled throughout the rolling downgrade
  process
* [Configuration Files]({{<baseurl>}}riak/kv/2.0.5/configuring/reference) must be replaced with those of the version
  being downgraded to
* [Active anti-entropy]({{<baseurl>}}riak/kv/2.0.5/learn/concepts/active-anti-entropy/) should be disabled if downgrading to a version
  below 1.3.

## Before Stopping a Node

### Object Format

If the new, more compact object format introduced in Riak 1.4 is in use,
the objects will need to be downgraded on each node prior to starting
the rolling downgrade. You can determine which object format is in use
by checking the `object_format` parameter under the `riak_kv` section of
the `app.config`. If not specified, this defaults to `v0` which is the
old format.

To downgrade the objects, run the below `riak-admin` command. This
command must be run on each node.

```bash
riak-admin downgrade-objects <kill-handoffs> [<concurrency>]
```

The `<kill-handoffs>` parameter is required and is set to either `true`
or `false`. If `false`, any ongoing handoff will be waited on before
performing the reformat. Otherwise, all in-flight handoff, inbound to
the node or outbound from it, will be killed. During and after the
reformat the transfer-limit will be set to 0.

The optional `<concurrency>` argument must be an integer greater than
zero. It determines how many partitions are reformatted on the node
concurrently. By default the concurrency is two. Additionally, in
anticipation that the entire cluster will be downgraded
downgrade-objects sets the preferred format to v0. downgrade-objects can
be run multiple times in the case of error or if the node crashes.

### Secondary Indexes

If you are using Secondary Indexes and have reformatted them with the
`riak-admin reformat-indexes` command introduced in 1.3, these indexes
will need to be downgraded before the rolling downgrade begins.

This can be done using the --downgrade flag with `riak-admin
reformat-indexes` More information on the `riak-admin reformat-indexes`
command, and downgrading indexes can be found in the
[`riak-admin`]({{<baseurl>}}riak/kv/2.0.5/using/admin/riak-admin/#reformat-indexes) documentation.

## Before Starting a Node

If LevelDB is in use and you are downgrading from 1.3, a change made to
the LevelDB folder structure will need to be reverted. Prior to 1.3,
each partition directory inside /var/lib/riak/leveldb contained the full
set of .sst files that make up the LevelDB for that partition. Since
1.3, the levels have been separated into folders titled `sst_\*` like
below:

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

In a downgrade from 1.3, all .sst files in these folders will need to be
moved from the `sst_\*` folders into the top level <PARTITION_ID>
folder. A LevelDB repair will need to be run on each partition *before*
starting the node. In the event a rolling downgrade needs to take place,
the below escript, repair.erl can be used:

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

```bash
for partition in $(ls /var/lib/riak/leveldb); do sudo riak escript /tmp/repair.erl /var/lib/riak/leveldb/$partition; done
```

## During the Rolling Downgrade

There is a known handoff issue that may occur when performing a rolling
downgrade from 1.4. This is a result of the handoff data encoding
cababilities not being negotiated correctly between nodes of mixed
versions during a rolling downgrade.

If transfers are not progressing in the `riak-admin transfers` output,
killing the Capability manager with the steps below on each node via
`riak attach` may solve this issue if it occurs.

Check the currently used encoding (returns either `encode_raw` or
`encode_zlib`):

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

If Active Anti-Entropy was enabled, and the downgraded version is below
1.3, the anti-entropy directory on each node can be deleted after the
rolling downgrade is complete.
