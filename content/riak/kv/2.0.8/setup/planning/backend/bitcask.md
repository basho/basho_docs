---
title: "Bitcask"
description: ""
project: "riak_kv"
project_version: "2.0.8"
menu:
  riak_kv-2.0.8:
    name: "Bitcask"
    identifier: "planning_backend_bitcask"
    weight: 100
    parent: "planning_choose_backend"
toc: true
aliases:
  - /riak/2.0.8/ops/advanced/backends/bitcask/
  - /riak/kv/2.0.8/ops/advanced/backends/bitcask/
---

[github bitcask]: https://github.com/basho/bitcask
[bitcask design pdf]: http://basho.com/assets/bitcask-intro.pdf
[use admin riak cli]: {{<baseurl>}}riak/kv/2.0.8/using/admin/riak-cli
[config reference]: {{<baseurl>}}riak/kv/2.0.8/configuring/reference
[glossary vnode]: {{<baseurl>}}riak/kv/2.0.8/learn/glossary/#vnode
[learn clusters]: {{<baseurl>}}riak/kv/2.0.8/learn/concepts/clusters
[plan backend multi]: {{<baseurl>}}riak/kv/2.0.8/setup/planning/backend/multi
[usage search]: {{<baseurl>}}riak/kv/2.0.8/developing/usage/search

[glossary aae]: {{<baseurl>}}riak/kv/2.0.8/learn/glossary/#active-anti-entropy-aae
[perf open files]: {{<baseurl>}}riak/kv/2.0.8/using/performance/open-files-limit

[plan bitcask capacity]: {{<baseurl>}}riak/kv/2.0.8/setup/planning/bitcask-capacity-calc
[usage delete objects]: {{<baseurl>}}riak/kv/2.0.8/developing/usage/deleting-objects

[Bitcask][github bitcask] is an Erlang application that provides an API for storing and retrieving key/value data using log-structured hash tables that provide very fast access. The [design][bitcask design pdf] of Bitcask was inspired, in part, by log-structured filesystems and log file merging.

## Bitcask's Strengths

* **Low latency per item read or written**

    This is due to the write-once, append-only nature of Bitcask
    database files.

* **High throughput, especially when writing an incoming stream of
    random items**

    Write operations to Bitcask generally saturate I/O and disk
    bandwidth, which is a good thing from a performance perspective.
    This saturation occurs for two reasons: because (1) data that is
    written to Bitcask doesn't need to be ordered on disk, and (2) the
    log-structured design of Bitcask allows for minimal disk head
    movement during writes.

* **Ability to handle datasets larger than RAM without degradation**

    Access to data in Bitcask involves direct lookup from an in-memory
    hash table. This makes finding data very efficient, even when
    datasets are very large.

* **Single seek to retrieve any value**

    Bitcask's in-memory hash table of keys points directly to locations
    on disk where the data lives. Bitcask never uses more than one disk
    seek to read a value and sometimes even that isn't necessary due to
    filesystem caching done by the operating system.

* **Predictable lookup _and_ insert performance**

    For the reasons listed above, read operations from Bitcask have
    fixed, predictable behavior. This is also true of writes to Bitcask
    because write operations require, at most, one seek to the end of
    the current open file followed by and append to that file.

* **Fast, bounded crash recovery**

    Crash recovery is easy and fast with Bitcask because Bitcask files
    are append only and write once. The only items that may be lost are
    partially written records at the tail of the last file that was
    opened for writes. Recovery operations need to review only the last
    record or two written and verify CRC data to ensure that the data is
    consistent.

* **Easy Backup**

    In most systems, backup can be very complicated. Bitcask simplifies
    this process due to its append-only, write-once disk format. Any
    utility that archives or copies files in disk-block order will
    properly back up or copy a Bitcask database.

## Weaknesses

* Keys must fit in memory

    Bitcask keeps all keys in memory at all times, which means that your
    system must have enough memory to contain your entire keyspace, plus
    additional space for other operational components and operating-
    system-resident filesystem buffer space.

## Installing Bitcask

Bitcask is the default storage engine for Riak. You can verify that
Bitcask is currently being used as the storage backend with the
[`riak`][use admin riak cli] command interface:

```bash
riak config effective | grep backend
```

If this operation returns anything other than `bitcask`, read
the following section for instructions on switching the backend to Bitcask.

## Enabling Bitcask

You can set Bitcask as the storage engine using each node's
[configuration files][config reference]:

```riakconf
storage_backend = bitcask
```

```appconfig
{riak_kv, [
  {storage_backend, riak_kv_bitcask_backend},
  %% Other riak_kv settings...

    ]},
```

## Configuring Bitcask

Bitcask enables you to configure a wide variety of its behaviors, from
filesystem sync strategy to merge settings and more.

> **Note on configuration systems**
>
> Riak 2.0 enables you to use either the newer [configuration system][config reference] based on a single `riak.conf` file or the older system, based on an `app.config` configuration file.
> Instructions for both systems will be included below. Narrative
descriptions of the various settings will be tailored to the newer
configuration system, whereas instructions for the older system will
largely be contained in the code tabs.

The default configuration values for Bitcask are as follows:

```riakconf
bitcask.data_root = ./data/bitcask
bitcask.io_mode = erlang
```

```appconfig
{bitcask, [
    {data_root, "/var/lib/riak/bitcask"},
    {io_mode, erlang},

    %% Other Bitcask-specific settings
    ]}
```

All of the other available settings listed below can be added to your
configuration files.

### Open Timeout

The open timeout setting specifies the maximum time Bitcask will block
on startup while attempting to create or open the Bitcask data
directory. The default is 4 seconds.

In general, you will not need to adjust this setting. If, however, you
begin to receive log messages of the form `Failed to start bitcask
backend: ...`, you may want to consider using a longer timeout.

Open timeout is specified using the `bitcask.sync.open_timeout`
parameter, and can be set in terms of seconds, minutes, hours, etc.
The following example sets the parameter to 10 seconds:

```riakconf
bitcask.sync.open_timeout = 10s
```

```appconfig
{bitcask, [
    ...,
    {open_timeout, 10} %% This value must be expressed in seconds
    ...
    ]}
```

### Sync Strategy

Bitcask enables you to configure the durability of writes by specifying
when to synchronize data to disk, i.e. by choosing a sync strategy. The
default setting (`none`) writes data into operating system buffers that
will be written to disk when those buffers are flushed by the operating
system. If the system fails before those buffers are flushed, e.g. due
to power loss, that data is lost. This possibility holds for any
database in which values are asynchronously flushed to disk.

Thus, using the default setting of `none` protects against data loss in
the event of application failure, i.e. process death, but leaves open a
small window in which data could be lost in the event of a complete
system failure, e.g. hardware or OS failure.

This possibility can be prevented by choosing the `o_sync` sync
strategy, which forces the operating system to flush to stable storage
at write time for every write. The effect of flushing each write is
better durability, although it should be noted that write throughput
will suffer because each write will have to wait for the write to
complete.

The following sync strategies are available:

  * `none` --- lets the operating system manage syncing writes
    (default)
  * `o_sync` --- uses the `O_SYNC` flag, which forces syncs on every
    write
  * Time interval --- Riak will force Bitcask to sync at specified
    intervals

The following are possible configurations:


```riakconf
bitcask.sync.strategy = none
bitcask.sync.strategy = o_sync

bitcask.sync.strategy = interval
bitcask.sync.interval = 65s
```

```appconfig
{bitcask, [
    ...,
        {sync_strategy, none},
        {sync_strategy, o_sync},
        {sync_strategy, {seconds, 10}}, %% The time interval must be specified in seconds
    ...
    ]}
```

> **Sync strategy interval limitations**
>
> Setting the sync interval to a value lower or equal to
  `riak_core.vnode_inactivity_timeout` (default: 60 seconds), will
  prevent Riak from performing handoffs.
>
> A vnode must be inactive (not receive any messages) for a certain amount of time before the handoff process can start. The sync mechanism causes a message to be sent to the vnode for every sync, thus preventing the vnode from ever becoming inactive.

### Max File Size

The `max_file_size` setting describes the maximum permitted size for any
single data file in the Bitcask directory. If a write causes the current
file to exceed this size threshold then that file is closed, and a new
file is opened for writes. The default is 2 GB.

Increasing `max_file_size` will cause Bitcask to create fewer, larger
files that are merged less frequently, while decreasing it will cause
Bitcask to create more numerous, smaller files that are merged more
frequently.

To give an example, if your ring size is 16, your servers could see as
much as 32 GB of data in the bitcask directories before the first merge
is triggered, irrespective of your working set size. You should plan
storage accordingly and be aware that it is possible to see disk data
sizes that are larger than the working set.

The `max_file_size` setting can be specified using kilobytes, megabytes,
etc. The following example sets the max file size to 1 GB:

```riakconf
bitcask.max_file_size = 1GB
```

```appconfig
%% The max_file_size setting must be expressed in bytes, as in the
%% example below

{bitcask, [
    ...,
    {max_file_size, 16#40000000}, %% 1 GB expressed in bytes
    ...
    ]}
```

### Hint File CRC Check

During startup, Bitcask will read from `.hint` files in order to build
its in-memory representation of the key space, falling back to `.data`
files if necessary. This reduces the amount of data that must be read
from the disk during startup, thereby also reducing the time required to
start up. You can configure Bitcask to either disregard `.hint` files
that don't contain a CRC value or to use them anyway.

If you are using the newer, `riak.conf`-based configuration system, you
can instruct Bitcask to disregard `.hint` files that do not contain a
CRC value by setting the `hintfile_checksums` setting to `strict` (the
default). To use Bitcask in a backward-compatible mode that allows for
`.hint` files without CRC signatures, change the setting to
`allow_missing`.

The following example sets the parameter to `strict`:

```riakconf
bitcask.hintfile_checksums = strict
```

```appconfig
%% In the app.config-based system, substitute "require_hint_crc" for
%% "hintfile_checksums", "true" for "strict", and "false" for
%% "allow_missing"

{bitcask, [
    ...,
    {require_hint_crc, true},
    ...
    ]}
```

### I/O Mode

The `io_mode` setting specifies which code module Bitcask should use for 
file access. The available settings are:

* `erlang` (default) --- Writes are made via Erlang's built-in file API
* `nif` --- Writes are made via direct calls to the POSIX C API

The following example sets `io_mode` to `erlang`:

```riakconf
bitcask.io_mode = erlang
```

```appconfig
{bitcask, [
    ...,
    {io_mode, erlang},
    ...
    ]}
```

In general, the `nif` IO mode provides higher throughput for certain
workloads, but it has the potential to negatively impact the Erlang VM,
leading to higher worst-case latencies and possible throughput collapse.

### `O_SYNC` on Linux

Synchronous file I/O via
[`o_sync`](http://linux.about.com/od/commands/l/blcmdl2_open.htm) is
supported in Bitcask if `io_mode` is set to `nif` and is not supported
in the `erlang` mode.

If you enable `o_sync` by setting `io_mode` to `nif`, however, you will
still get an incorrect warning along the following lines:

```log
[warning] <0.445.0>@riak_kv_bitcask_backend:check_fcntl:429 {sync_strategy,o_sync} not implemented on Linux
```

If you are using the older, `app.config`-based configuration system, you
can disable the check that generates this warning by adding the
following to the `riak_kv` section of your `app.config`:

```appconfig
{riak_kv, [
    ...,
    {o_sync_warning_logged, false},
    ...
    ]}
```

### Disk Usage and Merging Settings

Riak KV stores each [vnode][glossary vnode] of the
[ring][learn clusters] as a separate Bitcask directory within the
configured Bitcask data directory.

Each of these directories will contain multiple files with key/value
data, one or more "hint" files that record where the various keys exist
within the data files, and a write lock file. The design of Bitcask
allows for recovery even when data isn't fully synchronized to disk
(partial writes). This is accomplished by maintaining data files that
are append-only (i.e. never modified in-place) and are never reopened
for modification (i.e. they are only for reading).

This data management strategy trades disk space for operational
efficiency. There can be a significant storage overhead that is
unrelated to your working data set but can be tuned in a way that best
fits your use case. In short, disk space is used until a threshold is
met at which point unused space is reclaimed through a process of
merging. The merge process traverses data files and reclaims space by
eliminating out-of-date of deleted key/value pairs, writing only the
current key/value pairs to a new set of files within the directory.

The merge process is affected by all of the settings described in the
sections below. In those sections, "dead" refers to keys that no longer
contain the most up-to-date values, while "live" refers to keys that do
contain the most up-to-date value and have not been deleted.

### Merge Policy

Bitcask enables you to select a merge policy, i.e. when during the day
merge operations are allowed to be triggered. The valid options are:

* `always` --- No restrictions on when merge operations can occur
  (default)
* `never` --- Merge will never be attempted
* `window` --- Merge operations occur during specified hours

If you are using the newer, `riak.conf`-based configuration system, you
can select a merge policy using the `merge.policy` setting. The
following example sets the merge policy to `never`:

```riakconf
bitcask.merge.policy = never
```

```appconfig
{bitcask, [
    ...,
    {merge_window, never},
    ...
    ]}
```

If you opt to specify start and end hours for merge operations, you can
do so with the `merge.window.start` and `merge.window.end`
settings in addition to setting the merge policy to `window`.
Each setting is an integer between 0 and 23 for hours on a 24h clock,
with 0 meaning midnight and 23 standing for 11 pm.
The merge window runs from the first minute of the `merge.window.start` hour
to the last minute of the `merge.window.end` hour.
The following example enables merging between 3 am and 4:59 pm:

```riakconf
bitcask.merge.policy = window
bitcask.merge.window.start = 3
bitcask.merge.window.end = 17
```

```appconfig
%% In the app.config-based system, you specify the merge window using
%% a tuple, as in the following example:

{bitcask, [
    ...,
    {merge_window, {3, 17}},
    ...
    ]}
```

> **`merge_window` and the Multi backend**
>
>If you are using the older configuration system and using Bitcask with
the [Multi][plan backend multi] backend, please note that if you
wish to use a merge window, you _must_ set it in the global `bitcask`
section of your configuration file. `merge_window` settings
in per-backend sections are ignored.

If merging has a significant impact on performance of your cluster, or
if your cluster has quiet periods in which little storage activity
occurs, you may want to change this setting from the default.

A common way to limit the impact of merging is to create separate merge
windows  for each node in the cluster and ensure that these windows do
not overlap. This ensures that at most one node at a time can be
affected by merging, leaving the remaining nodes to handle requests.
The main drawback of this approach is that merges will occur less
frequently, leading to increased disk space usage.

### Merge Triggers

Merge triggers determine the conditions under which merging will be
invoked. These conditions fall into two basic categories:

* **Fragmentation** --- This describes the ratio of dead keys to total
  keys in a file that will trigger merging. The value of this setting is
  an integer percentage (0-100). For example, if a data file contains 6
  dead keys and 4 live keys, a merge will be triggered by the default
  setting (60%). Increasing this value will cause merging to occur less
  often, whereas decreasing the value will cause merging to happen more
  often.

* **Dead Bytes** --- This setting describes how much data stored for
  dead keys in a single file will trigger merging. If a file meets or
  exceeds the trigger value for dead bytes, a merge will be triggered.
  Increasing the value will cause merging to occur less often, whereas
  decreasing the value will cause merging to happen more often. The
  default is 512 MB.

  When either of these constraints are met by any file in the directory,
  Bitcask will attempt to merge files.

You can set the triggers described above using
`merge.triggers.fragmentation` and `merge.triggers.dead_bytes`,
respectively. The former is expressed as an integer between 0 and 100,
whereas the latter can be expressed in terms of kilobytes, megabytes,
gigabytes, etc. The following example sets the dead bytes threshold to
55% and the fragmentation threshold to 1 GB:

```riakconf
bitcask.merge.triggers.fragmentation = 55
bitcask.merge.triggers.dead_bytes = 1GB
```

```appconfig
%% The equivalent settings in the app.config-based system are
%% frag_merge_trigger and dead_bytes_merge_trigger, respectively. The
%% latter must be expressed in bytes.

{bitcask, [
    ...,
    {frag_merge_trigger, 55},
    {dead_bytes_merge_trigger, 1073741824},
    ...
    ]}
```

### Merge Thresholds

Merge thresholds determine which files will be chosen for inclusion in
a merge operation.

* **Fragmentation** --- This setting describes which ratio of dead keys
  to total keys in a file will cause it to be included in the merge. The
  value of this setting is a percentage (0-100). For example, if a data
  file contains 4 dead keys and 6 live keys, it will be included in the
  merge at the default ratio (40%). Increasing the value will cause
  fewer files to be merged, while decreasing the value will cause more
  files to be merged.

* **Dead Bytes** --- This setting describes which ratio the minimum
  amount of data occupied by dead keys in a file to cause it to be
  included in the merge. Increasing this value will cause fewer files to
  be merged, while decreasing this value will cause more files to be
  merged. The default is 128 MB.

* **Small File** --- This setting describes the minimum size a file must
  be to be _excluded_ from the merge. Files smaller than the threshold
  will be included. Increasing the value will cause more files to be
  merged, while decreasing the value will case fewer files to be merged.
  The default is 10 MB.

You can set the thresholds described above using the
`merge.thresholds.fragmentation`, `merge.thresholds.dead_bytes`, and
`merge.threshold.small_file` settings, respectively.

The `fragmentation` setting is expressed as an integer
between 0 and 100, and the `dead_bytes` and `small_file` settings can be
expressed in terms of kilobytes, megabytes, gigabytes, etc. The
following example sets the fragmentation threshold to 45%, the
dead bytes threshold to 200 MB, and the small file threshold to 25 MB:

```riakconf
bitcask.merge.thresholds.fragmentation = 45
bitcask.merge.thresholds.dead_bytes = 200MB
bitcask.merge.thresholds.small_file = 25MB
```

```appconfig
%% In the app.config-based system, the settings corresponding to those
%% listed above are frag_threshold, dead_bytes_threshold, and
%% small_files threshold, respectively. The latter two settings must be
%% expressed in bytes:

{bitcask, [
    ...,
    {frag_threshold, 45},
    {dead_bytes_threshold, 209715200},
    {small_file_threshold, 26214400},
    ...
    ]}
```
> **Note on choosing threshold values**
>
> The values for the fragmentation and dead bytes thresholds _must be
equal to or less than their corresponding trigger values_. If they are
set higher, Bitcask will trigger merges in cases where no files meet the
threshold, which means that Bitcask will never resolve the conditions
that triggered merging in the first place.

### Merge Interval

Bitcask periodically runs checks to determine whether merges are
necessary. You can determine how often those checks take place using
the `bitcask.merge_check_interval` parameter. The default is 3 minutes.

```riakconf
bitcask.merge_check_interval = 3m
```

```appconfig
%% In the app.config-based system, this setting is expressed in
%% milliseconds and found in the riak_kv section rather than the bitcask
%% section:

{riak_kv, [
    %% Other configs

    {bitcask_merge_check_interval, 180000},

    %% Other configs
    ]}
```

If merge check operations happen at the same time on different
[vnodes][glossary vnode] on the same node, this can produce spikes
in I/O usage and undue latency. Bitcask makes it less likely that merge
check operations will occur at the same time on different vnodes by
applying a **jitter** to those operations. A jitter is a random
variation applied to merge times that you can alter using the
`bitcask.merge_check_jitter` parameter. This parameter is expressed as a
percentage of `bitcask.merge_check_interval`. The default is 30%.

```riakconf
bitcask.merge_check_jitter = 30%
```

```appconfig
%% In the app.config-based system, this setting is expressed as a float
%% and found in the riak_kv section rather than the bitcask section:

{riak_kv, [
    %% Other configs

    {bitcask_merge_check_jitter, 0.3},

    %% Other configs
    ]}
```

For example, if you set the merge check interval to 4 minutes and the
jitter to 25%, merge checks will occur at intervals between 3 and 5
minutes. With the default of 3 minutes and 30%, checks will occur at
intervals between roughly 2 and 4 minutes.

### Log Needs Merge

If you are using the older, `app.config`-based configuration system, you
can use the `log_needs_merge` setting to tune and troubleshoot Bitcask
merge settings. When set to `true` (as in the example below), each time
a merge trigger is met, the partition/vnode ID and mergeable files will
be logged.

```appconfig
{bitcask, [
    ...,
    {log_needs_merge, true},
    ...
    ]}
```

> **Note on `log_needs_merge` and the Multi backend**
>
>If you are using Bitcask with the [Multi][plan backend multi] backend in conjunction with the older, `app.config`-based configuration system, please
note that `log_needs_merge` _must_ be set in the global `bitcask` section of your `app.config`. All `log_needs_merge` settings in per-backend sections are ignored.

### Fold Keys Threshold

Fold keys thresholds will reuse the keydir (a) if another fold was
started less than a specified time interval ago and (b) there were fewer
than a specified number of updates. Otherwise, Bitcask will wait until
all current fold keys complete and then start. The default time interval
is 0, while the default number of updates is unlimited. Both thresholds
can be disabled.

The conditions described above can be set using the `fold.max_age` and
`fold.max_puts` parameters, respectively. The former can be expressed in
terms of minutes, hours, days, etc., while the latter is expressed as an
integer. Each threshold can be disabled by setting the value to
`unlimited`. The following example sets the `max_age` to 1/2 second and
the `max_puts` to 1000:

```riakconf
bitcask.max_age = 0.5s
bitcask.max_puts = 1000
```

```appconfig
%% In the app.config-based system, the corresponding parameters are
%% max_fold_age and max_fold_puts, respectively. The former must be
%% expressed in milliseconds, while the latter must be an integer:

{bitcask, [
    ...,
    {max_fold_age, 500},
    {max_fold_puts, 1000},
    ...
    ]}

%% Each of these thresholds can be disabled by setting the value to -1
```

<a name="Automatic-Expiration"></a>
### Automatic Expiration

By default, Bitcask keeps all of your data. But if your data has limited
time value or if you need to purge data for space reasons, you can
configure object expiration, aka expiry. This feature is disabled by
default.

You can enable and configure object expiry using the `expiry` setting
and either specifying a time interval in seconds, minutes, hours, etc.,
or turning expiry off (`off`). The following example configures objects
to expire after 1 day:

```riakconf
bitcask.expiry = 1d
```

```appconfig
%% In the app.config-based system, expiry is expressed in terms of
%% seconds:

{bitcask, [
    ...,
    {expiry_secs, 86400}, %% Sets the duration to 1 day
    ...
    ]}

%% Expiry can be turned off by setting this value to -1
```

> **Note on stale data**
>
> Space occupied by stale data _may not be reclaimed immediately_,
but the data will become immediately inaccessible to client requests.
Writing to a key will set a new modification timestamp on the value
and prevent it from being expired.

By default, Bitcask will trigger a merge whenever a data file contains
an expired key. This may result in excessive merging under some usage
patterns. You can prevent this by configuring an expiry grace time.
Bitcask will defer trigger a merge solely for key expiry by the
configured amount of time. The default is 0, signifying no grace time.

If you are using the newer, `riak.conf`-based configuration system, you
can set an expiry grace time using the `expiry.grace_time` setting and
in terms of minutes, hours, days, etc. The following example sets the
grace period to 1 hour:

```riakconf
bitcask.expiry.grace_time = 1h
```

```appconfig
%% The equivalent setting in the app.config-based system is
%% expiry_grace_time. This must be expressed in seconds:

{bitcask, [
    ...,
    {expiry_grace_time, 3600}, %% Sets the grace period to 1 hour
    ...
    ]}
```

#### Automatic expiration and Riak Search

If you are using [Riak Search][usage search] in conjunction with
Bitcask, please be aware that automatic expiry does not apply to [Search Indexes](../../../../developing/usage/search). If objects are indexed using Search,
those objects can be expired by Bitcask yet still registered in Search
indexes, which means that Search queries may return keys that no longer
exist. Riak's [active anti-entropy (AAE)][glossary aae] subsystem will eventually
catch this discrepancy, but this depends on AAE being enabled (which is
the default) and could take some time. If search queries returning
expired keys is a problem for your use case, then we would recommend not
using automatic expiration.

## Tuning Bitcask

When tuning your environment, there are a number of things to bear in
mind that can assist you in making Bitcask as stable and reliable as
possible and to minimize latency and maximize throughput.

### Tips & Tricks

  * **Bitcask depends on filesystem caches**

    Some data storage layers implement their own page/block buffer cache
    in-memory, but Bitcask does not. Instead, it depends on the
    filesystem's cache. Adjusting the caching characteristics of your
    filesystem can impact performance.

  * **Be aware of file handle limits**

    Review the documentation on [open files limit][perf open files].

  * **Avoid the overhead of updating file metadata (such as last access
    time) on every read or write operation**

    You can achieve a substantial speed boost by adding the `noatime`
    mounting option to Linux's `/etc/fstab`. This will disable the
    recording of the last accessed time for all files, which results
    in fewer disk head seeks. If you need last access times but you'd
    like some of the benefits of this optimization, you can try
    `relatime`.

    ```
    /dev/sda5    /data           ext3    noatime  1 1
    /dev/sdb1    /data/inno-log  ext3    noatime  1 2
    ```

  * **Small number of frequently changed keys**

    When keys are changed frequently, fragmentation rapidly increases.
    To counteract this, you should lower the fragmentation trigger and
    threshold.

  * **Limited disk space**

    When disk space is limited, limiting the space occupied by dead keys
    is of paramount importance. Lower the dead bytes threshold and
    trigger to counteract wasted space.

  * **Purging stale entries after a fixed period**

    To automatically purge stale values, set the object expiry value to
    the desired cutoff time. Keys that are not modified for a period
    equal to or greater than this time interval will become
    inaccessible.

  * **High number of partitions per node**

    Because each cluster has many partitions running, Bitcask will have
    many [open files][perf open files]. To reduce the number of open
    files, we suggest increasing the max file size so that larger files
    will be written. You could also decrease the fragmentation and
    dead-bytes settings and increase the small file threshold so that
    merging will keep the number of open files small in number.

  * **High daytime traffic, low nighttime traffic**

    In order to cope with a high volume of writes without performance
    degradation during the day, you might want to limit merging to
    in non-peak periods. Setting the merge window to hours of the day
    when traffic is low will help.

  * **Multi-cluster replication (Riak Enterprise)**

    If you are using [Riak Enterprise](http://basho.com/riak-enterprise/)
    with the replication feature enabled, your clusters might experience
    higher production of fragmentation and dead bytes. Additionally,
    because the fullsync feature operates across entire partitions, it
    will be made more efficient by accessing data as sequentially as
    possible (across fewer files). Lowering both the fragmentation and
    dead-bytes settings will improve performance.

## FAQ

  * [[Why does it seem that Bitcask merging is only triggered when a
    Riak node is restarted?|Developing on Riak
    FAQs#why-does-it-seem-that-bitc]]
  * [[If the size of key index exceeds the amount of memory, how does
    Bitcask handle it?|Operating Riak FAQs#if-the-size-of-key-index-e]]
  * [Bitcask Capacity Planning][plan bitcask capacity]

## Bitcask Implementation Details

Riak will create a Bitcask database directory for each [vnode][glossary vnode]
in a [cluster][learn clusters]. In each of those directories, at most one
database file will be open for writing at any given time. The file being
written to will grow until it exceeds a specified size threshold, at
which time it is closed and a new file is created for additional writes.
Once a file is closed, whether purposely or due to server exit, it is
considered immutable and will never again be opened for writing.

The file currently open for writes is only written by appending, which
means that sequential writes do not require disk seeking, which can
dramatically speed up disk I/O. Note that this effect can be hampered if
you have `atime` enabled on your filesystem, because the disk head will
have to move to update both the data blocks _and_ the file and directory
metadata blocks. The primary speed advantage from a log-based database
stems of its ability to minimize disk head seeks.

Deleting a value from Bitcask is a two-step process: first, a
[tombstone][usage delete objects] is recorded in the open file for writes,
which indicates that a value was marked for deletion at that time, while
references to that key are removed from the in-memory "keydir"
information; later, during a merge operation, non-active data files are
scanned, and only  those values without tombstones are merged into the
active data file. This effectively removes the obsolete data and
reclaims disk space associated with it. This data management strategy
may use up a lot of space over time, since Bitcask writes new values
without touching the old ones.

The compaction process referred to as "merging" solves this
problem. The merge process iterates over all non-active (i.e. immutable)
files in a Bitcask database and produces as output a set of data files
containing only the "live" or latest versions of each present key.

### Bitcask Database Files

Below are two directory listings showing what you should expect to find
on disk when using Bitcask. In this example, we use a 64-partition
[ring][learn clusters], which results in 64 separate directories,
each holding its own Bitcask database.

```bash
ls ./data/bitcask
```

The result:

```
0
1004782375664995756265033322492444576013453623296
1027618338748291114361965898003636498195577569280

... etc ...

981946412581700398168100746981252653831329677312
```

Note that when starting up the directories are created for each
[vnode][glossary vnode] partition's data. At this point, however, there are not
yet any Bitcask-specific files.

After performing one PUT (write) into the Riak cluster running Bitcask:

```bash
curl -XPUT http://localhost:8098/types/default/buckets/test/keys/test \
  -H "Content-Type: text/plain" \
  -d "hello"
```

The "N" value for this cluster is 3 (the default), so you'll see that
the three vnode partitions responsible for this data now have Bitcask
database files:

```
bitcask/

... etc ...

|-- 1118962191081472546749696200048404186924073353216-1316787078245894
|   |-- 1316787252.bitcask.data
|   |-- 1316787252.bitcask.hint
|   `-- bitcask.write.lock

... etc ...


|-- 1141798154164767904846628775559596109106197299200-1316787078249065
|   |-- 1316787252.bitcask.data
|   |-- 1316787252.bitcask.hint
|   `-- bitcask.write.lock

... etc ...


|-- 1164634117248063262943561351070788031288321245184-1316787078254833
|   |-- 1316787252.bitcask.data
|   |-- 1316787252.bitcask.hint
|   `-- bitcask.write.lock

... etc ...

```

As more data is written to the cluster, more Bitcask files are created
until merges are triggered.

```
bitcask/
|-- 0-1317147619996589
|   |-- 1317147974.bitcask.data
|   |-- 1317147974.bitcask.hint
|   |-- 1317221578.bitcask.data
|   |-- 1317221578.bitcask.hint
|   |-- 1317221869.bitcask.data
|   |-- 1317221869.bitcask.hint
|   |-- 1317222847.bitcask.data
|   |-- 1317222847.bitcask.hint
|   |-- 1317222868.bitcask.data
|   |-- 1317222868.bitcask.hint
|   |-- 1317223014.bitcask.data
|   `-- 1317223014.bitcask.hint
|-- 1004782375664995756265033322492444576013453623296-1317147628760580
|   |-- 1317147693.bitcask.data
|   |-- 1317147693.bitcask.hint
|   |-- 1317222035.bitcask.data
|   |-- 1317222035.bitcask.hint
|   |-- 1317222514.bitcask.data
|   |-- 1317222514.bitcask.hint
|   |-- 1317223035.bitcask.data
|   |-- 1317223035.bitcask.hint
|   |-- 1317223411.bitcask.data
|   `-- 1317223411.bitcask.hint
|-- 1027618338748291114361965898003636498195577569280-1317223690337865
|-- 1050454301831586472458898473514828420377701515264-1317223690151365

... etc ...

```

This is normal operational behavior for Bitcask.
