---
title: Bitcask
project: riak
version: 0.10.0+
document: tutorials
toc: true
audience: intermediate
keywords: [backends, planning, bitcask]
prev: "[[Choosing a Backend]]"
up:   "[[Choosing a Backend]]"
next: "[[LevelDB]]"
interest: false
moved: {
    '1.4.0-': '/tutorials/choosing-a-backend/Bitcask'
}
---

## Overview

[Bitcask](https://github.com/basho/bitcask) is an Erlang application that
provides an API for storing and retrieving key/value data into a log-structured
hash table that provides very fast access. The
[design](http://downloads.basho.com/papers/bitcask-intro.pdf) owes a lot to the
principles found in log-structured file systems and draws inspiration from a
number of designs that involve log file merging.

### Strengths:

  * Low latency per item read or written

    This is due to the write-once, append-only nature of the Bitcask database
    files.

  * High throughput, especially when writing an incoming stream of random items

    Because the data being written doesn't need to be ordered on disk and
    because the log structured design allows for minimal disk head movement
    during writes these operations generally saturate the I/O and disk
    bandwidth.

  * Ability to handle datasets larger than RAM w/o degradation

    Because access to data in Bitcask is direct lookup from an in-memory hash
    table finding data on disk is very effecient, even when data sets are very
    large.

  * Single Seek to Retrieve Any Value

    Bitcask's in-memory hash-table of keys point directly to locations on disk
    where the data lives. Bitcask never uses more than one disk seek to read a
    value and sometimes, due to file-system caching done by the operating
    system, even that isn't necessary.

  * Predictable Lookup _and_ Insert Performance

    As you might expect from the description above, read operations have a
    fixed, predictable behavior. What you might not expect is that this is
    also true for writes. Write operations are at most a seek to the end of
    the current file open writing and an append to that file.

  * Fast, bounded Crash Recovery

    Due to the append-only write once nature of Bitcask files, recovery is easy
    and fast. The only items that might be lost are partially written records
    at the tail of the file last opened for writes. Recovery need only review
    the last record or two written and verify CRC data to ensure that the data
    is consistent.

  * Easy Backup

    In most systems backup can be very complicated but here again Bitcask
    simplifies this process due to it's append-only write once disk format.
    Any utility that archives or copies files in disk-block order will properly
    backup or copy a Bitcask database.

### Weaknesses:

  * Keys Must Fit In Memory

    Bitcask keeps all keys in memory at all times, this means that your system
    must have enough memory to contain your entire keyspace with room for other
    operational components and operating system resident filesystem buffer
    space.

## Installing Bitcask

Riak ships with Bitcask included within the distribution. In fact it is the
default storage engine and no separate installation is required.

The default configuration values found in the `app.config` for Bitcask are as
follows:

```erlang
 %% Bitcask Config
 {bitcask, [
             {data_root, "/var/lib/riak/bitcask"}
           ]},
```

## Configuring Bitcask

Modify the default behavior by adding these settings into the `bitcask` section
in your [[app.config|Configuration Files]].

### Open Timeout

  The `open_timeout` setting specifies the maximum time Bitcask will block on
  startup while attempting to create or open the data directory. The value is
  in seconds and the default is `4`. You generally need not change this value.
  If for some reason the timeout is exceeded on open you'll see a log message
  of the form: `"Failed to start bitcask backend: ...`. Only then should you
  consider a longer timeout.

```erlang
{bitcask, [
        ...,
            {open_timeout, 4} %% Wait time to open a keydir (in seconds)
        ...
]}
```

### Sync Strategy

  The `sync_strategy` setting changes the durability of writes by specifying
  when to synchronize data to disk. The default setting protects against data
  loss in the event of application failure (process death) but leaves open a
  small window wherein data could be lost in the event of complete system
  failure (e.g. hardware, O/S, power).

  The default mode, `none`, writes data into operating system buffers which
  which will be written to the disks when those buffers are flushed by the
  operating system. If the system fails (power loss, crash, etc.) before
  before those buffers are flushed to stable storage that data is lost.

  This is prevented by the setting `o_sync` which forces the operating system
  to flush to stable storage at every write. The effect of flushing each
  write is better durability, however write throughput will suffer as each
  write will have to wait for the write to complete.

  ___Available Sync Strategies___

  * `none` - (default) Lets the operating system manage syncing writes.
  * `o_sync` - Uses the O_SYNC flag which forces syncs on every write.
  * `{seconds, N}` - Riak will force Bitcask to sync every `N` seconds.

```erlang
{bitcask, [
        ...,
            {sync_strategy, none}, %% Let the O/S decide when to flush to disk
        ...
]}
```

<div class="note"><div class="title">Bitcask doesn't actually set O_SYNC on
Linux</div><p>At the time of this writing, due to an unresolved Linux <a
href="http://permalink.gmane.org/gmane.linux.kernel/1123952">kernel issue</a>
related to the <a
href="https://github.com/torvalds/linux/blob/master/fs/fcntl.c#L146..L198">implementation
of <code>fcntl</code></a> it turns out that Bitcask will not set the
<code>O_SYNC</code> flag on the file opened for writing, the call to
<code>fcntl</code> doesn't fail, it is silently ignored by the Linux kernel.
You will notice a <a
href="https://github.com/basho/riak_kv/commit/6a29591ecd9da73e27223a1a55acd80c21d4d17f#src/riak_kv_bitcask_backend.erl">warning
message</a> in the log files of the format:<br /><code>{sync_strategy,o_sync} not
implemented on Linux</code><br /> indicating that this issue exists on your system.
Without the <code>O_SYNC</code> setting enabled there is potential for data
loss if the OS or system dies (power outtage, kernel panic, reboot without a
sync) with dirty buffers not yet written to stable storage.</div>

### Disk-Usage and Merging Settings

Riak K/V stores each vnode partition of the ring as a separate Bitcask
directory within the configured bitcask data directory. Each of these
directories will contain multiple files with key/value data, one or more
"hint" files that record where the various keys exist within the data files,
and a write lock file. The design of Bitcask allows for recovery even when
data isn't fully synchronized to disk (partial writes). This is accomplished
by maintaining data files that are append-only (never modified in-place) and
are never reopened for modification (only reading).

The data management strategy trades disk space for operational efficiency.
There can be a significant storage overhead that is un-related to your
working data set but can be tuned to best fit your usage. In short, disk
space is used until a threshold is met, then unused space is reclaimed
through a process of merging. The merge process traverses data files and
reclaims space by eliminating out-of-date versions of key/value pairs writing
only the current key/value pairs to a new set of files within the directory.

The merge process is affected by the settings described below. In the
discussion, "dead" refers to keys that are no longer the latest value or
those that have been deleted; "live" refers to keys that are the newest value
and have not been deleted.

#### Max File Size

The `max_file_size` setting describes the maximum permitted size for any
single data file in the Bitcask directory. If a write causes the current
file to exceed this size threshold then that file is closed, and a new file
is opened for writes.

Increasing `max_file_size` will cause Bitcask to create fewer, larger
files, which are merged less frequently while decreasing it will cause
Bitcask to create more numerous, smaller files, which are merged more
frequently. If your ring size is 16 your servers could see as much as 32GB
of data in the bitcask directories before the first merge is triggered
irrespective of your working set size. Plan storage accordingly and don't
be surprised by larger than working set on disk data sizes.

Default is: `16#80000000` which is 2GB in bytes

```erlang
{bitcask, [
        ...,
        {max_file_size, 16#80000000}, %% 2GB default
        ...
]}
```

#### Merge Window

The `merge_window` setting lets you specify when during the day merge
operations are allowed to be triggered. Valid options are:

* `always` (default) No restrictions
* `never` Merge will never be attempted
* `{Start, End}` Hours during which merging is permitted, where `Start` and
  `End` are integers between 0 and 23. A window defined as `{1, 3}` means that
  merging will be allowed to start between 01:00 and 03:59.

If merging has a significant impact on performance of your cluster, or your
cluster has quiet periods in which little storage activity occurs, you may
want to change this setting from the default.

A common way to limit the impact of merging is to create separate merge windows 
for each node in the cluster and ensure that these windows do not overlap. This
ensures that at most one node at the time can be affected by merging, leaving 
the remaining nodes to handle requests. The main drawback of this approach is that
merges will occur less frequently, leading to increased disk space usage.

Default is: `always`

```erlang
{bitcask, [
        ...,
            {merge_window, always}, %% Span of hours during which merge is acceptable.
        ...
]}
```

<div class="note"><div class="title"> `merge_window` and Multi-Backend</div>
When using Bitcask with [[Multi-Backend|Multi]], please note that if you
wish to use a merge window, you *must* set it in the global `bitcask`
section of your `app.config`.  `merge_window` settings in per-backend
sections are ignored.
</div>


#### Merge Triggers

Merge triggers determine under what conditions merging will be
invoked.

* _Fragmentation_: The `frag_merge_trigger` setting describes what ratio of
  dead keys to total keys in a file will trigger merging. The value of this
  setting is a percentage (0-100). For example, if a data file contains 6
  dead keys and 4 live keys, then merge will be triggered at the default
  setting. Increasing this value will cause merging to occur less often,
  whereas decreasing the value will cause merging to happen more often.

  Default is: `60`

* _Dead Bytes_: The `dead_bytes_merge_trigger` setting describes how much
  data stored for dead keys in a single file will trigger merging. The
  value is in bytes. If a file meets or exceeds the trigger value for dead
  bytes, merge will be triggered. Increasing the value will cause merging
  to occur less often, whereas decreasing the value will cause merging to
  happen more often.

  When either of these constraints are met by any file in the directory,
  Bitcask will attempt to merge files.

  Default is: `536870912` which is 512MB in bytes

```erlang
{bitcask, [
        ...,
        %% Trigger a merge if any of the following are true:
            {frag_merge_trigger, 60}, %% fragmentation >= 60%
        {dead_bytes_merge_trigger, 536870912}, %% dead bytes > 512 MB
        ...
]}
```

#### Merge Thresholds

Merge thresholds determine which files will be chosen to be included in a
merge operation.

- _Fragmentation_: The `frag_threshold` setting describes what ratio of
    dead keys to total keys in a file will cause it to be included in the
    merge. The value of this setting is a percentage (0-100). For example,
    if a data file contains 4 dead keys and 6 live keys, it will be included
    in the merge at the default ratio. Increasing the value will cause fewer
    files to be merged, decreasing the value will cause more files to be
    merged.

    Default is: `40`

- _Dead Bytes_: The `dead_bytes_threshold` setting describes the minimum
    amount of data occupied by dead keys in a file to cause it to be included
    in the merge. Increasing the value will cause fewer files to be merged,
    decreasing the value will cause more files to be merged.

    Default is: `134217728` which is 128MB in bytes

- _Small File_: The `small_file_threshold` setting describes the minimum
    size a file must have to be _excluded_ from the merge. Files smaller
    than the threshold will be included. Increasing the value will cause
    _more_ files to be merged, decreasing the value will cause _fewer_ files
    to be merged.

    Default is: `10485760` while is 10MB in bytes

When any of these constraints are met for a single file, it will be
included in the merge operation.

```erlang
{bitcask, [
        ...,
        %% Conditions that determine if a file will be examined during a merge:
            {frag_threshold, 40}, %% fragmentation >= 40%
        {dead_bytes_threshold, 134217728}, %% dead bytes > 128 MB
        {small_file_threshold, 10485760}, %% file is < 10MB
        ...
]}
```

<div class="note"><div class="title">Choosing Threshold Values</div><p>The
values for <code>frag_threshold</code> and <code>dead_bytes_threshold</code>
<i>must be equal to or less than their corresponding trigger values</i>. If
they are set higher, Bitcask will trigger merges where no files meet the
thresholds, and thus never resolve the conditions that triggered
merging.</p></div>

#### Fold Keys Threshold

Fold keys thresholds will reuse the keydir if another fold was started less
than `max_fold_age` ago and there were less than `max_fold_puts` updates.
Otherwise it will wait until all current fold keys complete and then start.
Set either option to -1 to disable.

```erlang
{bitcask, [
        ...,
            {max_fold_age, -1}, %% Age in micro seconds (-1 means "unlimited")
        {max_fold_puts, 0}, %% Maximum number of updates
        ...
]}
```

#### Automatic Expiration

By default, Bitcask keeps all of your data around. If your data has
limited time-value, or if for space reasons you need to purge data, you can
set the `expiry_secs` option. If you needed to purge data automatically
after 1 day, set the value to `86400`.

Default is: `-1` which disables automatic expiration

```erlang
{bitcask, [
        ...,
        {expiry_secs, -1}, %% Don't expire items based on time
        ...
]}
```

<div class="note"><p>Space occupied by stale data <i>may not be reclaimed
immediately</i>, but the data will become immediately inaccessible to client
requests. Writing to a key will set a new modification timestamp on the value
and prevent it from being expired.</p></div>

{{#1.2.1+}}

By default, Bitcask will trigger a merge whenever a data file contains
an expired key. This may result in excessive merging under some usage
patterns. To prevent this you can set the `expiry_grace_time` option.
Bitcask will defer triggering a merge solely for key expiry by the
configured number of seconds. Setting this to `3600` effectively limits
each cask to merging for expiry once per hour.

Default is: `0`

```erlang
{bitcask, [
        ...,
        {expiry_grace_time, 3600}, %% Limit rate of expiry merging
        ...
]}
```

{{/1.2.1+}}

## Tuning Bitcask

Bitcask has many very desirable qualities and has been shown in production
to be stable, reliable, low-latency and high throughput storage engine for Riak
data.

### Tips & Tricks:

  * __Bitcask depends on filesystem caches__

    Some data storage layers implement their own page/block buffer cache
    in-memory, but Bitcask does not. Instead, it depends on the
    filesystem's cache. Adjusting the caching characteristics of your
    filesystem can impact performance.

  * __Be aware of file handle limits__

    Review the [[open files limitations|Open-Files-Limit]] information.

  * __Avoid the overhead of updating file metadata (such as last access time) on every read or write operation__

    You can get a big speed boost by adding the `noatime` mounting option to
    `/etc/fstab`. This will disable the recording of the "last accessed time"
    for all files, which results in less disk head seeks. If you need last
    access times but you'd like some of the benefits of this optimization
    you can try `relatime`.

    ```
    /dev/sda5    /data           ext3    noatime  1 1
    /dev/sdb1    /data/inno-log  ext3    noatime  1 2
    ```

  * __Small number of frequently changed keys__

    When keys are changed frequently, fragmentation rapidly increases. To
    counteract this, one should lower the fragmentation trigger and threshold.

  * __Limited disk space__

    When disk space is limited, keeping the space occupied by dead keys limited
    is of paramount importance. Lower the dead bytes threshold and trigger to
    counteract wasted space.

  * __Purging stale entries after a fixed period__

    To automatically purge stale values, set the `expiry_secs` value to the
    desired cutoff time. Keys that are not modified for a period equal to or
    greater than `expiry_secs` will become inaccessible.

  * __High number of partitions per-node__

    Because the cluster has many partitions running, this means Bitcask will
    have many [[files open|Open-Files-Limit]]. To reduce the number of open
    files, you might increase `max_file_size` so that larger files will be
    written. You might also decrease the fragmentation and dead-bytes settings
    and increase the `small_file_threshold` so that merging will keep the
    number of open files small in number.

  * __High daytime traffic, low nighttime traffic__

    In order to cope with a high volume of writes without performance
    degradation during the day, one might want to prevent merging except in
    non-peak periods. Setting the `merge_window` to hours of the day when
    traffic is low will help.

  * __Multi-cluster replication (Riak Enterprise)__

    If you are using Riak Enterprise with the replication feature enabled,
    your clusters might experience higher production of fragmentation and dead
    bytes caused by replays. Additionally, because the full-sync feature
    operates across entire partitions, it will be made more efficient by
    accessing data as sequentially as possible (across fewer files). Lowering
    both the fragmentation and dead-bytes settings will improve performance.

## FAQ

  * [[Why does it seem that Bitcask merging is only triggered when a Riak node is restarted?|Developing on Riak FAQs#why-does-it-seem-that-bitc]]
  * [[If the size of key index exceeds the amount of memory, how does Bitcask handle it?|Operating Riak FAQs#if-the-size-of-key-index-e]]
  * [[Bitcask Capacity Planning]] Guide

## Bitcask Implementation Details

Riak will create a Bitcask database directory for each vnode in a cluster. In
each of those directories there will be at most one database file open for
writing at any given time. The file being written to will grow until it
exceeds a size threshold at which time it is closed and a new ﬁle is created
for additional writes. Once a ﬁle is closed, either purposefully or due to
server exit, it is considered immutable and will never be opened for writing
again.

The file currently open for writes is only written by appending,
which means that sequential writes do not require disk seeking dramatically
speeding up disk I/O. Note that this can be spoiled when you still have
`atime` enabled on your filesystem because the disk head will have to move to
update both the data blocks and the file and directory meta data blocks. The
primary speed-up from a log-based database is its ability to minimize disk head
seeks. Deleting a value from Bitcask is a two step process. First we append
a "tombstone" record to the file open for writes which indicates that a value
was marked for deletion at that time. At the same time we remove references to
that key in the in-memory `keydir` information.

Later, during a merge, non-active data files are scanned and only those values
without tombstones are merged into the active data file. This effectively
removes the obsolete data and reclaims disk space associated with it. This data
management strategy may use up a lot of space over time, since we just write
out new values without touching the old ones. A process for compaction that we
refer to as ”merging” solves this. The merge process iterates over all
non-active (i.e. immutable) ﬁles in a Bitcask and produces as output a set of
data files containing only the ”live” or latest versions of each present key.

### Bitcask Database Files

Below there are two directory listings showing what you would expect to find on
disk when using Bitcask. In this example we use a 64 partition ring which
results in 64 separate directories, each with their own Bitcask database.

```
bitcask/
|-- 0-131678707860265
|-- 1004782375664995756265033322492444576013453623296-1316787078215038
|-- 1027618338748291114361965898003636498195577569280-1316787078218073

... etc ...

`-- 981946412581700398168100746981252653831329677312-1316787078206121
```

Note that when starting up the directories are created for each vnode
partition's data however there are no Bitcask-specific files as yet.

After performing one "put" (write) into the Riak cluster running Bitcask.

```
curl http://localhost:8098/riak/test/test -XPUT -d 'hello' -H 'content-type: text/plain'
```

The "N" value for this cluster is 3 so you'll see that the three vnode
partitions responsible for this data now have Bitcask database files.

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

As more data is written to the cluster, more Bitcask files are created until
merges are triggered.

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
