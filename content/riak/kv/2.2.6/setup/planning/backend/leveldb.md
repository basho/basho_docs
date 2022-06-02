---
title: "LevelDB"
description: ""
project: "riak_kv"
project_version: "2.2.6"
menu:
  riak_kv-2.2.6:
    name: "LevelDB"
    identifier: "planning_backend_leveldb"
    weight: 101
    parent: "planning_choose_backend"
toc: true
aliases:
  - /riak/2.2.6/ops/advanced/backends/leveldb/
  - /riak/kv/2.2.6/ops/advanced/backends/leveldb/
---

[upgrade 2.0#upgrading-leveldB]: {{<baseurl>}}
[glossary vnode]: {{<baseurl>}}riak/kv/2.2.6/learn/glossary/#vnode
[config reference]: {{<baseurl>}}riak/kv/2.2.6/configuring/reference
[perf index]: {{<baseurl>}}riak/kv/2.2.6/using/performance
[config reference#aae]: {{<baseurl>}}riak/kv/2.2.6/configuring/reference/#active-anti-entropy

> **Note on upgrading to 2.0**
>
> If you are using LevelDB in a 1.x version of Riak, are upgrading to 2.0,
and wish to keep using your old `app.config` file for configuration,
make sure to follow the steps for setting the
`total_leveldb_mem_percent` parameter in the
[2.0 upgrade guide][upgrade 2.0#upgrading-leveldB].

[eLevelDB](https://github.com/basho/eleveldb) is an Erlang application
that encapsulates [LevelDB](http://code.google.com/p/leveldb/), an
open-source, on-disk key/value store created by Google Fellows Jeffrey
Dean and Sanjay Ghemawat.

LevelDB is a relatively new entrant into the growing list of key/value
database libraries, but it has some very interesting qualities that we
believe make it an ideal candidate for use in Riak. LevelDB's storage
architecture is more like
[BigTable's](http://en.wikipedia.org/wiki/BigTable) memtable/sstable
model than it is like Bitcask. This design and implementation provide
the possibility of a storage engine without Bitcask's RAM limitation.

> **Note:** Riak uses a fork of LevelDB. The code can be found
[on Github](https://github.com/basho/leveldb).

A number of changes have been introduced in the LevelDB backend in Riak
2.0:

* There is now only _one_ performance-related setting that Riak users
  need to define---`leveldb.total_mem_percent`---as LevelDB now
  dynamically sizes the file cache and block sizes based upon active
  [vnodes][glossary vnode] assigned to the node.
* The LevelDB backend in Riak 2.0 utilizes a new, faster threading model
  for background compaction work on `.sst` table files. The new model
  has increased throughput by at least 10% in all test scenarios.
* Delete operations now receive priority handling in compaction
  selection, which means more aggressive reclaiming of disk space than
  in previous versions of Riak's LevelDB backend.
* Nodes storing massive key datasets (e.g. in the billions of keys) now
  receive increased throughput due to automatic management of LevelDB's
  block size parameter. This parameter is slowly raised to increase the
  number of files that can open simultaneously, improving random read
  performance.

## Strengths

1. **License** --- The LevelDB and eLevelDB licenses are the [New BSD
   License](http://www.opensource.org/licenses/bsd-license.php) and the
   [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0.html),
   respectively. We'd like to thank Google and the authors of LevelDB at
   Google for choosing a completely FLOSS license so that everyone can
   benefit from this innovative storage engine.
2. **Data compression** --- LevelDB provides two compression algorithms
   to reduce storage size and increase efficient use of storage bandwidth:
      * Google's [Snappy](https://code.google.com/p/snappy/) data compression
      * [LZ4](https://en.wikipedia.org/wiki/LZ4_(compression_algorithm)) data
        compression

    Enabling compression means more CPU usage but less disk space. Compression
    is especially good for text data, including raw text, Base64, JSON, etc.

## Weaknesses

1. Read access can be slow when there are many levels to search
2. LevelDB may have to do a few disk seeks to satisfy a read; one disk
   seek per level and, if 10% of the database fits in memory, one seek
   for the last level (since all of the earlier levels should end up
   cached in the OS buffer cache for most filesystems) whereas if 1%
   fits in memory, LevelDB will need two seeks.

## Installing eLevelDB

Riak ships with eLevelDB included within the distribution, so there is
no separate installation required. However, Riak is configured to use
the Bitcask storage engine by default. To switch to eLevelDB, set the
`storage_backend` variable in [`riak.conf`][config reference] to
`leveldb`:

```riakconf
storage_backend = leveldb
```

```appconfig
{riak_kv, [
    %% ...
    {storage_backend, riak_kv_eleveldb_backend},
    %% ...
    ]}
```

## Configuring eLevelDB

eLevelDb's default behavior can be modified by adding/changing
parameters in the `eleveldb` section of the [`riak.conf`][config reference]. The section below details the parameters you'll use to modify eLevelDB.

The configuration values that can be set in your
[`riak.conf`][config reference] for eLevelDB are as follows:

Config | Description | Default
:------|:------------|:-------
`leveldb.data_root` | LevelDB data root | `./data/leveldb`
`leveldb.maximum_memory.percent` | Defines the percentage (between 1 and 100) of total server memory to assign to LevelDB. LevelDB will dynamically adjust its internal cache sizes as Riak activates/inactivates [vnodes][glossary vnode] on this server to stay within this size. | `70`

If you are using the older, `app.config`-based system, the equivalent to
the `leveldb.data_root` is the `data_root` setting, as in the following
example:

```appconfig
{eleveldb, [
    {data_root, "/path/to/leveldb"},

    %% Other eleveldb-specific settings
]}
```

The `leveldb.maximum_memory.percent` setting is only available in the
newer configuration system.

### Recommended Settings

Below are **general** configuration recommendations for Linux
distributions. Individual users may need to tailor these settings for
their application.

#### sysctl

For production environments, please see [System Performance Tuning][perf index]
for the recommended `/etc/sysctl.conf` settings.

#### Block Device Scheduler

Beginning with the 2.6 kernel, Linux gives you a choice of four I/O
[elevator models](http://www.gnutoolbox.com/linux-io-elevator/). We
recommend using the NOOP elevator. You can do this by changing the
scheduler on the Linux boot line: `elevator=noop`.

#### ext4 Options

The ext4 filesystem defaults include two options that increase integrity
but slow performance. Because Riak's integrity is based on multiple
nodes holding the same data, these two options can be changed to boost
LevelDB's performance. We recommend setting: `barrier`=0 and
`data`=writeback.

#### CPU Throttling

If CPU throttling is enabled, disabling it can boost LevelDB performance
in some cases.

#### No Entropy

If you are using https protocol, the 2.6 kernel is widely known for
stalling programs waiting for SSL entropy bits. If you are using https,
we recommend installing the
[HAVEGE](http://www.irisa.fr/caps/projects/hipsor/) package for
pseudorandom number generation.

#### clocksource

We recommend setting `clocksource=hpet` on your Linux kernel's `boot`
line. The TSC clocksource has been identified to cause issues on
machines with multiple physical processors and/or CPU throttling.

#### swappiness

We recommend setting `vm.swappiness=0` in `/etc/sysctl.conf`. The
`vm.swappiness` default is 60, which is aimed toward laptop users with
application windows. This was a key change for MySQL servers and is
often referenced in database performance literature.

## Implementation Details

[LevelDB](http://leveldb.googlecode.com/svn/trunk/doc/impl.html) is a
Google-sponsored open source project that has been incorporated into an
Erlang application and integrated into Riak for storage of key/value
information on disk. The implementation of LevelDB is similar in spirit
to the representation of a single Bigtable tablet (section 5.3).

### How Levels Are Managed

LevelDB is a memtable/sstable design. The set of sorted tables is
organized into a sequence of levels. Each level stores approximately ten
times as much data as the level before it. The sorted table generated
from a flush is placed in a special young level (also called level-0).
When the number of young files exceeds a certain threshold (currently
four), all of the young files are merged together with all of the
overlapping level-1 files to produce a sequence of new level-1 files (a
new level-1 file is created for every 2MB of data.)

Files in the young level may contain overlapping keys. However files in
other levels have distinct non-overlapping key ranges. Consider level
number L where L >= 1. When the combined size of files in level-L
exceeds (10^L) MB (i.e.  10MB for level-1, 100MB for level-2, ...), one
file in level-L, and all of the overlapping files in level-(L+1) are
merged to form a set of new files for level-(L+1). These merges have the
effect of gradually migrating new updates from the young level to the
largest level using only bulk reads and writes (i.e., minimizing
expensive disk seeks).

When the size of level L exceeds its limit, LevelDB will compact it in a
background thread. The compaction picks a file from level L and all
overlapping files from the next level L+1. Note that if a level-L file
overlaps only part of a level-(L+1) file, the entire file at level-(L+1)
is used as an input to the compaction and will be discarded after the
compaction. Compactions from level-0 to level-1 are treated specially
because level-0 is special (files in it may overlap each other). A
level-0 compaction may pick more than one level-0 file in case some of
these files overlap each other.

A compaction merges the contents of the picked files to produce a
sequence of level-(L+1) files. LevelDB will switch to producing a new
level-(L+1) file after the current output file has reached the target
file size (2MB). LevelDB will also switch to a new output file when the
key range of the current output file has grown enough to overlap more
then ten level-(L+2) files. This last rule ensures that a later
compaction of a level-(L+1) file will not pick up too much data from
level-(L+2).

Compactions for a particular level rotate through the key space. In more
detail, for each level L, LevelDB remembers the ending key of the last
compaction at level L. The next compaction for level L will pick the
first file that starts after this key (wrapping around to the beginning
of the key space if there is no such file).

Level-0 compactions will read up to four 1MB files from level-0, and at
worst all the level-1 files (10MB) (i.e., LevelDB will read 14MB and
write 14MB in that case).

Other than the special level-0 compactions, LevelDB will pick one 2MB
file from level L. In the worst case, this will overlap with
approximately 12 files from level L+1 (10 because level-(L+1) is ten
times the size of level-L, and another two at the boundaries since the
file ranges at level-L will usually not be aligned with the file ranges
at level-L+1). The compaction will therefore read 26MB, write 26MB.
Assuming a disk IO rate of 100MB/s, the worst compaction cost will be
approximately 0.5 second.

If we throttle the background writing to a reasonably slow rate, for
instance 10% of the full 100MB/s speed, a compaction may take up to 5
seconds. If the user is writing at 10MB/s, LevelDB might build up lots
of level-0 files (~50 to hold the 5*10MB). This may significantly
increase the cost of reads due to the overhead of merging more files
together on every read.

### Compaction

Levels are compacted into ordered data files over time. Compaction first
computes a score for each level as the ratio of bytes in that level to
desired bytes. For level 0, it computes files / desired files instead.
The level with the highest score is compacted.

When compacting L0 the only special case to consider is that after
picking the primary L0 file to compact, it will check other L0 files to
determine the degree to which they overlap. This is an attempt to avoid
some I/O, we can expect L0 compactions to usually if not always be "all
L0 files".

See the PickCompaction routine in
[1](https://github.com/basho/leveldb/blob/develop/db/version_set.cc)
for all the details.

### Comparison of eLevelDB and Bitcask

LevelDB is a persistent ordered map; Bitcask is a persistent hash table
(no ordered iteration). Bitcask stores keys in memory, so for databases
with large number of keys it may exhaust available physical memory and
then swap into virtual memory causing a severe slow down in performance.
Bitcask guarantees at most one disk seek per look-up. LevelDB may have
to do a small number of disk seeks. For instance, a read needs one disk
seek per level. If 10% of the database fits in memory, LevelDB will need
to do one seek (for the last level since all of the earlier levels
should end up cached in the OS buffer cache). If 1% fits in memory,
LevelDB will need two seeks.

## Recovery

LevelDB never writes in place: it always appends to a log file, or
merges existing files together to produce new ones. So an OS crash will
cause a partially written log record (or a few partially written log
records). LevelDB recovery code uses checksums to detect this and will
skip the incomplete records.

### eLevelDB Database Files

Below are two directory listings showing what you would expect to find
on disk when using eLevelDB. In this example, we use a 64-partition ring
which results in 64 separate directories, each with their own LevelDB
database:

```bash
leveldb/
|-- 0
|   |-- 000003.log
|   |-- CURRENT
|   |-- LOCK
|   |-- LOG
|   `-- MANIFEST-000002
|-- 1004782375664995756265033322492444576013453623296
|   |-- 000005.log
|   |-- CURRENT
|   |-- LOCK
|   |-- LOG
|   |-- LOG.old
|   `-- MANIFEST-000004
|-- 1027618338748291114361965898003636498195577569280
|   |-- 000005.log
|   |-- CURRENT
|   |-- LOCK
|   |-- LOG
|   |-- LOG.old
|   `-- MANIFEST-000004

... etc ...

`-- 9819464125817003981681007469812.2.63831329677312
    |-- 000005.log
    |-- CURRENT
    |-- LOCK
    |-- LOG
    |-- LOG.old
    `-- MANIFEST-000004

64 directories, 378 files
```

After performing a large number of PUT (write) operations, the Riak
cluster running eLevelDB will look something like this:

```bash
tree leveldb
```

The result should look something like this:

```
├── 0
│   ├── 000003.log
│   ├── CURRENT
│   ├── LOCK
│   ├── LOG
│   ├── MANIFEST-000002
│   ├── sst_0
│   ├── sst_1
│   ├── sst_2
│   ├── sst_3
│   ├── sst_4
│   ├── sst_5
│   └── sst_6
├── 1004782375664995756265033322492444576013453623296
│   ├── 000003.log
│   ├── CURRENT
│   ├── LOCK
│   ├── LOG
│   ├── MANIFEST-000002
│   ├── sst_0
│   ├── sst_1
│   ├── sst_2
│   ├── sst_3
│   ├── sst_4
│   ├── sst_5
│   └── sst_6

... etc ...
```

## Tiered Storage

Google's original LevelDB implemented stored all `.sst` table files in a
single database directory. In Riak 1.3, the original LevelDB code was
modified to store `.sst` files in subdirectories representing each
"level" of the file, e.g. `sst_0` or `sst_1`, in the name of speeding up
database repair operations.

An additional advantage of this approach is that it enables Riak
operators to mount alternative storage devices at each level of a
LevelDB database. This can be an effective strategy because LevelDB is
write intensive in lower levels, with the write intensity declining as
the level number increases. This is due to LevelDB's storage strategy,
which places more frequently updated data in lower levels.

Because write intensity differs by level, performance can be improved by
mounting faster, more expensive storage arrays in lower levels and
slower, less expensive arrays at higher levels. Tiered storage enables
you to configure the level at which LevelDB switches from a faster array
to a slower array.

> **Note on write throttling**
>
> High-volume, sustained write operations can occasionally fill the
higher-speed storage arrays before LevelDB has had the opportunity to
move data to the low-speed arrays. LevelDB's write throttle will slow
incoming write operations to allow compactions to catch up, as would be
the case when using a single storage array.

### Configuring Tiered Storage

If you are using the newer, `riak.conf`-based configuration system, the
following parameters can be used to configure LevelDB tiered storage:

Parameter | Description
:---------|:-----------
`leveldb.tiered` | The level number at which data should switch to the slower array. The default is `0`, which disables the feature.
`leveldb.tiered.path.fast` | The path prefix for `.sst` files below the level set by `leveldb.tiered`
`leveldb.tiered.path.slow` | The path prefix for `.sst` files at and above the level set by `leveldb.tiered`

If you are using the older, `app.config`-based system, the example below
will show you the equivalents of the settings listed in the table above.

#### Example

The following example LevelDB tiered storage
[configuration][config reference] for Riak 2.0 sets the level for
switching storage arrays to 4 and the file path prefix to `fast_raid`
for the faster array and `slow_raid` for the slower array:

```riakconf
leveldb.tiered = 4
leveldb.tiered.path.fast = /mnt/fast_raid
leveldb.tiered.path.slow = /mnt/slow_raid
```

```appconfig
{eleveldb, [
    {tiered_slow_level, 4},
    {tiered_fast_prefix, "/mnt/fast_raid"},
    {tiered_slow_prefix, "/mnt/slow_raid"}
]}
```

With this configuration, level directories `sst_0` through `sst_3` will
be stored in `/mnt/fast_raid`, while directories `sst_4` and `sst_6`
will be stored in `/mnt/slow_raid`.

### Selecting a Level

LevelDB will perform optimally when as much data as possible is stored
in the faster array. The amount of data that can be stored in the faster
array depends on the size of your array and the total number of LevelDB
databases (i.e. the total number of Riak [vnodes][glossary vnode])
in your cluster. The following table shows approximate sizes (in
megabytes) for each of the following sizes: the amount of raw data
stored in the level, the cumulative size of all levels up to the
specified level, and the cumulative size including active anti-entropy
data.

Level | Level Size | Cumulative Size | Cumulative with AAE
:-----|:-----------|:----------------|:-------------------
0 | 360 | 360 | 720
1 | 2,160 | 2,520 | 5,040
2 | 2,940 | 5,460 | 10,920
3 | 6,144 | 11,604 | 23,208
4 | 122,880 | 134,484 | 268,968
5 | 2,362,232 | 2,496,716 | 4,993,432
6 | not limited | not limited | not limited

To select the appropriate value for `leveldb.tiered`, use the following
steps:

* Determine the value of (ring size) / (N - 1), where ring size is the
  value of the `ring_size` configuration parameter and N is the number
  of nodes in the cluster. For a `ring_size` of 128 and a cluster with
  10 nodes, the value would be 14.
* Select either the **Cumulative Size** or **Cumulative with AAE**
  column from the table above. Select the third column if you are not
  using active anti-entropy or the fourth column if you are (i.e. if the
  `anti_entropy` [configuration parameter][config reference#aae] is set to `active`).
* Multiply the value from the first step by the cumulative column in
  each row in the table. The first result that exceeds your fast storage
  array capacity will provide the level number that should be used for
  your `leveldb.tiered` setting.

### Migrating from One Configuration to Another

If you want to use tiered storage in a new Riak installation, you don't
need to take any steps beyond setting configuration. The rest is
automated.

But if you'd like to use tiered storage in an existing installation that
is not currently using it, you will need to manually move your
installation's `.sst` files from one configuration to another.
