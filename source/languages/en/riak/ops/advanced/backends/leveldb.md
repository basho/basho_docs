---
title: LevelDB
project: riak
version: 1.0.0+
document: tutorials
toc: true
audience: intermediate
keywords: [backends, planning, leveldb]
prev: "[[Bitcask]]"
up:   "[[Choosing a Backend]]"
next: "[[Memory]]"
interest: [
  "[[LevelDB documentation|http://leveldb.googlecode.com/svn/trunk/doc/index.html]]",
  "[[Cache Oblivious BTree|http://supertech.csail.mit.edu/cacheObliviousBTree.html]]",
  "[[LevelDB benchmarks|http://leveldb.googlecode.com/svn/trunk/doc/benchmark.html]]",
  "[[LevelDB on Wikipedia|http://en.wikipedia.org/wiki/LevelDB]]",
  "[[LSM trees|http://nosqlsummer.org/paper/lsm-tree]]",
  "[[Cache Conscious Indexing for Decision-Support in Main Memory|http://www.cs.columbia.edu/~library/TR-repository/reports/reports-1998/cucs-019-98.pdf]]"
]
moved: {
    '1.4.0-': '/tutorials/choosing-a-backend/LevelDB'
}
---

## Overview

[eLevelDB](https://github.com/basho/eleveldb) is an Erlang application that encapsulates [LevelDB](http://code.google.com/p/leveldb/), an open-source, on-disk key/value store created by Google Fellows Jeffrey Dean and Sanjay Ghemawat.

LevelDB is a relatively new entrant into the growing list of key/value database libraries, but it has some very interesting qualities that we believe make it an ideal candidate for use in Riak. LevelDB's storage architecture is more like [BigTable's](http://en.wikipedia.org/wiki/BigTable) memtable/sstable model than it is like Bitcask. This design and implementation provide the possibility of a storage engine without Bitcask's RAM limitation.

**Note**: Riak uses a fork of LevelDB. The code can be found [on Github](https://github.com/basho/leveldb).

A number of changes have been introduced in the LevelDB backend in Riak 2.0:

* There is now only _one_ performance-related setting that Riak users need to define---`leveldb.total_mem_percent`---as LevelDB now dynamically sizes the file cache and block sizes based upon active vnodes assigned to the node.
* The LevelDB backend in Riak 2.0 utilizes a new, faster threading model for background compaction work on `.sst` table files. The new model has increased throughput by at least 10% in all test scenarios.
* Delete operations now receive priority handling in compaction selection, leading to a more aggressive reclaiming of disk space than in previous versions of Riak's LevelDB backend.
* Nodes storing massive key datasets (e.g. in the billions of keys) now receive increased throughput due to automatic management of LevelDB's block size parameter. This parameter is slowly raised to increase the number of files that can open simultaneously, leading to improved random read performance.

### Strengths

* License

    The LevelDB and eLevelDB licenses are the [New BSD License](http://www.opensource.org/licenses/bsd-license.php) and the [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0.html), respectively. We'd like to thank Google and the authors of LevelDB at Google for choosing a completely FLOSS license so that everyone can benefit from this innovative storage engine.

* Data compression

    LevelDB uses Google [Snappy](https://code.google.com/p/snappy/) data compression by default. This means more CPU usage but less disk space. The compression efficiency is especially good for text data, including raw text, Base64, JSON, etc.

### Weaknesses

  * Read access can be slow when there are many levels to search

    LevelDB may have to do a few disk seeks to satisfy a read; one disk seek per level and, if 10% of the database fits in memory, one seek for the last level (since all of the earlier levels should end up cached in the OS buffer cache for most filesystems) whereas if 1% fits in memory, LevelDB will need two seeks.

## Installing eLevelDB

Riak ships with eLevelDB included within the distribution, so there is no separate installation required. However, Riak is configured to use the Bitcask storage engine by default. To switch to eLevelDB, set the `storage_backend` variable in `[[riak.conf|Configuration Files]]` to `leveldb`:

```riakconf
storage_backend = leveldb
```

## Configuring eLevelDB

eLevelDb's default behavior can be modified by adding/changing parameters in the `eleveldb` section of the `[[riak.conf|Configuration Files]]`. The [[Key Parameters|LevelDB#Key-Parameters]] section below details the parameters you'll use to modify eLevelDB. The [[Parameter Planning|LevelDB#Parameter-Planning]] section gives a step-by-step example illustrating how to choose parameter values based on your application requirements.

The configuration values that can be set in your `[[riak.conf|Configuration Files]]` for eLevelDB are as follows:

Config | Description | Default
:------|:------------|:-------
`leveldb.data_root` | LevelDB data root | `./data/leveldb`
`leveldb.total_mem_percent` | Defines the percentage (between 1 and 100) of total server memory to assign to LevelDB. LevelDB will dynamically adjust its internal cache sizes as Riak activates/inactivates vnodes on this server to stay within this size. | `70`

### Recommended Settings

Below are **general** configuration recommendations for Linux distributions. Individual users may need to tailor these settings for their application.

For production environments, we recommend the following settings within `/etc/sysctl.conf`:

```bash
net.core.wmem_default=8388608
net.core.rmem_default=8388608
net.core.wmem_max=8388608
net.core.rmem_max=8388608
net.core.netdev_max_backlog=10000
net.core.somaxconn=4000
net.ipv4.tcp_max_syn_backlog=40000
net.ipv4.tcp_fin_timeout=15
net.ipv4.tcp_tw_reuse=1
```

#### Block Device Scheduler
Beginning with the 2.6 kernel, Linux gives you a choice of four I/O [elevator models](http://www.gnutoolbox.com/linux-io-elevator/). We recommend using the NOOP elevator. You can do this by changing the scheduler on the Linux boot line: `elevator=noop`.

#### ext4 Options
The ext4 filesystem defaults include two options that increase integrity but slow performance. Because Riak's integrity is based on multiple nodes holding the same data, these two options can be changed to boost LevelDB's performance. We recommend setting: `barrier`=0 and `data`=writeback.

#### CPU Throttling
If CPU throttling is enabled, disabling it can boost LevelDB performance in some cases.

#### No Entropy
If you are using https protocol, the 2.6 kernel is widely known for stalling programs waiting for SSL entropy bits. If you are using https, we recommend installing the [HAVEGE](http://www.irisa.fr/caps/projects/hipsor/) package for pseudorandom number generation.

#### clocksource
We recommend setting "clocksource=hpet" on your linux kernel's `boot` line. The TSC clocksource has been identified to cause issues on machines with multiple physical processors and/or CPU throttling.

#### swappiness

We recommend setting `vm.swappiness=0` in `/etc/sysctl.conf`. The `vm.swappiness` default is 60, which is aimed toward laptop users with application windows. This was a key change for MySQL servers and is often referenced in database performance literature.

## Implementation Details

[LevelDB](http://leveldb.googlecode.com/svn/trunk/doc/impl.html) is a Google
sponsored open source project that has been incorporated into an Erlang
application and integrated into Riak for storage of key/value information on
disk. The implementation of LevelDB is similar in spirit to the representation
of a single Bigtable tablet (section 5.3).

### How "Levels" Are Managed

LevelDB is a memtable/sstable design. The set of sorted tables are organized
into a sequence of levels. Each level stores approximately ten times as much
data as the level before it. The sorted table generated from a flush is placed
in a special young level (also called level-0). When the number of young files
exceeds a certain threshold (currently four), all of the young files are merged
together with all of the overlapping level-1 files to produce a sequence of new
level-1 files (a new level-1 file is created for every 2MB of data.)

Files in the young level may contain overlapping keys. However files in other
levels have distinct non-overlapping key ranges. Consider level number L where
L >= 1. When the combined size of files in level-L exceeds (10^L) MB (i.e.
10MB for level-1, 100MB for level-2, ...), one file in level-L, and all of the
overlapping files in level-(L+1) are merged to form a set of new files for
level-(L+1). These merges have the effect of gradually migrating new updates
from the young level to the largest level using only bulk reads and writes
(i.e., minimizing expensive disk seeks).

When the size of level L exceeds its limit, LevelDB will compact it in a
background thread. The compaction picks a file from level L and all overlapping
files from the next level L+1. Note that if a level-L file overlaps only part
of a level-(L+1) file, the entire file at level-(L+1) is used as an input to
the compaction and will be discarded after the compaction. Compactions from
level-0 to level-1 are treated specially because level-0 is special (files in
it may overlap each other). A level-0 compaction may pick more than one
level-0 file in case some of these files overlap each other.

A compaction merges the contents of the picked files to produce a sequence of
level-(L+1) files. LevelDB will switch to producing a new level-(L+1) file
after the current output file has reached the target file size (2MB). LevelDB
will also switch to a new output file when the key range of the current output
file has grown enough to overlap more then ten level-(L+2) files. This last
rule ensures that a later compaction of a level-(L+1) file will not pick up too
much data from level-(L+2).

Compactions for a particular level rotate through the key space. In more
detail, for each level L, LevelDB remembers the ending key of the last
compaction at level L. The next compaction for level L will pick the first file
that starts after this key (wrapping around to the beginning of the key space
if there is no such file).

Level-0 compactions will read up to four 1MB files from level-0, and at worst
all the level-1 files (10MB) (i.e., LevelDB will read 14MB and write 14MB in
that case).

Other than the special level-0 compactions, LevelDB will pick one 2MB file from
level L. In the worst case, this will overlap with approximately 12 files from
level L+1 (10 because level-(L+1) is ten times the size of level-L, and another
two at the boundaries since the file ranges at level-L will usually not be
aligned with the file ranges at level-L+1). The compaction will therefore read
26MB, write 26MB. Assuming a disk IO rate of 100MB/s, the worst compaction cost
will be approximately 0.5 second.

If we throttle the background writing to a reasonably slow rate, for instance
10% of the full 100MB/s speed, a compaction may take up to 5 seconds. If the
user is writing at 10MB/s, LevelDB might build up lots of level-0 files (~50 to
hold the 5*10MB). This may significantly increase the cost of reads due to the
overhead of merging more files together on every read.

### Compaction

Levels are compacted into ordered data files over time. Compaction first
computes a score for each level as the ratio of bytes in that level to desired
bytes. For level 0, it computes files / desired files instead. The level with
the highest score is compacted.

When compacting L0 the only special case to consider is that after picking the
primary L0 file to compact, it will check other L0 files to determine the
degree to which they overlap. This is an attempt to avoid some I/O, we can
expect L0 compactions to usually if not always be "all L0 files".

See the PickCompaction routine in
[1](http://www.google.com/codesearch#mHLldehqYMA/trunk/db/version_set.cc) for
all the details.

### Comparison of eLevelDB and Bitcask

LevelDB is a persistent ordered map; Bitcask is a persistent hash table (no
ordered iteration). Bitcask stores keys in memory, so for databases with large
number of keys it may exhaust available physical memory and then swap into
virtual memory causing a severe slow down in performance. Bitcask guarantees
at most one disk seek per look-up. LevelDB may have to do a small number of
disk seeks. For instance, a read needs one disk seek per level. If 10% of
the database fits in memory, LevelDB will need to do one seek (for the last
level since all of the earlier levels should end up cached in the OS buffer
cache). If 1% fits in memory, LevelDB will need two seeks.

## Recovery

LevelDB never writes in place: it always appends to a log file, or merges
existing files together to produce new ones. So an OS crash will cause a
partially written log record (or a few partially written log records). LevelDB
recovery code uses checksums to detect this and will skip the incomplete
records.

### eLevelDB Database Files

Below are two directory listings showing what you would expect to find on
disk when using eLevelDB. In this example, we use a 64-partition ring which
results in 64 separate directories, each with their own LevelDB database:

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

`-- 981946412581700398168100746981252653831329677312
    |-- 000005.log
    |-- CURRENT
    |-- LOCK
    |-- LOG
    |-- LOG.old
    `-- MANIFEST-000004

64 directories, 378 files
```

After performing a large number of PUT (write) operations, the Riak cluster
running eLevelDB will look something like this:

```bash
$ tree leveldb/
leveldb/
|-- 0
|   |-- 000118.sst
|   |-- 000119.sst
|   |-- 000120.sst
|   |-- 000121.sst
|   |-- 000123.sst
|   |-- 000126.sst
|   |-- 000127.log
|   |-- CURRENT
|   |-- LOCK
|   |-- LOG
|   |-- LOG.old
|   `-- MANIFEST-000125
|-- 1004782375664995756265033322492444576013453623296
|   |-- 000120.sst
|   |-- 000121.sst
|   |-- 000122.sst
|   |-- 000123.sst
|   |-- 000125.sst
|   |-- 000128.sst
|   |-- 000129.log
|   |-- CURRENT
|   |-- LOCK
|   |-- LOG
|   |-- LOG.old
|   `-- MANIFEST-000127
|-- 1027618338748291114361965898003636498195577569280
|   |-- 000003.log
|   |-- CURRENT
|   |-- LOCK
|   |-- LOG
|   `-- MANIFEST-000002

... etc ...

`-- 981946412581700398168100746981252653831329677312
    |-- 000003.log
    |-- CURRENT
    |-- LOCK
    |-- LOG
    `-- MANIFEST-000002

64 directories, 433 files
```
