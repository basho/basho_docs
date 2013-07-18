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
---

## Overview

[eLevelDB](https://github.com/basho/eleveldb) is an Erlang application that
encapsulates [LevelDB](http://code.google.com/p/leveldb/), an open source
on-disk key-value store written by Google Fellows Jeffrey Dean and Sanjay
Ghemawat. LevelDB is a relatively new entrant into the growing list of
key/value database libraries but it has some very interesting qualities that we
believe make it an ideal candidate for use in Riak. LevelDB's storage
architecture is more like [BigTable's](http://en.wikipedia.org/wiki/BigTable)
memtable/sstable model than it is like Bitcask. This design
and implementation brings the possibility of a storage engine without Bitcask's RAM limitation.

Riak 1.2 introduced changes in elevelDB that allow users to tune levelDB performance
for "large data" environments typical in Riak deployments.


### Strengths:

  * License

    The LevelDB and eLevelDB licenses are the [New BSD
    License](http://www.opensource.org/licenses/bsd-license.php) and the
    [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0.html).
    We'd like to thank Google and the authors of LevelDB at Google for choosing
    a completely F/LOSS license so that everyone can benefit from this
    innovative storage engine.

  * Data compression

    LevelDB uses Google Snappy data compression by default. This means more CPU
    usage but less disk space. The compression efficiency is especially good for
    text data: raw text, Base64, JSON, etc.

### Weaknesses:

  * Read access can slow when there are many levels to search

    LevelDB may have to do a few disk seeks to satisfy a read; one disk seek
    per level and, if 10% of the database fits in memory, one seek for the last
    level (since all of the earlier levels should end up cached in the OS
    buffer cache for most file systems) whereas if 1% fits in memory, LevelDB
    will need two seeks.

## Installing eLevelDB

Riak ships with eLevelDB included within the distribution, so there is no
separate installation required.  However, Riak is configured by default to use
the Bitcask storage engine.  To switch to eLevelDB set the ```storage_backend```
variable in [[app.config|Configuration Files]] to ```riak_kv_eleveldb_backend```.

```bash
{riak_kv, [
    {storage_backend, riak_kv_eleveldb_backend},
```

## Configuring eLevelDB

eLevelDb's default behavior can be modified by adding/changing parameters in the `eleveldb` section
of the [[app.config|Configuration Files]].  The [[Key Parameters|LevelDB#Key-Parameters]] section below details
the parameters you'll use to modify eLevelDB.  The [[Parameter Planning|LevelDB#Parameter-Planning]] section gives
a step-by-step example illustrating how to choose parameter values based on your application requirements.

The configuration values that can be set in your
[[app.config|Configuration Files]] for eLevelDB are as follows:

```erlang
 %% LevelDB Config
 {eleveldb, [
     %% Required. Set to your data storage directory
     {data_root, "/var/lib/riak/leveldb"},

     %% Memory usage per vnode

     %% Maximum number of files open at once per partition
     %% Default. You must calculate to adjust (see below)
     {max_open_files, 30},
     %% Default. You must calculate to adjust (see below)
     {cache_size, 8388608},

     %% Write performance, Write safety

     %% this is default, recommended
     {sync, false},
     %% this is default, recommended
     {write_buffer_size_min, 31457280},
     %% this is default, recommended
     {write_buffer_size_max, 62914560},

     %% Read performance

     %% Required, strongly recommended to be true
     {use_bloomfilter, true},
     %% Default. Recommended to be 4k
     {sst_block_size, 4096},
     %% Default. Recommended to be 16
     {block_restart_interval, 16},

     %% Database integrity

     %% Default. Strongly recommended to be true
     {verify_checksums, true},
     %% Default. Strongly recommended to be true
     {verify_compactions, true}
 ]},
```

### Memory Usage per Vnode

The following values help adjust the memory usage per vnode.

#### Max Open Files

The `max_open_files` value is multiplied by 4 megabytes to create a file cache. The file cache may end up holding more or fewer files at any given moment due to variations in file metadata size. `max_open_files` applies to a single vnode, not to the entire server. See calculation details in [[Parameter Planning|LevelDB#Parameter-Planning]].

Where server resources allow, the value of `max_open_files` should exceed the count of `.sst` table files within the vnode's database directory.  Memory budgeting for this variable is more important to random read operations than the cache_size discussed next.

{{#1.4.0-}}
Number of open files that can be used by the DB. You may need to increase
this if your database has a large working set (budget one open file per 2MB
of working set divided by `ring_creation_size`).

The minimum `max_open_files` is 20. The default is also 20.

```erlang
{eleveldb, [
    ...,
    {max_open_files, 20},
    ...
]}
```

<div class="note"><div class="title">Changing max_open_files</div>Users that have manually set max_open_files in versions of Riak prior to 1.2 will need to reduce this value by half in Riak 1.2 (e.g. if you have max_open_files set it to 250 in Riak 1.1 then set to 125 in Riak 1.2.)</div>

{{/1.4.0-}}
{{#1.4.0+}}
The minimum `max_open_files` is 30. The default is also 30.

```erlang
{eleveldb, [
    ...,
    {max_open_files, 30},
    ...
]}
```
{{/1.4.0+}}

{{#1.4.0-1.5.0}}
<div class="note"><div class="title">Riak 1.4 Update</div>
<p>
<code>max_open_files</code> was originally a file handle limit, but file metadata is now a much larger resource concern.  The impact of Basho's larger table files and Google's recent addition of Bloom filters made the accounting switch necessary.
</p>
</div>
{{/1.4.0-1.5.0}}

<div class="note"><div class="title">Check your system's open files limits</div>
Due to the large number of open files used by this storage engine is it imperative that you review and properly set your system’s open files limits. If you are seeing an error that contains emfile then it is highly likely that you’ve exceeded the limits on your system for open files, read more about this later in the <a href="/tutorials/choosing-a-backend/LevelDB/#Tips-Tricks">Tips & Tricks section</a> to see how to fix this issue.</p>
</div>

#### Cache Size

The cache_size determines the size of each vnode's block cache. The block cache holds data blocks that leveldb has recently retrieved from `.sst` table files. Any given block contains one or more complete key/value pairs. The cache speeds up repeat access to the same key and potential access to adjacent keys.

<div class="note"><div class="title">Riak 1.2 bug</div>
<p>A bug exists in Riak 1.2 where read performance is dramatically reduced if this value is greater than the default 8,388,608.  The bug was fixed in 1.3 and the cache performance tested well to values as high as 2 gigabytes.</p>
</div>

The `cache_size` determines how much data LevelDB caches in memory. The more
of your data set that can fit in-memory, the better LevelDB will perform.
The LevelDB cache works in conjunction with your operating system and file
system caches; do not disable or under-size them. If you are running a
64-bit Erlang VM, `cache_size` can safely be set above 2G assuming you have
enough memory available. Unlike Bitcask, LevelDB keeps keys and values in a
block cache, this allows for management of key spaces that are larger than
available memory. eLevelDB creates a separate LevelDB
instance for each partition of the cluster and so each partition will have
its own cache.

We recommend that you set this to be 20-30% of available RAM (available
means after subtracting RAM consumed by other services including the
file system cache overhead from physical memory).

For example, take a cluster with 64 partitions running on 4 physical nodes
with 16GB of RAM free on each. In a best case scenario, all the nodes are
running, so a good cache size would be half the available RAM (8GB) divided
by the number of expected active vnodes on each node, which would be 64/4 = 16.
That's 536870912 bytes (512MB) per vnode

Best Case:

     (Number of free GBs / 2) * (1024 ^ 3)
    ---------------------------------------- = Cache Size
    (Number of partitions / Number of nodes)

But in reality, a node may fail. What happens to this cluster when a physical
node fails? The 16 vnodes that were managed by that node, are now handled by the
remaining active nodes in the cluster (3 in this case). Now, rather than
16 vnodes, each node will be handling approximately 22 vnodes (16 + some
number of fallback vnodes they are managing on behalf of the failed node).
With the cache size set for the optimal case, the total cache is now consuming
22 * 512MB = 11GB instead of the expected 16 * 512MB = 8GB. The total available
memory is now too heavily weighted towards cache. You'll want to add in some wiggle
room for this case.

Real Life:

        (Number of free GBs / 2) * (1024 ^ 3)
    ---------------------------------------------- = Cache Size
    (Number of partitions / (Number of nodes - F))

    F = Number of nodes that can fail before impacting cache use of memory
        on remaining systems

If we wanted 1 physical node to be able to fail before impacting cache memory
utilization, We'd use an F = 1. Now we're dividing half the available memory (still 8GB)
by (64/(4-1)) = 21.3333 (round up to 22!). This turns out to be 390451572 or 372MB.
Now each physical node can cache up to 22 vnodes before hitting the 50% memory usage mark.
If a second node went down, this cluster would feel that.

You might ask yourself, 'why only 50% of available RAM?' and the
answer is that the other physical RAM will be used by the operating
system, the Erlang VM, and by the operating system's filesystem cache
(the buffer pool on Linux). That filesystem cache will translate into
much faster access times (read and write) for your cluster. A second
reason for the buffer is to avoid paging memory to disk. Virtual memory
helps prevent failure due to out of memory conditions, but the costs of paging
memory to/from disk are very high and cause noticeable impact on cluster
performance.

Default: 8MB

```erlang
{eleveldb, [
    ...,
    {cache_size, 8388608}, %% 8MB default cache size per-partition
    ...
]}
```

### Write Performance/Write safety

These values can be adjusted to tune between write performance, and write saftey. The general rule is, the faster you want your writes perform (eg., a larger buffer), the less safe they are (more data could be lost in a crash).

#### Sync

{{#1.4.0+}}
This parameter defines how new key/value data is placed in the recovery log.  The recovery log is only used if the Riak program crashes or the server loses power unexpectedly.  The parameter's original intent was to guarantee that each new key / value was written to the physical disk before leveldb responded with "write good".  The reality in modern servers is that many layers of data caching exist between the database program and the physical disks.  This flag influences only one of the layers.

Setting this flag `true` guarantees the write operation will be slower, but does not actually guarantee the data was written to the physical device.  The data has likely been flushed from the operating system cache to the disk controller.  The disk controller may or may not save the data if the server power fails.  Setting this flag "false" allows faster write operations and depends upon the operating system's memory mapped I/O conventions to provide reasonable recovery should the Riak program fail, but not if server power fails.
{{/1.4.0+}}
{{#1.4.0-}}
If `true`, the write will be flushed from the operating system buffer cache
before the write is considered complete. Writes will be slower but data more
durable.

If this flag is false, and the machine crashes, some recent writes may be
lost. Note that if it is just the process that crashes (i.e., the machine
does not reboot), no writes will be lost even if sync is set to false.

In other words, a DB write with sync is false has similar crash semantics as
the "write()" system call. A DB write with when sync is `true` has similar
crash semantics to a "write()" system call followed by "fsync()".

One other consideration is that the hard disk itself may be buffering the
write in its memory (a write ahead cache) and responding before the data has
been written to the platter. This may or may not be safe based on whether or
not the hard disk has enough power to save its memory in the event of a power
failure. If data durability is absolutely necessary then make sure you
disable this feature on your drive or provide battery backup and a proper
shutdown procedure during power loss.
{{/1.4.0-}}

Default: `false`

```erlang
{eleveldb, [
    ...,
    {sync, false},  %% do not write()/fsync() every time
    ...
]}
```

<div id="Write-Buffer-Size"></div>
#### Write Buffer Size

Each vnode first stores new key/value data in a memory based write buffer.  This write buffer is in parallel to the recovery log mentioned in the "sync" parameter.  Riak creates each vnode with a randomly sized write buffer for performance reasons.  The random size is somewhere between `write_buffer_size_min` and `write_buffer_size_max`.

If unspecified in [[app.config|Configuration Files]], eLevelDB will default to a `write_buffer_size_min` of 31,457,280 Bytes (30 MB) and `write_buffer_size_max` of 62,914,560 Bytes (60 MB).  In this case, the average write buffer will be 47,185,920 bytes (45 MB).

Other portions of Basho's leveldb tuning assume these two values have the default values. Changing the values higher or lower will likely impact overall write throughput in a negative fashion.

```erlang
{eleveldb, [
    ...,
    {write_buffer_size_min, 31457280},  %% 30 MB in bytes
    {write_buffer_size_max, 62914560},  %% 60 MB in bytes
    ...
]}
```

If you choose to change the write buffer size by setting `write_buffer_size_min` and `write_buffer_size_max`, `write_buffer_size_min` must be at least 30 MB, and `write_buffer_size_min` should be about half the size of `write_buffer_size_max`.

If you wish to set all write buffers to the same size, use the `write_buffer_size` parameter.  This will override the `write_buffer_size_min` and `write_buffer_size_max` parameters.  This is not recommended.

Larger write buffers increase performance, especially during bulk loads.  Up to two write buffers may be held in memory at the same time, so you may wish to adjust this parameter to control memory usage.

### Read Performance

These parameters can be set/adjusted to increase read performance.

The `block_size` and `block_restart_interval` parameters determine how leveldb will organize the key space within each `.sst` table file.  The defaults are very good for all researched cases and therefore recommended.

#### Bloomfilter

Each database `.sst` table file can include an optional "bloom filter" that is highly effective in shortcutting data queries that are destined to not find the requested key. The `bloom_filter` typically increases the size of an `.sst` table file by about 2%. This option must be set to `true` in the app.config to take effect.

Default: `true`

```erlang
{eleveldb, [
    ...,
    {use_bloomfilter, true},
    ...
]}
```

#### Block Size

{{#1.4.0+}}
`sst_block_size` defines the size threshold for a block / chunk of data within one `.sst` table file.  Each new block gets an index entry in the `.sst` table file's master index.
{{/1.4.0+}}
{{#1.4.0-}}
Approximate size of user data packed per block. For very large databases
bigger block sizes are likely to perform better so increasing the block size
to 256k (or another power of 2) may be a good idea. Keep in mind that
LevelDB's default internal block cache is only 8MB so if you increase the
block size you will want to resize `cache_size` as well.
{{/1.4.0-}}

Default: `4096` (4K)

{{#1.3.0-}}

```erlang
{eleveldb, [
    ...,
    {block_size, 4096},  %% 4K blocks
    ...
]}
```

<div class="note"><div class="title">Block size in Riak 1.2</div>
<p>Is it not recommended to change block size from default in Riak 1.2.  Block sizes
larger than 4K can hurt performance.</p>
</div>

{{/1.3.0-}}
{{#1.3.0+}}

```erlang
{eleveldb, [
    ...,
    {sst_block_size, 4096},  %% 4K blocks
    ...
]}
```
{{/1.3.0+}}

#### Block Restart Interval

`block_restart_interval` defines the key count threshold for a new key entry in the key index for a block.

Most clients should leave this parameter alone.

Default: `16`

```erlang
{eleveldb, [
	    ...,
      {block_restart_interval, 16}, %% # of keys before restarting delta encoding
	    ...
]}
```

### Database Integrity

The `verify_checksums` and `verify_compactions` options control whether or not each data block from the disk is first validated via a CRC32c checksum.  The tradeoff is this:  disk read operations are slightly faster without the CRC32c validation, but it is quite likely Riak will crash if a corrupted block is passed from the disk to leveldb unvalidated.

#### Verify Checksums

`verify_checksums` controls whether or not validation occurs when Riak requests data from the leveldb database on behalf of the user.

{{#1.4.0-}}
Default: `false`
{{/1.4.0-}}
{{#1.4.0+}}
Default: `true`
{{/1.4.0+}}

```erlang
{eleveldb, [
    ...,
    {verify_checksums, true}, %% make sure data is what we expected it to be
    ...
]}
```

#### Verify Compaction

`verify_compaction` controls whether or not validation occurs when leveldb reads data as part of its background compaction operations.

Default: `true`

```erlang
{eleveldb, [
    ...,
    {verify_compaction, true},
    ...
]}
```


## Parameter Planning

The following steps walk you through setting parameters and evaluating how much working memory (i.e. RAM) you'll need for a given levelDB implementation.

### Step 1:  Calculate Available Working Memory

Current unix-like systems (Linux / Solaris / SmartOS) use physical memory that is not allocated by programs as buffer space for disk operations.  In Riak 1.2, levelDB is modeled to depend upon this Operating System (OS) buffering.  You must leave 25-50% of the physical memory available for the operating system (25-35% if servers have Solid State Drive (SSD) arrays, 35-50% if servers have spinning hard drives).

levelDB working memory is calculated simply as the memory not reserved for the OS.

```bash
leveldb_working_memory = server_physical_memory * (1 - percent_reserved_for_os)
```

Example:

 If a server has 32G RAM and we wish to reserve 50%,

```bash
leveldb_working_memory = 32G * (1 - .50) = 16G
```

### Step 2: Calculate Working Memory per vnode

Riak 1.2 configures / assigns memory allocations by vnode.  To calculate the vnode working memory, divide levelDB's total working memory by the number of vnodes.

```bash
vnode_working_memory = leveldb_working_memory / vnode_count
```

Example:

 If a physical server contains 64 vnodes,
```bash
vnode_working_memory = 16G / 64 = 268,435,456 Bytes per vnode
```


### Step 3: Estimate Memory Used by Open Files

There are many variables that determine the exact memory any given file will require when open.  The formula below gives an approximation that should be accurate within 10% for moderately large levelDB implementations.

```bash
open_file_memory = (max_open_files-10) * (184 + (average_sst_filesize/2048) * (8 + ((average_key_size+average_value_size)/2048 +1) * 0.6)
```

 If a physical server contains 64 vnodes and the parameter values in the table below,

```bash
open_file_memory =  (150-10)* (184 + (314,572,800/2048) * (8+((28+1024)/2048 +1)*0.6 = 191,587,760 Bytes
```

Example:

<table class="centered_table">
    <tr>
        <th>Parameter</th>
        <th>Value</th>
    </tr>
    <tr>
        <td>max_open_files</td>
        <td>150</td>
    </tr>
    <tr>
        <td>average_sst_filesize</td>
        <td>314,572,800 Bytes</td>
    </tr>
    <tr>
        <td>average_key_size</td>
        <td>28 Bytes</td>
    </tr>
    <tr>
        <td>average_value_size</td>
        <td>1,024 Bytes</td>
    </tr>
    <tr>
        <td>Total</td>
        <td>191,587,760 Bytes</td>
    </tr>
</table>
<br>


### Step 4: Calculate Average Write Buffer

Calculate the average of ```write_buffer_size_min``` and ```write_buffer_size_max``` (see [[write buffer size|LevelDB#Write-Buffer-Size]] for more  on these parameters).  The defaults are 31,457,280 Bytes (30 MB) and 62,914,560 Bytes (60 MB), respectively.  Therefore the default average is 47,185,920 Bytes (45 MB).


### Step 5: Calculate vnode Memory Used

The estimated amount of memory used by a vnode is the sum of:
<ul>
  	<li>average_write_buffer_size (from Step 4)</li>
	<li>cache_size (from [[app.config|Configuration Files]])</li>
	<li>open_file_memory (from step 3)</li>
	<li>20 MB (for management files)</li>
</ul>

Example:
<table>
    <tr>

        <th>Parameter</th>
        <th>Bytes</th>
    </tr>
    <tr>
        <td>average write buffer size</td>
        <td>47,185,920</td>
    </tr>
    <tr>
        <td>cache size</td>
        <td>8,388,608</td>
    </tr>
    <tr>
        <td>open files</td>
        <td>191,587,760</td>
    </tr>
    <tr>
        <td>management files</td>
        <td>20,971,520</td>
    </tr>
    <tr>
        <td>Total</td>
        <td>268,133,808 (~255 MB)</td>
    </tr>
</table>

### Step 6: Compare Step 2 and Step 5 and Adjust Variables

Example:
In Step 2 we calculated a working memory per vnode of 268,435,456 Bytes.  In Step 5, we estimated vnodes would consume approximately 268,133,808 Bytes.  Step 2 and step 5 are within 301,648 Bytes (~300 kB) of each other.  This is exceptionally close, but happens to be more precise than really needed.  The values are good enough when they are within 5%.

The above calculations are automated in this [memory model spreadsheet](data/LevelDB1.2MemModel_calculator.xls).

## Tuning LevelDB

While eLevelDB can be extremely fast for a durable store, its performance
varies based on how you tune it. All the configuration is exposed via
application variables in the `eleveldb` application scope.

<div id="Tips-Tricks"></div>
### Tips & Tricks:

  * __Be aware of file handle limits__

    You can control the number of file descriptors eLevelDB will use with
    `max_open_files`. eLevelDB configuration is set to 20 per partition (which
    is both the default and minimum allowed value) which means that in a
    cluster with 64 partitions you'll have at most 1280 file handles in use at
    a given time. This can cause problems on some platforms (e.g. OS X has a
    default limit of 256 handles). The solution is to increase the number of
    file handles available. Review the (open files
    limitations)(Open-Files-Limit) information.

  * __Avoid extra disk head seeks by turning off `noatime`__

    eLevelDB is very aggressive at reading and writing files. As such, you
    can get a big speed boost by adding the `noatime` mounting option to
    `/etc/fstab`. This will disable the recording of the "last accessed time"
    for all files. If you need last access times but you'd like some of the
    benefits of this optimization you can try `relatime`.

```bash
/dev/sda5    /data           ext3    noatime  1 1
/dev/sdb1    /data/inno-log  ext3    noatime  1 2
```

### Recommended Settings

Below are **general** configuration recommendations for Linux distributions.  Individual users may need to tailor these settings for their application.

For production environments, we recommend the following settings within ```/etc/syscfg.conf```:

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
Beginning with the 2.6 kernel, Linux gives you a choice of four I/O [elevator models](http://www.gnutoolbox.com/linux-io-elevator/).  We recommend using the NOOP elevator.  You can do this by changing the scheduler on the Linux boot line: ```elevator=noop```.

#### ext4 Options
The ext4 file system defaults include two options that increase integrity but slow performance.  Because Riak's integrity is based on multiple nodes holding the same data, these two options can be changed to boost levelDB's performance.  We recommend setting: ```barrier```=0 and ```data```=writeback.

#### CPU Throttling
If CPU throttling is enabled, disabling it can boost levelDB performance in some cases.

#### No Entropy
If you are using https protocol, the 2.6 kernel is widely known for stalling programs waiting for SSL entropy bits.  If you are using https, we recommend installing the [HAVEGE](http://www.irisa.fr/caps/projects/hipsor/) package for pseudorandom number generation.

#### clocksource
We recommend setting "clocksource=hpet" on your linux kernel's ```boot``` line.  The TSC clocksource has been identified to cause issues on machines with multiple physical processors and/or CPU throttling.

#### swappiness
We recommend setting ```vm.swappiness```=0 in ```/etc/sysctl.conf```.  The vm.swappiness default is 60, which is aimed toward laptop users with application windows.  This was a key change for mysql servers and is often referenced on db performance.



## FAQ

  * As of Riak 1.0 use of secondary indexes (2I) requires that the bucket
    being indexed be configured to use eLevelDB.

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

Below there are two directory listings showing what you would expect to find on
disk when using eLevelDB. In this example we use a 64 partition ring which
results in 64 separate directories, each with their own LevelDB database.

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

After performing a large number "put" (write) operations the Riak cluster
running eLevelDB will look something like this.

```bash
gburd@toe:~/Projects/riak/dev/dev1/data$ tree leveldb/
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
