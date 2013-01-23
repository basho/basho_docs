---
title: Innostore
project: riak
version: 1.2.0-
document: tutorials
toc: true
audience: intermediate
keywords: [backends, planning, innostore]
prev: "[[Multi]]"
up:   "[[Choosing a Backend]]"
interest: false
---

<div class="note">
<div class="title">Warning</div>
As of Riak version 1.2, the Innostore backend is deprecated and no longer supported.
</div>

## Overview

[Innostore](https://github.com/basho/innostore) is an Erlang application that
provides an API for storing and retrieving key/value data using the
[InnoDB](http://www.innodb.com/products/innodb/) storage system. This storage
system is the same one used by [MySQL](http://www.mysql.com) for reliable,
transactional data storage. It’s a proven, fast system and perfect for use with
Riak if you have a large amount of data to store. Let’s take a look at how you
can use Innostore as a backend for Riak.

### Strengths:

  * Very good performance when configured/tuned for your use case

    InnoDB can be very fast when configured properly.  Depending on your
    configuration and access patterns InnoDB can perform within very strict
    SLAs.

  * Able to manage more keys than can fit into memory

    InnoDB caches as much of the active dataset (keys, values, index
    information and locks) in memory as will fit in the buffer pool space
    allocated (`buffer_pool_size`).  As the active data set changes so does the
    in-memory cache.  InnoDB can operate in conditions where the active data
    exceeds the available memory (buffer size).  It does this by maintaining an
    LRU cache in memory of as much data as possible.  In cases where your
    active data set exceeds memory you will experience additional latency as
    InnoDB fetches data from disk.

  * Highly tunable

    InnoDB has grown to have a lot of tuning parameters over the years.

### Weaknesses:

  * GPL rather than Apache license

    There are additional restrictions and requirements placed on your use of
    Innostore because InnoDB is licensed under the GPL.  This is the reason
    that we do not bundle the Innostore backend with the rest of Riak.

  * Embedded InnoDB is no longer supported by InnoBase Oy (Oracle)

    InnoDB has been around for many years as the sole product of InnoBase Oy
    which was acquired by Oracle in November of 2006, long before the
    acquisition of Sun (which included MySQL).  The "Embedded InnoDB" product
    was was dropped shortly after the acquisition and integration of Sun and
    MySQL.  Innostore is based on the last release of Embedded InnoDB with some
    additional patches and updates but is not fully up to date with the current
    state of the InnoDB code distributed within MySQL.  This divergence and
    lack of ongoing support makes us believe that Oracle is no longer
    interested in any use of InnoDB outside of MySQL.

  * Key's must be < 255 bytes

    Although Innostore does support arbitrarily large numbers of keys within a
    bucket it does not support lengthy keys.  Keep this in mind when using this
    storage engine.

  * Recovery process can take some time if log files are large enough

    Depending on the size and number of log file segments InnoDB recovery (the
    process of reviewing files to ensure data integrity during start-up) can
    take some time.

  * Bulk insert should be pre-sorted to maintain a balanced btree

    Due to the nature of btrees and the implementation of said in InnoDB if you
    are going to insert a large amount of data (such as a bulk load process)
    then it is best if you pre-sort it before inserting.  This prevents a
    degenerate case where btrees become highly unbalanced causing performance
    to suffer.

  * On-disk overhead

    InnoDB incurs a fair amount of overhead on disk due to the way it
    constructs and maintains index information.  Expect up to 2x data bloat
    plus the overhead of log files at each node running Innostore.

  * Log/data contention demands separate disks

    InnoDB's default is to synchronize data to disk when writing and to bypass
    operating system buffers.  This results in more than one write at different
    locations on a single drive causing each update to incur the overhead of at
    least one disk seek.  Avoiding this is key when tuning for high performance
    environments and that generally involves separating data and log files onto
    different drives.

  * Highly tunable

    InnoDB has grown to have a lot of tuning parameters over the years, it can
    be hard to know that you've achieved maximum performance without a lot of
    testing various combinations under real application load.

## Installing Innostore

Unlike the other storage engines, Innostore is distributed separately from
Riak.  We recommend using a pre-packaged binary distribution but you can also
build it from source code.

### Installing from pre-built packages

Determine your system and architecture (such as "Ubuntu" and "64bit Intel") and
then find the latest download package.
[http://downloads.basho.com/](http://downloads.basho.com/innostore/CURRENT) Use
your browser or a tool such as `wget` or `curl` to download the package and
then install the package using the proper package manager.

```bash
# Example: Ubuntu x64 or other Debian-based Linux distributions:
$ cd /tmp
$ wget http://downloads.basho.com/innostore/innostore-1.0.3/innostore-1.0.3-1-deb-x86_64.tar.gz
$ cd /usr/lib/riak/lib
$ sudo tar -xvzf /tmp/innostore-1.0.3-1-deb-x86_64.tar.gz
$ sudo chown -R riak.riak innostore-1.0.3
$ rm /tmp/innostore-1.0.3-1-deb-x86_64.tar.gz

# Example: RedHat x64 or other RedHat-based Linux distributions:
$ cd /tmp
$ wget http://downloads.basho.com/innostore/innostore-1.0.3/innostore-1.0.3-1-rh-x86_64.tar.gz
$ cd /usr/lib/riak/lib
$ sudo tar -xvzf /tmp/innostore-1.0.3-1-rh-x86_64.tar.gz
$ sudo chown -R riak.riak innostore-1.0.3
$ rm /tmp/innostore-1.0.3-1-rh-x86_64.tar.gz

# Example: Fedora Core x64 and related Linux distributions:
$ cd /tmp
$ sudo wget http://downloads.basho.com/innostore/innostore-1.0.3/innostore-1.0.3-1-fedora-x86_64.tar.gz
$ cd /var/lib/riak/lib
$ sudo tar -xvzf /tmp/innostore-1.0.3-1-fedora-x86_64.tar.gz
$ sudo chown -R riak.riak innostore
$ rm /tmp/innostore-1.0.3-1-fedora-x86_64.tar.gz

# etc...
```

### Building and installing from source code

 * You will need to have [[Erlang installed|Installing-Erlang]] to
    compile Innostore.

 * Obtain the source
    code.
    
   - You can either obtain the source code
     [as a package](http://downloads.basho.com/innostore/innostore-1.0.3/innostore-1.0.3.tar.gz).

     ```bash
     # Example: downloading the code
     $ wget http://downloads.basho.com/innostore/innostore-1.0.3/innostore-1.0.3.tar.gz
     $ tar -xzf innostore-1.0.3.tar.gz
     $ cd innostore-1.0.3
     ```

   - or directly from the [source code repository](http://github.com/basho/innostore).

    ```bash
    # Example: clone the Git repository
    $ git clone http://github.com/basho/innostore
    $ cd innostore
    $ git checkout innostore-1.0.3
    ```

 * Now that you have the code,
   let's build it:
   
    ```bash
    # NOTE: on a Single CPU system you'll need to enable Erlang to run SMP before building
    $ export ERL_FLAGS="-smp enable"
    $ make
    ```
  
 * Install innostore

   If your compile passed all the tests you are now ready to install Innostore
   into your Riak distribution.

   - Option 1: into your Erlang distribution:
   If you are using a copy of Riak you compiled yourself you can install
   Innostore by issuing the following command replacing $RIAK with the location
   of your Riak install:
   
   ```bash
   $ make install
   ```

   - Option 2: into an existing Riak build:
   If you are using a copy of Riak you compiled yourself you can install
   Innostore by issuing the following command replacing $RIAK with the location
   of your Riak install:

   ```bash
   $ RIAK=/usr/lib/riak make install
   ```
   
   Which would result in the following files being installed into `/usr/lib/riak/lib/innostore-1.0.3`

   ```
   innostore-1.0.3/
   |-- ebin
   |   |-- innostore.app
   |   |-- innostore.beam
   |   |-- innostore_riak.beam
   |   `-- riak_kv_innostore_backend.beam
   |-- priv
   |   |-- innodump
   |   |-- innoload
   |   |-- innostore_drv.so
   |   `-- riak-innostore
   `-- src
       |-- innostore.erl
       |-- innostore_riak.erl
       `-- riak_kv_innostore_backend.erl
   ```

## Configuring Innostore

There are two steps to configure Riak to use Innostore.

1. Edit your Riak installation's `app.config` file
   Change the `storage_backend` setting to `riak_kv_innostore_backend`.

    ```erlang
    {storage_backend, riak_kv_innostore_backend}
    ```

2. While the defaults should work just fine you may want to modify the location
   of the Innostore database files.  To do that you first add an `innostore`
   application section to the `riak/etc/app.config` file.  Modify your
   `data_home_dir` and `log_group_home_dir` paths as needed.

    ```erlang
    {innostore, [
        {data_home_dir, "/var/lib/riak/innodb"}, %% Where data files reside
        {log_group_home_dir, "/var/lib/riak/innodb/logs"}, %% Where log files reside
        {buffer_pool_size, 2147483648} %% 2GB of buffer
    ]}
    ```

3. Now that you have configured Innostore you can test your install by
   connecting to the Riak console and watching for messages about Innostore
   `sudo /usr/sbin/riak console`. If the install was successful you will
   see something similar to:

     ```bash
     100220 16:36:58 InnoDB: highest supported file format is Barracuda.
     100220 16:36:58 Embedded InnoDB 1.0.3.5325 started; log sequence number 45764
     ```

## Tuning Innostore

While InnoDB can be extremely fast for a durable store, its performance
is highly dependent on tuning the configuration to match the hardware and
dataset. All the configuration is available as application variables in
the `innostore` application scope.

### Tips & Tricks:

  * __Store data and logs files on separate spindles/drives__

    In general, only the first three parameters (`data_home_dir`,
    `log_group_home_dir` and `buffer_pool_size`) will need to be changed. It is
    _strongly recommended_ that the data home dir and log home dir are kept on
    _separate spindles/drives_.  For best performance, we recommend putting
    Innostore's log data on a separate hard disk from the actual data.  This
    will also make its data more resilient to hardware failure -- corrupted
    writes to the data files can often be recovered from the information in the
    transaction logs.

    ``` erlang
    {innostore, [
        ...,
               {data_home_dir,            "<path to device for data files>"},
               {log_group_home_dir,       "<path to a different device for log files>"},
         ...
    ]}
    ```

  * __Size the transaction log appropriately__

    When storing binary objects or working with a cluster the number of log
    files as well as their size should be increased to handle the additional
    amount of data being stored in Innostore. The following error exemplifies
    the log messages that will be seen if your log setup can't cope with the
    amount of data.

    ``` bash
    InnoDB: ERROR: the age of the last checkpoint is 30209814,
    InnoDB: which exceeds the log group capacity 30195303.
    InnoDB: If you are using big BLOB or TEXT rows, you must set the
    InnoDB: combined size of log files at least 10 times bigger than the
    InnoDB: largest such row.
    ```

    To fix this issue the following log settings will work for most environments:
    
    ``` erlang
    {innostore, [
        ...,
    {log_files_in_group, 6},  %% How many files you need, usually 3 < x < 6
    {log_file_size, 268435456},  %% No bigger than 256MB, otherwise recovery takes too long
         ...
    ]}
    ```

  * __Maximize the buffer pool size__

    The `buffer_pool_size` determines how much data InnoDB tries to keep in
    memory at once. Obviously, the more of your dataset that can fit in RAM,
    the better InnoDB will perform. If you are running a 64-bit Erlang VM, the
    `buffer_pool_size` can safely be set above 2G.  Unlike Bitcask InnoDB keeps
    keys and values in the buffer pool cache, this allows for management of key
    spaces that are larger than available memory.  We recommend that you set
    this to be 60-80% of available RAM (after subtracting RAM consumed by other
    services).  For example, on a 12GB machine you might set it to 8GB:

    ```erlang
    {innostore, [
        ...,
        {buffer_pool_size, 8589934592} %% 8 * 1024 * 1024 * 1024
        ...
    ]}
    ```

  * __Double buffering only wastes RAM when using InnoDB__

    On Linux and other Unix-like platforms, setting
    [InnoDB's](http://dev.mysql.com/doc/refman/5.6/en/innodb-parameters.html#sysvar_innodb_flush_method)
    `flush_method` to `O_DIRECT` will bypass a layer of filesystem buffering
    provided by the operating system.  Turing this off is important because
    InnoDB manages its own buffer cache (unlike Bitcask for instance).

    ```erlang
    {innostore, [
        ...,
        {flush_method, "O_DIRECT"}
        ...
    ]}
    ```

  * __Be aware of file handle limits__

    You can control the number of file descriptors InnoDB will use with
    `open_files`.  InnoDB defaults to 300 which can cause problems on some
    platforms (e.g. OS X has a default limit of 256 handles).  Innostore opens
    a file for each partition/bucket combination, plus several for its
    transaction log files. Each of these count against the total number of
    files any one program may have open.  As a result, you may need to adjust
    this number up or down from its default to accommodate a lower limit, or
    more open buckets.  The default is `300`.  The other option, really the
    preferred option for production, is to increase the number of file handles
    available.

    ```erlang
    {innostore, [
        ...,
              {open_files, 100} %% accommodate a lower limit
        ...
    ]}
    ```

  * __Avoid extra disk head seeks by turning off `noatime`__

    Innostore is very aggressive at reading and writing files.  As such, you
    can get a big speed boost by adding the `noatime` mounting option to
    `/etc/fstab`.  This will disable the recording of the "last accessed time"
    for all files.  If you need last access times but you'd like some of the
    benefits of this optimization you can try `relatime`.

    ```bash
    /dev/sda5    /data           ext3    noatime  1 1
    /dev/sdb1    /data/inno-log  ext3    noatime  1 2
    ```

  * __ZFS__

    A great deal of
    [thought](http://blogs.oracle.com/realneel/entry/mysql_innodb_zfs_best_practices)
    has been given to [performance tuning MySQL using InnoDB on ZFS
    filesystems](http://www.mysqlconf.com/mysql2009/public/schedule/detail/7121).
    Most of the recommendations carry over directly for use of Riak with
    Innostore on ZFS.  The most critical ZFS option we've found is to set the
    `recordsize=16k` on the pool where `data_home_dir` lives (prior to starting
    innostore) and `recordsize=128k` on the `log_group_home_dir`.  This matches
    the ZFS recordsize with InnoDB page size (16KB for datafiles, and 128KB for
    InnoDB log files).

    On your `data_home_dir` pool you might also consider setting
    `primarycache=metadata`.  When ZFS reads a block from a disk, it inflates
    the I/O size, hoping to pull interesting data or metadata from the disk.
    For workloads that have an extremely wide random reach into 100s of TB with
    little locality, then even metadata is not expected to be cached
    efficiently.[1](http://www.solarisinternals.com/wiki/index.php/ZFS_Evil_Tuning_Guide)

  * __Adaptive Flushing__

    Write-heavy workloads can reach a situation where InnoDB runs out of usable
    space in its log files.  When that happens, InnoDB does a lot of disk
    writes to create new space and, as a result, you will see a drop in server
    throughput for a few seconds.  This non-uniform overhead often shows up as
    repeated spikes affecting latency and/or throughput.  One potential way to
    address this is to turn on
    `adaptive_flushing`.[1](http://blogs.innodb.com/wp/2010/09/mysql-5-5-innodb-adaptive_flushing-how-it-works/)
    [2](http://www.innodb.com/wp/products/innodb_plugin/plugin-performance/innodb-plugin-1-0-4-adaptive-flushing-oltp-test-dbt2/)
    [3](http://www.mysqlperformanceblog.com/2009/12/04/effect-of-adaptive_flushing/)

    ```erlang
    {innostore, [
        ...,
              {adaptive_flushing, true} %% Enable adaptive flushing
        ...
    ]}
    ```

  * __InnoDB Table Format__

    Embedded InnoDb offers several table formats: `compact`, `dynamic` and
    `compressed`.

    - `compact` format stores the first 768 bytes of the value with the
      key and any extra data on extension pages.  This is a good option
      for small values.
    - `dynamic` format stores the value outside separately from the
      index.  For larger values (>600 bytes) this will make the index
      pages denser and may provide a performance increase.
    - `compressed` format is like dynamic format, except it compresses
      the key and data pages.

    The default format is `compact` to match previous innostore releases. To
    configure, set the `format` option in your innostore config.  The setting
    is system wide and will be used for all buckets.

    ```erlang
    {innostore, [
        ...,
        {format, dynamic} %% Use dynamic format tables.
        ...
    ]}
    ```

    If your dataset is likely to compress well, then you can enable compression.

    ```erlang
    {innostore, [
        ...,
        {format, compressed}, %% Use compressed, dynamic format tables.
        ...
    ]}
    ```

## Innostore Implementation Details

The Innostore backend creates separate tables for each bucket.  These tables
have two columns; key and value.  The `key` is a VARBINARY limited to 255
bytes.  This means that keys must be no larger than 255 bytes when you use the
Innostore backend.  The `value` column is a BLOB, it can store data of any
size.  One clustered primary key index is created on the `key` column to speed
up access to data.

All operations are performed within transactions but the degree of
serialization differs across operations.  Gets (reads) are performed within the
context of a repeatable read (IB_TRX_REPEATABLE_READ).  Puts (insert or update)
are within a serializable transaction (IB_TRX_SERIALIZABLE).  Delete also
occurs within a serializable transaction.

### Innostore Database Files

Here's what you can expect to see on disk when running Innostore.  First, let's
agree on the following as our configuration for Innostore in our `app.config`
file.

```erlang
%% Innostore Config
{innostore, [
    {data_home_dir, "/var/lib/riak/innodb"}, %% Where data files go
    {log_group_home_dir, "/var/lib/riak/innodb/logs"}, %% Where log files go
    {flush_method, "O_DIRECT"}, %% Prevent double buffering of filesystem blocks
    {buffer_pool_size, 1073741824}, %% 2GiB
    {log_files_in_group, 6},  %% How many files you need, usually 3 < x < 6
    {log_file_size, 268435456},  %% No bigger than 256MB, otherwise recovery takes too long
    {open_files, 2048}, %% Restrict the number of open file handles available to Innostore
    {adaptive_flushing, true}  %% Enable adaptive flushing
    {format, compressed}, %% Use compressed, dynamic format tables.
]},
```

When you first start up Riak (`riak start`) there will be no evidence of the
Innostore/InnoDB database files.  It isn't until the first access to Riak that
the files are created.  Once that happens you can expect a short lag as the
Innostore backend initializes the data and log files resulting in the following
layout:

```
innodb/
|-- ibdata1
|-- innokeystore
`-- logs
    |-- ib_logfile0
    |-- ib_logfile1
    |-- ib_logfile2
    |-- ib_logfile3
    |-- ib_logfile4
    `-- ib_logfile5
```

Every time thereafter you will experience a short delay (generally a few
seconds) at startup while InnoDB runs recovery to ensure that the database
files are up to date and consistent on disk according to the latest information
in the log files.

Conceptually, there are three categories of data being stored for each bucket
managed by Innostore; the keys, the values and the index into the keys to speed
lookup.

The file `ibdata1` is the first in a set of files with names such as `ibdata1`,
`ibdata2`, and so on, that make up the InnoDB system tablespace. These files
contain metadata about InnoDB tables, and can contain some or all of the table
data also (depending on whether the file-per-table option is in effect when
each table is created).  Innostore index and value date along with InnoDB
metadata all end up in the `ibdata1..n` files.  This is where all values and
indexes live for all buckets.

The file `innokeystore` contains the keys.  They are in a separate database
file to improve the cache hit rate on sequential key access.

More documentation on the Embedded InnoDB library can be found
[here](http://www.innodb.com/doc/embedded_innodb-1.0/).
