---
title: "Potential data loss on restart with LevelDB tiered storage"
description: ""
project: community
menu:
  community:
    name: "Potential Data Loss with LevelDB Tiered Storage"
    identifier: "datalossleveldb"
    weight: 250
    parent: "productadvisories"
toc: true
---

Data loss may occur during node restart when using LevelDB tiered storage on a low write volume cluster.

Info | Value
:----|:-----
Date issued | March 2nd 2016
Product | Riak KV & Riak TS
Affected KV versions | 2.0.0+ and 2.1.0+
Affected TS versions | 1.0.0+

## Overview

This issue is limited to customers using LevelDB tiered storage. There is a recognized error in the LevelDB tiered storage subsystem whereby if less than 60MB of data is written per vnode before Riak is restarted, the data will be unavailable for reads. The data is written to an incorrectly located recovery log that is not found on restart. Once more than 60MB of data is written to the vnode, no data will be lost upon restart.

## Description

When using LevelDB tiered storage as the backend for Riak, LevelDB creates the first (and only the first) recovery log per vnode, 0000xxxx.log, in an incorrect location. This first recovery log file is only used by LevelDB if the Riak server restarts prior to committing it permanently. LevelDB obsoletes a recovery file once it writes the newly arrived data to a long term storage file (an .sst table file). All subsequent recovery files exist in the location anticipated by LevelDB's startup procedures. The data loss is therefore limited to the contents of this first recovery log and only if LevelDB has not subsequently rewritten the data to long term storage.

The only symptom of this issue is that the data within the first recovery is unavailable for reads after a restart. However, Riak has several resiliency features that mitigate the likelihood of the read failure. Riak defaults to a replication factor of n_val = 3. This means that Riak is writing the data to 3 different locations. Therefore, all 3 locations must restart within the same short period for data loss to occur. Otherwise, Riak will automatically correct individual nodes with missing data from other nodes via its read-repair and/or AAE features.

## Affected Users

This issue will affect you if ALL of these conditions are true:

* You are using the LevelDB backend, AND
* LevelDB is configured to use tiered storage (leveldb.tiered settings in riak.conf), AND
* All Riak nodes responsible for the n_val copies of the data are restarted before LevelDB rewrites the initial recovery log into a permanent .sst table file. (60MB data per vnode).

## Mitigation Strategy

This issue can be mitigated in an existing Riak installation by creating a soft link between the incorrect log location and the fast tier directory. The same steps can be used if you need to create a fresh install with tiered storage before starting Riak the first time.

To mitigate the issue, follow these steps:

1. Identify where the first LevelDB files will be written to - look in leveldb.data_root
2. Move the existing leveldb.data_root directory out of the way
3. Identify where the fast tiered storage directory is - {leveldb.tiered.path.fast}/{leveldb.data_root}
4. Create a symbolic link from the fast path data_root to the standard data_root.

### Step-by-Step Instructions

1. Locate where the first LevelDB log files are being written to. To do so, look at the `Set Value`:

    ```
    $ riak config describe leveldb.data_root
    Documentation for leveldb.data_root
    Where LevelDB will store its data.

       Valid Values:
         - the path to a directory
       Default Value : $(platform_data_dir)/leveldb
       Set Value     : leveldb
       Internal key  : eleveldb.data_root
    ```

    If the `Set Value` includes a parameter like $(platform_data_dir), as shown in the below example, you will need to look it up with another riak config describe. If the value is 'Value not set' then you will need to use the default value of `$(platform_data_dir)/leveldb` and do the `platform_data_dir` lookup:

	```
    $ riak config describe leveldb.data_root
    Documentation for leveldb.data_root
    Where LevelDB will store its data.

       Valid Values:
         - the path to a directory
       Default Value : $(platform_data_dir)/leveldb
       Set Value     : $(platform_data_dir)/leveldb
       Internal key  : eleveldb.data_root
    ```

    You would look up the value of `platform_data_dir like this:

    ```
    $ riak config describe platform_data_dir
    Documentation for platform_data_dir
    Platform-specific installation paths (substituted by rebar)

       Valid Values:
         - the path to a directory
       Default Value : /var/lib/riak
       Set Value     : /var/lib/riak
       Internal key  : riak_core.platform_data_dir
    ```

    You would then combine the two paths, from the two examples immediately above, giving you a path of /var/lib/riak/leveldb.

    If the path is relative (as shown in the very first example), then it will be relative to the working directory Riak starts in. The working directory Riak starts in is identified by the `RUNNER_BASE_DIR` value found in your env.sh file, located in your Riak library directory. You may grep for its value:

	```
    $ grep ^RUNNER_BASE_DIR /usr/lib64/riak/lib/env.sh
    RUNNER_BASE_DIR=/usr/lib64/riak
    ```

    This combined with the relative path in the very first example would provide an example path of /usr/lib64/riak/leveldb.

    Once located, verify the directory only contains .log files. If it contains anything other than .log files, please recheck the steps above. If the result is the same, contact the riak-users mailing list.

    You can quickly check whether any files, other than .log files, exist using the following:

    ```
    # As root or sudo
    LOGDIR=/path/you/worked/out/in/step1 find ${LOGDIR} -type f  # List of log files
    find ${LOGDIR} -type f | grep -v '\.log$'  # should be - empty output
    ```

    In the very unlikely case the leveldb.tiered.path.slow is set to "." (dot - the self-link to a directory) and the leveldb.data_root path is a relative path, then you are not able to use this workaround.

2. Move the existing log directory to one side:

    ```
    # As root or sudo
    mv ${LOGDIR} ${LOGDIR}.bak
    ```

3. Identify the fast tier path. First we need to identify the fastdir path. To do so, look at the `Set Value`:

    ```
    $ riak config describe leveldb.tiered.path.fast
    Documentation for leveldb.tiered.path.fast
    leveldb can be configured to use different mounts for
    different levels. This tiered option defaults to off, but you can
    configure it to trigger at levels 1-6. If you do this, anything
    stored at the chosen level or greater will be stored on
    leveldb.tiered.mounts.slow, while everything at the levels below will
    be stored on leveldb.tiered.mounts.fast
    Levels 3 or 4 are recommended settings.
    WARNING: There is no dynamic reallocation of leveldb
    data across mounts. If you change this setting without manually
    moving the level files to the correct mounts, leveldb will act in
    an unexpected state.

       Valid Values:
         - the path to a directory
       No default set
       Set Value     : /ssd/riak
       Internal key  : eleveldb.tiered_fast_prefix
    ```

    Append the leveldb.data_root path (identified in the first example in Step 1) to the fast path to find the location of the LevelDB files. For example, if leveldb.data_root is the relative path 'leveldb', we would append that to the `Set Value` of /ssd/riak returned in the above example, for a full path of: /ssd/riak/leveldb.

    Check that LevelDB MANIFEST files are present to make sure it is the correct directory:

	```
    FASTDIR=/path/to/fast/dir/and/leveldb/basedir   # e.g. /ssd/riak/leveldb from above
    find $FASTDIR -name 'MANIFEST*'
    # Check there are multiple files returned
      ```

4. Finally, we will create a symbolic link from the fast tier path (identified in step 3) to the log directory (identified in step 1).

    ```
    # As root or sudo
    ln -s ${FASTDIR} ${LOGDIR}
    ```

    Make sure the Riak user has read/write permissions to that directory.