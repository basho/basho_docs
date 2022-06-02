---
title: "LevelDB SEGV in Riak KV 2.1.3"
description: ""
menu:
  community:
    name: "LevelDB Segfault"
    identifier: "leveldbsegv"
    weight: 120
    parent: "productadvisories"
toc: true
aliases:
  - /riak/latest/community/product-advisories/leveldbsegfault/
---


# Overview

Riak KV (aka Riak / Riak EE) 2.1.3 has a race condition with opening LevelDB databases and may crash with a segmentation violation (SIGSEGV). LevelDB databases are opened on primary or fallback vnode startup and during AAE expiration  The issue is possible but much less likely on the Riak TS 2.0, Riak KV 2.0 and earlier Riak KV 2.1 series.


# Description

Beginning with Riak KV 2.1.3, a list of opened databases is used by the GroomingCompactions feature to look for databases with low write activity that can benefit from being compacted.

The race condition occurs because newly opened databases are added to the list before their initialization is complete. Scanning for GroomedCompaction candidates takes place once every minute or whenever one completes. If the initializing database is accessed during initialization, it may reference uninitialized memory and crash with a segmentation violation.

The open database list was originally added to support the FlexCache feature in Riak 2.0. There is a much smaller chance this race condition will occur in Riak 2.0, since it requires more than one database to open or close at the same time.

This issue is corrected by ensuring that the list of open databases is updated after the initialization of the database is complete. The correction is shipped within Riak KV 2.0.7, Riak KV 2.1.4, and Riak TS 1.3.0


# Affected Users

* Users running Riak KV 2.1.3 with the LevelDB backend, with or without AAE enabled.

* Users running Riak KV 2.1.3 with other backends (multi, memory, bitcask) and AAE enabled could also hit this issue.

* Users running Riak KV 2.0.0 and higher with AAE enabled are also affected, but the likelihood of hitting the race condition is much lower.

* Users running Riak TS 1.0.0 and higher are also affected, but the likelihood of hitting the race condition is much lower.


# Impact

An affected node will reference uninitialized memory and will likely exit with a segmentation violation.  It may be logged by the operating system as a crash by the beam.smp process.


# Mitigation Strategy

Upgrade to a release of Riak KV at 2.0.7 / 2.1.4 or later to resolve the issue, or patch the eleveldb.so shared library.

Riak KV 2.1.4 is the same as Riak KV 2.1.3 with a corrected version of the eleveldb.so library. Riak KV 2.1.4 will be released in the next few days.

Riak KV 2.0.7 is a regular patch release that is in progress and will be released in the next few weeks.

If you are unable to upgrade/patch, the occurrence can be reduced by temporarily disabling AAE until the node is upgraded or patched.


## Upgrade Riak

Download and update Riak KV using the downloaded package or the package cloud repo.

Packages can be downloaded by going to  [{{< baseurl >}}riak/kv/latest/downloads/]({{< baseurl >}}riak/kv/latest/downloads/) and selecting **2.1.4**, or from PackageCloud at [https://packagecloud.io/basho/riak](https://packagecloud.io/basho/riak).


## Patch eleveldb.so

If a package upgrade is not possible in your environment, the LevelDB shared library can be patched.

Do *not* apply the eleveldb.so patch to Riak TS, it will prevent it functioning correctly.

* [Debian 6](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_debian6.tar.gz)
* [Debian 7](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_debian7.tar.gz)
* [Fedora 19](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_fedora19.tar.gz)
* [FreeBSD 9](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_freebsd9.2.tar.gz)
* [OS X 10](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_osx10.8.tar.gz)
* [RHEL 5](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_rhel5.tar.gz)
* [RHEL 6](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_rhel6.tar.gz)
* [RHEL 7](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_rhel7.tar.gz)
* [SLES 11](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_sles11.tar.gz)
* [SmartOS 1.8](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_smartos1.8.tar.gz)
* [SmartOS 13.1](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_smartos13.1.tar.gz)
* [Solaris 10](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_solaris10.tar.gz)
* [Ubuntu Lucid](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_ubuntuLucid.tar.gz)
* [Ubuntu Precise](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_ubuntuPrecise.tar.gz)
* [Ubuntu Trusty](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.17/eleveldb_2.0.17_ubuntuTrusty.tar.gz)


### Installation and Removal Instructions

This patch contains natively compiled code. The `eleveldb.so` file must be installed to the eleveldb priv directory and cannot be added to basho-patches.

By default, this directory is in the following locations per OS:

* RHEL/CentOS - `/usr/lib64/riak/lib/eleveldb-2.1.10-0-g0537ca9/priv/`,
* Debian/Ubuntu - `/usr/lib/riak/lib/eleveldb-2.1.10-0-g0537ca9/priv/`,
* Solaris - `/opt/riak/lib/eleveldb-2.1.10-0-g0537ca9/priv/`,
* SmartOS - `/opt/local/lib/riak/lib/eleveldb-2.1.10-0-g0537ca9/priv/`,
* FreeBSD - `/usr/local/lib/riak/lib/eleveldb-2.1.10-0-g0537ca9/priv/`,
* On other platforms it may be `/usr/lib/riak/lib/eleveldb-2.1.10-0-g0537ca9/priv/`.


#### To install this patch, on each node in the cluster you must:

1.   Stop the node: `riak stop`.
2.   Change to the eleveldb priv directory (similar to `/usr/lib64/riak/lib/eleveldb-2.1.10-0-g0537ca9/priv/`)
3.   Rename the original leveldb library ( `eleveldb.so`) to `eleveldb.so.orig`.
4.   Copy the provided `eleveldb.so` to the directory and verify correct permissions.
5.   If possible, verify that the md5 sum of the `eleveldb.so` located in the eleveldb priv directory is correct.
6.   Start the node: `riak start`.


#### To back out of this patch, on each node in the cluster you must:

1.   Stop the node: `riak stop`.
2.   Remove the patched `eleveldb.so` file from the eleveldb priv directory.
3.   Rename `eleveldb.so.orig` to `eleveldb.so`.
4.   Start the node: `riak start`.


### Verifying the Patch Installation

When the patch is installed, the LevelDB LOG files will report that version 2.0.15 is installed. The LOG files for each running vnode will have a log line similar to the following:

```
<pre>2016/03/17-18:42:50.544293 7ffaaf3b1700             Version: 2.0.15
</pre>
```

## Disable AAE

Disabling AAE helps reduce the likelihood of hitting the race on startup during AAE rebuilds, until the system can be patched/upgraded.

You can disable it in a running cluster by running `riak attach`.

From within `riak attach` run the following snippet:

```
riak_core_util:rpc_every_member_ann(riak_kv_entropy_manager, disable, [], 5000).
riak_core_util:rpc_every_member_ann(riak_core_entropy_manager, cancel_exchanges,[],5000).
```

> **Note**: the trailing periods are significant
Press Ctrl-G, q to exit riak attach.

Make sure AAE remains off after a reboot by setting the following in your **riak.conf** file:


```riak.conf
anti_entropy = passive
```

 If you are using the legacy configuration file format, set the following in the `riak_kv` section of the **app.config** configuration file:

```app.config
{anti_entropy, {off, []}},
```
