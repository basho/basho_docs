---
title: "Active Anti-Entropy: Slight chance that AAE could stall itself or crash a Riak node."
description: ""
project: community
project_version: "community"
lastmod: 2016-11-18T00:00:00-00:00
sitemap:
  priority: 0.5
menu:
  community:
    name: "AAE: Slight Chance for Stall or Crash"
    identifier: "aaestall"
    weight: 050
    parent: "productadvisories"
toc: true
---

Info |Value
:----|:----
Date issued | November 17, 2016
Product | Riak KV
Affected versions | Riak KV 2.0.0+, Riak KV 2.1.0+

## Overview

There exists a highly unlikely condition where Riak KV’s active anti-entropy feature can either stall or cause a segmentation fault crash.

{{% note title="Riak KV Enterprise Edition" %}}
If you are using Riak KV Enterprise Edition, please see the Product Advisory in the Basho Support Portal for your patches and instructions.
{{% /note %}}

## Description

The active anti-entropy feature (AAE) has a procedure known as a hash tree rebuild.  This procedure uses a LevelDB feature called an iterator.  The rebuild procedure has the potential to simultaneously instruct the iterator to retrieve the next data item and to close the iterator.  The probability of both happening is very slight, but Basho was able to create the simultaneous instructions under extremely heavy loads across many hours.

AAE stores its metadata within a LevelDB database.  This metadata is independent of the user’s selected storage backend for Riak (LevelDB, Bitcask, memory, or multi).  Therefore all users of AAE are impacted regardless of their chosen storage backend.

## Affected Users

You could be impacted if you are running one of the listed versions of Riak KV and your riak.conf has the default setting: `entropy = active`.

* Riak KV 2.0.0+
* Riak KV 2.1.0+

## Impact

Active entropy hash tree rebuilds could stall indefinitely on a Riak node, or
A Riak node could experience a segmentation fault that requires restarting Riak.

## Mitigation Strategy

There are two approaches you can take to mitigate this issue:

* Update to the newly released Riak KV 2.2, or
* Patch eLevelDB to 2.0.32.

## Patching eLevelDB to 2.0.32

If an upgrade to Riak KV 2.2 is not possible in your environment, the LevelDB library can be patched. This patch contains natively-compiled code and is operating system specific.

{{% note title="WARNING" %}}
Do not apply the eleveldb.so patch to Riak TS, it will prevent it functioning correctly.
{{% /note %}}

#### OS specific package links:

* [Debian 6](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_debian6.tar.gz)
* [Debian 7](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_debian7.tar.gz)
* [Fedora 19](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_fedora19.tar.gz)
* [FreeBSD 9](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_freebsd10.tar.gz)
* [OS X 10](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_osx10.8.tar.gz)
* [RHEL 5](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_rhel5.tar.gz)
* [RHEL 6](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_rhel6.tar.gz)
* [RHEL 7](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_rhel7.tar.gz)
* [SLES 11](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_sles11.tar.gz)
* [SmartOS 1.8](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_smartos1.8.tar.gz)
* [SmartOS 13.1](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_smartos13.1.tar.gz)
* [Solaris 10](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_solaris10.tar.gz)
* [Ubuntu Lucid](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_ubuntuLucid.tar.gz)
* [Ubuntu Precise](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_ubuntuPrecise.tar.gz)
* [Ubuntu Trusty](https://s3.amazonaws.com/downloads.basho.com/patches/eleveldb/2.0.32/eleveldb_2.0.32_ubuntuTrusty.tar.gz)

### Install the Patch

To install this patch, on each node in the cluster you must:

1. Determine your eleveldb directory<a name="determining"></a>.
    1. Find the Riak `lib` directory
        * RHEL/Centos/Fedora/SLES: `/usr/lib64/riak/lib/`
        * Ubuntu/Debian: `/usr/lib/riak/lib/`
        * FreeBSD: `/usr/local/lib/riak/lib/`
        * SmartOS: `/opt/local/lib/riak/lib/`
        * Solaris: `/opt/riak/lib/`
    1. Consult the directory listing for a directory beginning with `eleveldb`, for example: `eleveldb-2.0.17-0-g973fc92` on Riak KV 2.1.4 or `eleveldb-2.0.22-0-g185e296` on Riak KV 2.0.7
    1. Make a note of this directory name for the following steps.

1. Stop the node by running `riak stop`.
1. Change to the priv subdirectory of [your eleveldb directory](#determining).
1. Rename the original eleveldb.so file to eleveldb.so.orig.
1. Copy the provided eleveldb.so to the directory and verify correct permissions.
1. If possible, verify that the md5sum of the eleveldb.so located in the eleveldb priv directory is correct.
1. Change into the ebin subdirectory of [your eleveldb directory](#determining).
1. Rename the original eleveldb.beam file to eleveldb.beam.orig.
1. Copy the provided eleveldb.beam to the directory and verify correct permissions.
1. If possible, verify that the md5sum of the eleveldb.beam located in the eleveldb ebin directory is correct.
1. Start the node by running `riak start`.

### To back out of this patch, on each node in the cluster you must:

1. Determine your eleveldb directory<a name="determining2"></a>.
    1. Change to the Riak lib directory
        * RHEL/Centos/Fedora/SLES: `/usr/lib64/riak/lib/`
        * Ubuntu/Debian: `/usr/lib/riak/lib/`
        * FreeBSD: `/usr/local/lib/riak/lib/`
        * SmartOS: `/opt/local/lib/riak/lib/`
        * Solaris: `/opt/riak/lib/`
    1. Consult the directory listing for a directory beginning with `eleveldb`, for example: `eleveldb-2.0.17-0-g973fc92` on Riak KV 2.1.4 or `eleveldb-2.0.22-0-g185e296` on Riak KV 2.0.7
    1. Make a note of this directory name for the following steps.

1. Stop the node by running `riak stop`.
1. Change to the priv subdirectory of [your eleveldb directory](#determining).
1. Verify that you have an eleveldb.so.orig file present in this directory.
1. Remove the patched eleveldb.so file.
1. Rename `eleveldb.so.orig` to `eleveldb.so`.
1. Change into the ebin subdirectory of [your eleveldb directory](#determining).
1. Verify that you have an eleveldb.beam.orig file present in this directory.
1. Remove the patched `eleveldb.beam` file.
1. Rename `eleveldb.beam.orig` to `eleveldb.beam`.
1. Start the node by running `riak start`.

## Verifying the Patch Installation

When the patch is installed, the LevelDB LOG files will report that version 2.0.31 is installed.

{{% note %}}
**Note**:  this is eleveldb version 2.0.32 but the leveldb LOG file is expected to report 2.0.31.
{{% /note %}}

The LOG files for each running vnode will have a log line similar to the following:

```plaintext
2016/03/17-18:42:50.544293 7ffaaf3b1700             Version: 2.0.31
```
