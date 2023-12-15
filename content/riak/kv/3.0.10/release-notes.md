---
title: "Riak KV 3.0.10 Release Notes"
description: ""
project: "riak_kv"
project_version: "3.0.10"
lastmod: 2022-05-30T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-3.0.10:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/3.0.10/community/release-notes
  - /riak/kv/3.0.10/intro-v20
  - /riak/3.0.10/intro-v20
  - /riak/kv/3.0.10/introduction
---

Released May 29, 2022.

## Overview

This release is focused on improving memory management, especially with the leveled backend, and improving the efficiency and ease of configuration of tictacaae full-sync.

* Improved memory management of leveled SST files that contain rarely accessed data

* Fix a bug whereby leveled_sst files could spend an extended time in the delete_pending state, causing significant short-term increases in memory usage when there are work backlogs in the penciller.

* Change the queue for reapers and erasers so that they overflow to disk, rather than simply consuming more and more memory.

* Change the replrtq (nextgenrepl) queue to use the same overflow queue mechanism as used by the reaper and erasers.

* Change the default full-sync mechanism for tictacaae (nextgenrepl) full-sync to auto_check, which attempts to automatically learn and use information about modified date-ranges in full-sync checks. The related changes also make full-sync by default bi-directional, reducing the amount of wasted effort in full-sync queries.

* Add a peer discovery feature for replrtq (nextgenrepl) so that new nodes added to the cluster can be automatically recognised without configuration changes. By default this is disabled, and should only be enabled once both clusters have been upgraded to at least 3.0.10.

* Allow for underlying beam memory management and scheduler configuration to be exposed via riak.conf to allow for further performance tests on these settings. Note initial tests indicate the potential for significant improvements when using the leveled backend.

* Fix a potential issue whereby corrupted objects would prevent AAE (either legacy or nextgenrepl) tree rebuilds from completing.

* Improved handling of key amnesia, to prevent rebounding of objects, and also introduce a reader process (like reaper and eraser) to which read repairs can be queued with overflow to disk.

* The release does not support OTP 20, only OTP 22 is supported. Updating some long out-of-date components have led to a requirement for the OTP version to be lifted.

* To maintain backwards compatibility with older linux versions, the latest version of basho's leveldb is not yet supported. This is likely to change in the next release, where support for older linux versions will be dropped.
## Previous Release Notes

Please see the KV 3.0.9 release notes [here]({{<baseurl>}}riak/kv/3.0.9/release-notes/).

