---
title: "Riak KV 3.0.9 Release Notes"
description: ""
project: "riak_kv"
project_version: 3.0.9
menu:
  riak_kv-3.0.9:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/3.0.9/community/release-notes
  - /riak/kv/3.0.9/intro-v20
  - /riak/3.0.9/intro-v20
  - /riak/kv/3.0.9/introduction
---

Released Nov 13, 2021.

## Overview

This release contains stability, monitoring and performance improvements.

* Fix to the riak_core coverage planner to significantly reduce the CPU time required to produce a coverage plan, especially with larger ring sizes. This improves both the mean and tail-latency of secondary index queries. As a result of this change, it is recommended that larger ring sizes should be used by default, even when running relatively small clusters - for example in standard volume tests a ring size of 512 is outperforming lower ring sizes even on small (8-node) clusters.

* Further monitoring stats have been added to track the performance of coverage queries, in particular secondary index queries. For each worker queue (e.g. vnode_worker_pool, af1_pool etc) the queue_time and work_time is now monitored with results available via riak stats. The result counts, and overall query time for secondary index queries are now also monitored via riak stats. See the PR for a full list of stats added in this release.

* Change to some default settings to be better adapted to running with higher concentrations of vnodes per nodes. The per-vnode cache sizes in leveled are reduced, and the default size of the vnode_worker_pool has been reduced from 10 to 5 and is now configurable via riak.conf. Exceptionally heavy users of secondary index queries (i.e. > 1% of transactions), should consider monitoring the new queue_time and work_time statistics before accepting this new default.

* Fix to an issue in the leveled backend when a key and (internal) sequence number would hash to 0. It is recommended that users of leveled uplift to this version as soon as possible to resolve this issue. The risk is mitigated in Riak as it can generally be expected that most keys will have different sequence numbers in different vnodes, and will always have different sequence numbers when re-written - so normal anti-entropy process will recover from any localised data loss.

* More time is now given to the legacy AAE kv_index_hashtree process to shut down, to handle delays as multiple vnodes are shutdown concurrently and contend for disk and CPU resources.

## Previous Release Notes

Please see the KV 3.0.8 release notes [here]({{<baseurl>}}riak/kv/3.0.8/release-notes/).

