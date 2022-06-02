---
title: "Riak KV 2.9.8 Release Notes"
description: ""
project: "riak_kv"
project_version: 2.9.8
menu:
  riak_kv-2.9.8:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/2.9.8/community/release-notes
  - /riak/kv/2.9.8/intro-v20
  - /riak/2.9.8/intro-v20
  - /riak/kv/2.9.8/introduction
---

Released Dec 06, 2020.


## Overview

This release improves the performance and stability of the leveled backend and of AAE folds. These performance improvements are based on feedback from deployments with > 1bn keys per cluster.

The particular improvements are:

- In leveled, caching of individual file scores so not all files are required to be scored each journal compaction run.

- In leveled, a change to the default journal compaction scoring percentages to make longer runs more likely (i.e. achieve more compaction per scoring run).

- In leveled, a change to the caching of SST file block-index in the ledger, that makes repeated folds with a last modified date range an order of magnitude faster and more computationally efficient.

- In leveled, a fix to prevent very long list-buckets queries when buckets have just been deleted (by erasing all keys).

- In kv_index_tictcatree, improved logging and exchange controls to make exchanges easier to monitor and less likely to prompt unnecessary work.

- In kv_index_tictcatree, a change to speed-up the necessary rebuilds of aae tree-caches following a node crash, by only testing journal presence in scheduled rebuilds.

- In riak_kv_ttaaefs_manager, some essential fixes to prevent excessive CPU load when comparing large volumes of keys and clocks, due to a failure to decode clocks correctly before passing to the exchange.

Further significant improvements have been made to Tictac AAE full-sync, to greatly improve the efficiency of operation when there exists relatively large deltas between relatively large clusters (in terms of key counts). Those changes, which introduce the use of 'day_check', 'hour_check' and 'range_check' options to nval-based full-sync will be available in a future 3.0.2 release of Riak. For those wishing to use Tictac AAE full-sync at a non-trivial scale, it is recommended moving straight to 3.0.2 when it is available.

## Previous Release Notes

Please see the KV 2.9.7 release notes [here]({{<baseurl>}}riak/kv/2.9.7/release-notes/).





