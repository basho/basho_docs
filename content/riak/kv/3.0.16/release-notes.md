---
title: "Riak KV 3.0.16 Release Notes"
description: ""
project: "riak_kv"
project_version: 3.0.16
menu:
  riak_kv-3.0.16:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/3.0.16/community/release-notes
  - /riak/kv/3.0.16/intro-v20
  - /riak/3.0.16/intro-v20
  - /riak/kv/3.0.16/introduction
---

Released Feb 15, 2023.

## Overview

This release includes the following updates:

A significant new riak_core cluster claim algorithm has been added - `choose_claim_v4`. The default cluster claim algorithm (`choose_claim_v2`) is unchanged, but the `choose_claim_fun` configuration option in riak.conf can be used to enable the new algorithm. The new algorithm offers significant improvements in the claim process, especially when location awareness is enabled, with the algorithm now more likely to find valid solutions and with fewer required transfers. The trade-off when compared to `choose_claim_v2` is that in some situations computational overhead of claim may increase by multiple orders of magnitude, leading to long cluster plan times.

The leveled backend has been updated to reduce the memory required for stores with a large number of keys, and also reduce volatility in the memory demanded, by optimising the memory footprint of the SST process working heap.

A collection of fixes have been added. A fix to timeouts in leveled hot backups. A fix to reap and erase queries to prevent a backlog from overloading `riak_kv_eraser` or `riak_kv_reaper` mailboxes (creating rapidly increasing memory footprint). A fix to an issue with nextgen replication when node_confirms is enabled. The R/W value used by nextgen replication is now configurable, and this may be useful for preventing a backlog of replication events from overloading a cluster.

As a result of the memory management improvements made in 3.0.16, the recommendation to consider altering the eheap single-block carrier threshold made in Riak 3.0.12 has been deprecated. With the leveled backend, memory management should now be efficient on default settings, although with a small overhead on the latency of HEAD operations in that backend.

## Previous Release Notes

Please see the KV 3.0.15 release notes [here]({{<baseurl>}}riak/kv/3.0.15/release-notes/).

