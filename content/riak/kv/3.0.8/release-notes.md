---
title: "Riak KV 3.0.8 Release Notes"
description: ""
project: "riak_kv"
project_version: 3.0.8
menu:
  riak_kv-3.0.8:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/3.0.8/community/release-notes
  - /riak/kv/3.0.8/intro-v20
  - /riak/3.0.8/intro-v20
  - /riak/kv/3.0.8/introduction
---

Released Oct 12, 2021.

## Overview

This release contains a number of stability improvements:

* Fix to critical issue in leveled when using (non-default, but recommended, option): leveled_reload_recalc = enabled. If using this option, it is recommended to rebuild the ledger on each vnode at some stage after updating.

* Fix to an issue with cluster leave operations that could leave clusters unnecessarily unbalanced after Stage 1 of leave, and also cause unexpected safety violations in Stage 1 (with either a simple transfer or a rebalance). An option to force a rebalance in Stage 1 is also now supported.

* The ability to set an environment variable to remove the risk of atom table exhaustion due to repeated calls to riak status (and other) command-line functions.

* The default setting of the object_hash_version environment variable to reduce the opportunity for the riak_core_capability system to falsely request downgrading to legacy, especially when concurrently restarting many nodes.

* An update to the versions of recon and redbug used in Riak.

* The fixing of an issue with connection close handling in the Riak erlang client.

This release also contains two new features:

* A new aae_fold operation (`repair_keys_range`) has been added which will support the prompting of read-repair for a range of keys. For example, you could run a read-repair on all keys between two values in a given bucket starting with  e.g. all the keys in a bucket after a given last modified date. This is intended to allow an operator to accelerate full recovery of data following a known node outage. Read more [here]({{<baseurl>}}riak/kv/3.0.8/using/cluster-operations/tictac-aae-fold/repair-keys-range/).

* The addition of the `sync_on_write` property for write operations. Some Riak users require flushing of writes to disk to protect against data loss in disaster scenarios, such as mass loss of power across a DC. This can have a significant impact on throughput even with hardware acceleration (e.g. flash-backed write caches). The decision to flush was previously all or nothing. It can now be set as a bucket property (and even determined on individual writes), and can be set to flush on all vnodes or just one (the coordinator), or to simply respect the backend configuration. If one is used the single flush will occur only on client-initiated writes - writes due to handoffs or replication will not be flushed.

## Previous Release Notes

Please see the KV 3.0.7 release notes [here]({{<baseurl>}}riak/kv/3.0.7/release-notes/).

