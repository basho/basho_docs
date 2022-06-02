---
title: "Riak KV 2.0.9 Release Notes"
description: ""
project: "riak_kv"
project_version: "2.0.9"
menu:
  riak_kv-2.0.9:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: true
aliases:
  - /riak/2.0.9/community/release-notes
  - /riak/kv/2.0.9/intro-v20
  - /riak/2.0.9/intro-v20
  - /riak/kv/2.0.9/introduction
---

Released March 21, 2017.

In KV 2.0.8 we [fixed](https://github.com/basho/node_package/pull/183) an  [issue](https://github.com/basho/riak/issues/509) with nodetool, allowing it to work with SSL options in vm.args. However, `exec` was, in specific cases, calling the new NODETOOL alias, and four riak-admin commands (`handoff`, `get`, `set`, and `describe`) were using `exec` to allow them to run properly on all of our supported operating systems, meaning running those commands caused the following error message:
```
[root@node1 ~]# riak-admin handoff
/usr/sbin/riak-admin: line 1072: exec: NODETOOL: not found
```
This bugfix release resolves the issues with `exec` and `NODETOOL`, allowing `handoff`, `get`, `set`, and `describe` to function properly. We have located the broken QA test that misidentified these commands as working and have corrected it for future releases.


## Bugs Fixed

* [[riak PR 903](https://github.com/basho/riak/pull/903)] Remove `exec` from calls to NODETOOL alias entirely.



## Riak KV 2.0.8 Release Notes

Released February 15, 2017.

This is an LTS (long term support) bugfix release that includes improvements to Solr integration and Riak search.


### New Features

* Improved Solr integration with Riak search. In KV 2.0.7, we introduced a new batching system for Riak search so indexing calls are no longer made synchronously when data is written to Riak. This allows Solr to process the data in larger chunks and Riak to move forward accepting new work at the vnode level without waiting for the call to Solr to happen. In 2.0.9, we've significantly improved that batching system (see below).
    * [[yokozuna PR 614](https://github.com/basho/yokozuna/pull/614)]
    * [[yokozuna PR 634](https://github.com/basho/yokozuna/pull/634)]
    * [[yokozuna PR 648](https://github.com/basho/yokozuna/pull/648)]

* The Riak search batching system has significant improvements to its performance, robustness, and operational logging, including:
    * deleteByQuery has been changed to delete documents in Solr by document ID rather than by query. Prior to this change, when a document that might contain siblings was added to Solr, we would add a delete by query operation to the batch we send to Solr (in a single HTTP request). This negatively impacted performance, especially when objects being updated had a lot of siblings.
    *`yz_drain_fsm`, `yz_drain_mgr`, and `yz_exchange_fsm` have been changed to split Solr drains from the hashtree update. This split resolves the issue of drain timeouts under normal operating conditions.
    * `fold_keys` now uses the same "90 async + 1 sync" call pattern that `yz_kv:index` uses. During performance testing, it was discovered that the yz_index_hashtree:fold_keys function could swamp the mailbox of the yz_index_hashtree so that other processes could not make progress. That logic (which lived in yz_kv) has been moved to yz_index_hashtree and shared b the new index and delete calls that do not take an explicit "call mode" parameter.
    * "Will Repair" logs in the yz_exchange_fsm have been modified to track the direction of repair, specifically, whether the repair resulted in a delete of Solr data or an add/update to Solr.
    * Exometer statistics are now removed when an index is removed. Before, if an index was later re-added, the fuse creation would fail eventually causing the node to crash.
    * `yz_solrq_drain_fsm` are now monitored from the queues being drained. Before, it was possible for a queue to get stuck in `wait_for_drain_complete` state if the drain fsm crashed before the drain complete messages were sent.
    * Logging has been added to clear and exchange trees for audit of administrative operations.
    * All above work captured in [yokozuna PR 704](https://github.com/basho/yokozuna/pull/704).

* Additional [Cuttlefish parameters]({{<baseurl>}}riak/kv/2.0.9/configuring/reference/#search) have been added to support the Riak search batching updates. These configs will allow you to set batching parameters based on your needs and have, in certain cases, led to significantly higher write throughput to Solr.
    * [[yokozuna PR 704](https://github.com/basho/yokozuna/pull/704)]


### Bugs Fixed

* LevelDB has been upgraded to version 2.0.33, which resolves the [AAE stall product advisory]({{<baseurl>}}community/productadvisories/aaestall/).
* [[riak_kv PR 1527](https://github.com/basho/riak_kv/pull/1527)] A race condition was occurring where a `gen_fsm` timeout event was not reliably sent, even when the timeout was set to zero, and another message or event could preempt or unset the timeout. To fix this, a timeout event is manually sent using `gen_fsm:send_event`.
* [[riak PR 886](https://github.com/basho/riak/pull/886), [riak_ee PR 412](https://github.com/basho/riak_ee/pull/412), and [node_package PR 210](https://github.com/basho/node_package/pull/210)] Atom usage in `riak` and `riak-admin` commands has been restricted to 1000. Previously, the OS PID was being used as a pseudo-random number generator, but the range was too large since each nodename used would generate an entry in the atom table. `riak-admin top` uses $$ to randomize the name used to connect to the local Riak node, and the large range of possible OS PIDs can result in atom table exhaustion on long running nodes/clusters. The nodename used by `riak top` has been changed to match `riak-admin top` convention, using `$RANDOM` with the range restricted to 1-1000.
* [[riak_core Issue 855](https://github.com/basho/riak_core/issues/855)/[riak_core PR 886](https://github.com/basho/riak_core/pull/886)] If updates to the same key in the ring metadata occurred on different nodes during the same second, they were not reconciled. This could lead to nodes flip-flopping the value and many gossip messages causing extremely high message queues and heap usage by the gossip processes. Nodenames have been added to the `merge_meta` comparison to avoid this issue.
* [[riak_repl PR 766](https://github.com/basho/riak_repl/pull/766)] Status output is expected to be a list. When the repl leader is undefined the status will now match the regular format: a list.
* [[riak_core PR 894](https://github.com/basho/riak_core/pull/894)] The `dropped_vnode_requests_total` stat was not incremented correctly in Riak stats due to being incorrectly projected in `riak_core_stat`. This has been fixed by setting stat name correctly as `dropped_vnode_requests`.
* [[riak_core PR 887](https://github.com/basho/riak_core/pull/887)] If `riak_core_vnode_master:sync_spawn_command` was called, it would send a `gen_event all-state` event to the proxy for forwarding. However, in `handle_overload`, the code only had case clauses for '$gen_event' and not '$gen_all_state_event', meaning the event would be passed to `handle_overload_info` instead of `handle_overload_reques` which would skip the vnode callback code that sends a response. Then certain operations would hang during overload since the caller to `sync_spawn_command` would never be sent a response.
A clause for `$gen_all_state_event` has been added to fix potential hang due to vnode proxy overload bug.
* [[riak_kv PR 1575](https://github.com/basho/riak_kv/pull/1575)] Dots are now always dropped from CRDTS. Prior to this change the first write of a new CRDT contained  a dot in its metadata, since dots were only dropped on merge. This could potentially lead to sibling CRDTs on disk, since our merge code assumes CRDTs will never have dots. Typically this did not cause problems, but doing a basic GET could cause the siblings to be exposed to the client directly, and was causing the test_hll test to intermittently fail.
* [[riak Issue 509](https://github.com/basho/riak/issues/509)/[node_package PR 183](https://github.com/basho/node_package/pull/183)] SSL options in vm.args would let nodetool stop working. This was fixed by having node_package use an alias for nodetool rather than an environment variable.
* [[riak_core PR 812](https://github.com/basho/riak_core/pull/812)] `riak_core_tcp_mon` became blocked when checking the socket options when SSL sockets were used for replication. The processes holding SSL sockets can block indefinitely when the TCP buffer is full and the getopt call goes through the SSL connection process. This is now fixed by peeking into to the SSL socket structure to find the options.
* [[riak_repl Issue 649 ](https://github.com/basho/riak_repl/issues/649)/[riak_repl PR 652](https://github.com/basho/riak_repl/pull/652) and [riak_repl PR 725](https://github.com/basho/riak_repl/pull/725)] A memory leak problem was discovered in Riak's replication. The leak was discovered in a sink cluster. The real-time sink and source connection has been fixed to ensure cleaner shutdown of processes within real-time replication. Also, real-time queues have been refactored to ignore exceptions from pull delivery functions and the error for unregistered consumers now includes debugging information.
* [[leveldb PR 197](https://github.com/basho/leveldb/pull/197)] MoveItems are eLevelDB's iterator objects and are reusable. MoveItems communicate the reuse desire to the hot threads logic via the resubmit() property. When resubmit() returns true, hot threads executes the same task again immediately. Prior to merging eLevelDB's hot threads with LevelDB's hot threads, only eLevelDB's code supported the resubmit() property. The support required an extra five lines of code within the thread loop routine. Unfortunately, leveldb had two thread loop routines. Only one of the two received the extra five lines during the merge. The additional five lines supporting the resubmit() property have been added to LevelDB's second thread loop.
* [[yokozuna commit](https://github.com/basho/yokozuna/commit/0b4486e9331048e371ea01aeb554fb42c5228d2f)] When making requests to Solr, if requests timed-out between the caller and ibrowse, ibrowse might still send a response slightly after the timeout. The post-timeout response would cause `yz_solrq_proc` to crash due to improperly handling the late reply message. This fix prevents late replies from causing crashes.
* [[yokozuna commit](https://github.com/basho/yokozuna/commit/f6e16f9cbf14b193ab447cfce0bb8d6971fb93a4)] When Riak search and active anti-entropy are both enabled, all keys written to Riak KV must be hashed and written to yokozuna hash trees, even if they are not written to Solr. This is done asynchronously, by passing the data along to the yokozuna batching infrastructure. Initially, the queues responsible for handling these non-indexed items were not included in the list of required queues, so they would be shut down by the synchronization mechanism that is designed to keep the queue processes running in sync with the partitions and indexes that are currently hosted by the node in question. Non-indexed queues are now included in the list of required queues so they stay active.



### Additions

* The Debian packages have been updated to support Debian 8. And Ubuntu 16.0.4 (Xenial) is  now supported. [[node_package PR 204](https://github.com/basho/node_package/pull/204)]
* While cleaning up yz_stats.erl, we discovered that the yz_stats_worker was no longer being used. Made yz_stats use a background process to update exometer stats similar to riak_kv_stat. [[yokozuna PR 646](https://github.com/basho/yokozuna/pull/646)]



### Changes


* The following changes have been backported to KV 2.0.9: When calling `clear_trees`, trees are not updated before being destroyed; error returns are handled in vnode fold request; and build throttle may now be changed while a build is ongoing. Detailed information for each change is below. [[riak_kv PR 1574](https://github.com/basho/riak_kv/pull/1574)]
    * When the hashtree is about to be destroyed, via `clear_trees`, it can now simply close without updating.
    * When `fold_fun` called `riak_core_master:sync_command`, the result of the call was ignored. Now, `sync_command` returns an `{error, overload}` if the vnode is overloaded and the non-success result causes the tree build to fail.
    * The build throttle can be changed while a build is in-progress. Before, the size and wait time of a build could only be changed before a build started. Once the build began, the settings could not be altered, even if the vnode was large and you wanted to decrease the wait or increase the size.
* Empty exchange history is now handled. When `compute_exchange_info` is called on an index with no previous exchanges, `undefined` is now returned for the Recent and LastAll timestamps. [[riak_kv PR 1576](https://github.com/basho/riak_kv/pull/1576)]
* Debug logging for ring metadata merges and ring membership has been added. There have been a few issues where rapidly updating the ring results in suboptimal behavior, and it has been difficult to debug due to the lack of logging in the riak_core_ring module. This logging can be enabled as needed. [[riak_core PR 893](https://github.com/basho/riak_core/pull/893)]
* node_package has been updated to version 3.1.1. You may have to change permissions & etc. Please read the [node_package release notes](https://github.com/basho/node_package/blob/develop/RELEASE-NOTES.md) for more information. 
* The Solr queue is now flushed on graceful shutdown. [[yokozuna Issue 581](https://github.com/basho/yokozuna/issues/581) and [yokozuna Issue 582](https://github.com/basho/yokozuna/issues/582)/[yokozuna PR 610](https://github.com/basho/yokozuna/pull/610)] 
* In KV 2.1, an [issue](https://github.com/basho/riak_kv/issues/679) was fixed by having a monotonic counter fsynced to disk when the vnode starts. Additional detail has been added to the `vnodeid` warning, and the warning has been downgraded to warn from error since the per-key-epoch code stops this from being an error. [[riak_kv PR 1344](https://github.com/basho/riak_kv/pull/1344)]
* Erlang sets have been changed to ordsets across data types for a small performance improvement. [[riak_dt PR 117](https://github.com/basho/riak_dt/pull/117)]
* The `riak_core_table_owner` gen_server process has been introduced to replace the use of application environment variables with ETS tables. This was needed because `riak_core_throttle` had used application environment variables for maintaining its state. However, due to race conditions that manifested during application shutdown, errors would occur that prevented applications from shutting down cleanly.  The replacement avoids these errors and shutdown cleanly. [[riak_core PR 861](https://github.com/basho/riak_core/pull/861)]
* Additional checks have been added to validate object values. It is possible to write objects to a CRDT-enabled bucket that are not, in fact, CRDTs. To prevent Yokozuna from attempting to merge non-CRDT objects, additional checks have implemented in `riak_kv_crdt:is_crdt_object` to validate that the object values are, in fact, CRDTs. [[yokozuna PR 630](https://github.com/basho/yokozuna/pull/630)]
* All successful requests are now logged at the debug level. [[riak_core PR 864](https://github.com/basho/riak_core/pull/864)]
* Calls to `yz_index_hashtree:compare/5` are now made on a separate process to allow the exchange FSM to handle other messages. This change prevents 'DOWN' messages from failing to get through due to compare calls. [[yokozuna commit](https://github.com/basho/yokozuna/commit/62ef65aee9fd035d4cce55219e0d0110509b02f7)]
* `riak_core_vnode_manager` is now asked which `riak_kv_vnode` vnodes are currently running, and uses the list to figure out which queues need to run. This prevents the soleqs on fallback vnodes from stopping if they are still needed. [[yokozuna commit](https://github.com/basho/yokozuna/commit/e228268148b02364da52801d787dc367999db9bf)]
* riak_kv has been changed such that updating an object also sends the old object being replaced. From that old object, we can extract any siblings and generate associated document ids to delete in Solr. [[riak_kv PR 1520](https://github.com/basho/riak_kv/pull/1520)]


### Upgraded components

* Bitcask has been upgraded to version 1.7.4
* Cuttlefish has been upgraded to version 2.0.2p2
* eLevelDB has been upgraded to version 2.0.23
* LevelDB has been upgraded to version 2.0.33
* node_package has been upgraded to version 4.0.0
* OTP has been upgraded to version R16B02_basho10
