---
title: "Riak KV 2.2.0 Release Notes"
description: ""
project: "riak_kv"
project_version: "2.2.0"
menu:
  riak_kv-2.2.0:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: true
aliases:
  - /riak/2.2.0/community/release-notes
  - /riak/kv/2.2.0/intro-v20
  - /riak/2.2.0/intro-v20
  - /riak/kv/2.2.0/introduction
---


Released November 17, 2016.


This is a backwards-incompatible release that includes several improvements and features alongside many bugfixes. While most backwards-incompatible features and improvements are opt-in, active anti-entropy (AAE) improvements and Solr upgrades are not. You may opt-out of AAE improvements (see the [Upgrading](#upgrading) note below), but you cannot opt-out of the Solr upgrades (see the [Downgrading](#downgrading) note below).


We have improved Solr integration and Riak search, as well as upgraded the version of Solr we use. We've fixed some long-standing issues with AAE. And we've also added enhanced configuration controls for performance-impacting commands. 


New features in KV 2.2.0 include global object expiration and LZ4 compression for LevelDB, and the introduction of a HyperLogLog distributed data type. You can read more about these [below](#release-features).



## Upgrading
### Riak KV Enterprise Edition Only


If you are using AAE fullsync and have a very tight downgrade window, consider disabling the AAE upgrade until you have fully accepted 2.2.0 and rolled it out to all participating clusters. You can read how to disable the upgraded AAE at [Step 5 here]({{<baseurl>}}riak/kv/2.2.0/setup/upgrading/version/#upgrading-process). 


AAE trees are versioned, so if you choose to enable the 2.2.0 AAE improvements, the AAE trees will need to be destroyed on downgrade and fully repopulated from the object data. During any period in which the AAE trees are invalid, AAE fullsyncs will not work.

If MDC clusters will be upgraded in stages, during the time that the cluster versions are mismatched with Riak KV versions 2.2.0 and Riak KV versions less than 2.2.0, replication will fail due to a known issue with Bucket Mismatch between the clusters documented [here]({{<baseurl>}}riak/kv/2.2.0/release-notes/#replication-bucket-mismatch).


## Downgrading
### Riak search users


The upgrade to Solr 4.10.4 causes new data written to the cluster to be written in a format that is incompatible with earlier versions of Solr (and, therefore, earlier versions of Riak KV). The [Upgrade]({{<baseurl>}}riak/kv/2.2.0/setup/upgrading/version/) and [Downgrade]({{<baseurl>}}riak/kv/2.2.0/setup/downgrade/) documentation describes the steps you will need to take to reindex your data in a rolling fashion. Be aware this can make downgrades take a very long time, but will minimize exposure of the downgrading nodes to applications that utilize the Riak search feature.



## Release Features

* Improved Solr integration mostly impacts Riak search, but includes an upgrade to a [newer version of Solr](#upgraded-components). In Riak 2.0.7, we introduced a new batching system for Riak search so indexing calls are no longer made synchronously when data is written to Riak. This allows Solr to process the data in larger chunks and Riak to move forward accepting new work at the vnode level without waiting for the call to Solr to happen. In 2.2, we've significantly improved that batching system (see below).
    * [[yokozuna PR 614](https://github.com/basho/yokozuna/pull/614)]
    * [[yokozuna PR 634](https://github.com/basho/yokozuna/pull/634)]
    * [[yokozuna PR 648](https://github.com/basho/yokozuna/pull/648)]
* The Riak search batching system has significant improvements to its performance, robustness, and operational logging, including: 
    * deleteByQuery has been changed to delete documents in Solr by document ID rather than by query. Prior to this change, when a document that might contain siblings was added to Solr, we would add a delete by query operation to the batch we send to Solr (in a single HTTP request). This negatively impacted performance, especially when objects being updated had a lot of siblings.
    * `yz_drain_fsm`, `yz_drain_mgr`, and `yz_exchange_fsm` have been changed to split Solr drains from the hashtree update. This split resolves the issue of drain timeouts under normal operating conditions.
    * `fold_keys` now uses the same "90 async + 1 sync" call pattern that `yz_kv:index` uses. During performance testing, it was discovered that the yz_index_hashtree:fold_keys function could swamp the mailbox of the yz_index_hashtree so that other processes could not make progress. That logic (which lived in yz_kv) has been moved to yz_index_hashtree and shared by the new index and delete calls that do not take an explicit "call mode" parameter.
    * "Will Repair" logs in the yz_exchange_fsm have been modified to track the direction of repair, specifically, whether the repair resulted in a delete of Solr data or an add/update to Solr.
    * Exometer statistics are now removed when an index is removed. Before, if an index was later re-added, the fuse creation would fail eventually causing the node to crash. 
    * `yz_solrq_drain_fsm` are now monitored from the queues being drained. Before, it was possible for a queue to get stuck in wait_for_drain_complete state if the drain fsm crashed before the drain complete messages were sent.
    * Logging has been added to clear and exchange trees for audit of administrative operations.
    * All above work captured in  [[yokozuna PR 700](https://github.com/basho/yokozuna/pull/700)].
* Additional [Cuttlefish parameters]({{<baseurl>}}riak/kv/2.2.0/configuring/reference/#search) have been added to support the Riak search batching updates. These configs will allow you to set batching parameters based on your needs and have, in certain cases, led to significantly higher write throughput to Solr.
    * [[yokozuna PR 700](https://github.com/basho/yokozuna/pull/700)]
* LevelDB global object expiration allows data to be automatically, efficiently deleted in LevelDB and brings LevelDB to feature parity with Bitcask.
    * [[eleveldb PR 211](https://github.com/basho/eleveldb/pull/211)]
    * [[eleveldb PR 210](https://github.com/basho/eleveldb/pull/210)]
* LevelDB now has LZ4 compression, which provides faster compression of data for enhanced cluster performance.
    * [[eleveldb PR 208](https://github.com/basho/eleveldb/pull/208)]
    * [[eleveldb PR 216](https://github.com/basho/eleveldb/pull/216)]
* Cluster job controls allow you to set controls over commands that might have a performance impact on the Riak cluster, for example: list keys, list buckets, secondary index(2i) queries, and MapReduce. Denied operations will be logged to file. You can read more about these [here]({{<baseurl>}}riak/kv/2.2.0/configuring/reference#cluster-job-controls).
    * [[riak PR 868](https://github.com/basho/riak/pull/868)]
    * [[riak_core PR 851](https://github.com/basho/riak_core/pull/851)]
    * [[riak_ee PR ](https://github.com/basho/riak_ee/pull/405)]
    * [[riak_kv PR 1459](https://github.com/basho/riak_kv/pull/1459)]
    * [[riak_search PR 184](https://github.com/basho/riak_search/pull/184)]
    * [[yokozuna PR 671](https://github.com/basho/yokozuna/pull/671)]
* The [HyperLogLog (HLL) distributed data type]({{<baseurl>}}riak/kv/2.2.0/learn/concepts/crdts/#hyperloglogs) provides high-performance, approximate count of unique objects in massive sets by estimating the unique elements in a large set or stream of data. HLL keeps items at a constant size using a hash-based algorithm, which keeps memory usage low. Normally, calculating the exact cardinality of a set requires an amount of memory proportional to the cardinality when counting these unique items. With HLLs, the trade off is less memory in exchange for approximated cardinality. More of HLL usage can be found [here]({{<baseurl>}}riak/kv/2.2.0/developing/data-types/hyperloglogs/).
    * [[riak_kv PR 1435](https://github.com/basho/riak_kv/pull/1435)]
* Active anti-entropy (AAE) improvements remedy an issue in prior versions of Riak KV where the hashing function used for AAE could trigger unneeded read-repairs. In 2.2, hashing is improved so that unnecessary read repairs are not triggered and AAE uses less resources. AAE also upgrades automatically. (You can configure AAE not to automatically upgrade, but we do not recommend this.)
    * [[yokozuna PR 662](https://github.com/basho/yokozuna/pull/662)]
    * [[yokozuna PR 679](https://github.com/basho/yokozuna/pull/679)]
    * [[yokozuna PR 680](https://github.com/basho/yokozuna/pull/680)]
    * [[riak_core PR 849](https://github.com/basho/riak_core/pull/849)]
    * [[riak_kv PR 1446](https://github.com/basho/riak_kv/pull/1446)]
    * [[riak_kv PR 1488](https://github.com/basho/riak_kv/pull/1488)]
    * [[riak_kv PR 1485](https://github.com/basho/riak_kv/pull/1485)]
    * [[riak_kv PR 1484](https://github.com/basho/riak_kv/pull/1484)]
    * [[riak_kv PR 1481](https://github.com/basho/riak_kv/pull/1481)]
    * [[riak_kv PR 1473](https://github.com/basho/riak_kv/pull/1473)]
    * [[riak_repl PR 745](https://github.com/basho/riak_repl/pull/745)]
    * [[riak_repl PR 750](https://github.com/basho/riak_repl/pull/750)]


## Additions


* The Debian packages have been updated to support Debian 8. And Ubuntu 16.0.4 (Xenial) is  now supported. [[node_package PR 204](https://github.com/basho/node_package/pull/204)]
* While cleaning up yz_stats.erl, we discovered that the yz_stats_worker was no longer being used. Made yz_stats use a background process to update exometer stats similar to riak_kv_stat. [[yokozuna PR 646](https://github.com/basho/yokozuna/pull/646)]
* A Lager sink object has been added to advanced.config to support Lager 3 sinks.
[[riak PR 876](https://github.com/basho/riak/pull/876)]



## Changes

* node_package has been updated to version 3.1.1. You may have to change permissions & etc. Please read the [node_package release notes](https://github.com/basho/node_package/blob/develop/RELEASE-NOTES.md) for more information. 
* The Solr queue is now flushed on graceful shutdown. [[yokozuna Issue 581](https://github.com/basho/yokozuna/issues/581) and [yokozuna Issue 582](https://github.com/basho/yokozuna/issues/582)/[yokozuna PR 610](https://github.com/basho/yokozuna/pull/610)] 
* The default for `ERL_MAX_PORTS (+Q) ` has been increased to 262144. This change should help mitigate a fairly rare issue where Erlang would run out of available ports. This issue was seen especially when using the multi-backend, as many more files could be opened depending on the multi-backend configuration.[[Issue 801](https://github.com/basho/riak/issues/801)/[cuttlefish PR 208](https://github.com/basho/cuttlefish/pull/208)].
* In KV 2.1, an [issue](https://github.com/basho/riak_kv/issues/679) was fixed by having a monotonic counter fsynced to disk when the vnode starts. Additional detail has been added to the `vnodeid` warning, and the warning has been downgraded to warn from error since the per-key-epoch code stops this from being an error. [[riak_kv PR 1344](https://github.com/basho/riak_kv/pull/1344)]
* Erlang sets have been changed to ordsets across data types for a small performance improvement. [[riak_dt PR 117](https://github.com/basho/riak_dt/pull/117)]
* RTQ has been refactored to ignore exceptions from pull delivery functions. The error that occurs in the event that the consumer is not registered to include information needed to debug RTQ has also been improved. [[riak_repl PR 725](https://github.com/basho/riak_repl/pull/725)]
* The `riak_core_table_owner` gen_server process has been introduced to replace the use of application environment variables with ETS tables. This was needed because `riak_core_throttle` had used application environment variables for maintaining its state. However, due to race conditions that manifested during application shutdown, errors would occur that prevented applications from shutting down cleanly.  The replacement avoids these errors and shutdown cleanly. [[riak_core PR 861](https://github.com/basho/riak_core/pull/861)]
* Additional checks have been added to validate object values. It is possible to write objects to a CRDT-enabled bucket that are not, in fact, CRDTs. To prevent Yokozuna from attempting to merge non-CRDT objects, additional checks have implemented in `riak_kv_crdt:is_crdt_object` to validate that the object values are, in fact, CRDTs. [[yokozuna PR 630](https://github.com/basho/yokozuna/pull/630)]
* All successful requests are now logged at the debug level. [[riak_core PR 864](https://github.com/basho/riak_core/pull/864)]
* Calls to `yz_index_hashtree:compare/5` are now made on a separate process to allow the exchange FSM to handle other messages. This change prevents 'DOWN' messages from failing to get through due to compare calls. [[yokozuna commit](https://github.com/basho/yokozuna/commit/62ef65aee9fd035d4cce55219e0d0110509b02f7)]
* `riak_core_vnode_manager` is now asked which `riak_kv_vnode` vnodes are currently running, and uses the list to figure out which queues need to run. This prevents the soleqs on fallback vnodes from stopping if they are still needed. [[yokozuna commit](https://github.com/basho/yokozuna/commit/e228268148b02364da52801d787dc367999db9bf)]
* riak_kv has been changed such that updating an object also sends the old object being replaced. From that old object, we can extract any siblings and generate associated document ids to delete in Solr. [[riak_kv PR 1520](https://github.com/basho/riak_kv/pull/1520)] 



## Bugs Fixed

* [[Issue 1178](https://github.com/basho/riak_kv/issues/1178)/[riak_kv PR 1420](https://github.com/basho/riak_kv/pull/1420)] riak_kv can no longer run with sidejob disabled. The removal of the non-sidejob code cuts down on risk and maintenance costs, and improves performance. Included in the code removal are `riak_kv_get_fsm` and `riak_kv_put_fsm` supervisors. The GET/PUT FSM start_link functions have been renamed to 'start', though the start_link function name is kept as an alias to avoid any potential problems during rolling upgrades. This resolves an issue where calls to `riak_kv_get_fsm_sup:start_get_fsm` leave defunct PIDs in the `riak_kv_get_fsm_sup`, which can cause extended shutdown times as the supervisor attempts to iterate through millions of dead PIDs.
* A thorough review of file ownership across the Riak KV package was done and several files, including riak init, were changed to tighten the ownership to root:root instead of riak:riak to prevent a potential code injection across all supported operating systems. You can read more about this issue [here]({{<baseurl>}}community/productadvisories/codeinjectioninitfiles/). [[node_package PR 196](https://github.com/basho/node_package/pull/196)]
* The AddDB() call now occurs after all object initialization is complete to eliminate a race condition that leads to segfault. You can read more about the issue [here]({{<baseurl>}}community/productadvisories/leveldbsegfault/).[[LevelDB PR 184](https://github.com/basho/leveldb/pull/184)]
* [[Issue 1064](https://github.com/basho/riak_kv/issues/1064)/[riak_kv PR 1331](https://github.com/basho/riak_kv/pull/1331) & [riak_kv PR 963](https://github.com/basho/riak_kv/pull/963)] When using the `max_memory` setting in the memory backend, a list of timers is kept in ETS. In certain circumstances, these timer references were not deleted when the item had expired or when a new value was put to the table.The timer references are now appropriately deleted.
* [[riak_kv PR 1282](https://github.com/basho/riak_kv/pull/1282)] Unregister per-vnode statistics when cleanly shutting down. However if the vnode crashes, the terminate callback will not be executed.
* [[mochiweb PR 20](https://github.com/basho/mochiweb/pull/20)] In certain circumstances, mochiweb_http could receive an unexpected message and reply with a 400 response. When using keep-alive HTTP connections and a load balancer, it was possible for this same connection to later receive and transmit back to a client a delayed message rather than closing the connection properly. Mochiweb is now prevented from sending an erroneous 400 message.
* [[Bitcask PR 229](https://github.com/basho/bitcask/pull/229) and [Bitcask PR 227](https://github.com/basho/bitcask/pull/227)] This stops the `bitcask_file` process leak after a failure to open file and logs the file open failure.
* [[riak_core PR 812](https://github.com/basho/riak_core/pull/812)] `riak_core_tcp_mon` became blocked when checking the socket options when SSL sockets were used for replication. The processes holding SSL sockets can block indefinitely when the TCP buffer is full and the getopt call goes through the SSL connection process. This is now fixed by peeking into to the SSL socket structure to find the options.
* [[Issue 1156](https://github.com/basho/riak_kv/issues/1156)/[riak _kv PR 1363](https://github.com/basho/riak_kv/pull/1363)] During read repair, if the reconciled object exceeded sibling limits or object size it could not be repaired. A new read repair element has been added so that read-repair ignores limits.
* [[Issue 559](https://github.com/basho/yokozuna/issues/559)/[yokozuna PR 549](https://github.com/basho/yokozuna/pull/549) & [yokozuna PR 561](https://github.com/basho/yokozuna/pull/561)] Riak search returned inconsistent results for an unknown reason, though deleting data is assumed to have something to do with it.  To address the root causes of inconsistent search results, search delete operations are checked for crdts/lww=true, allow_mult=false, and strong consistency by checking for tombstones.
* [[riak_repl Issue 649 ](https://github.com/basho/riak_repl/issues/649)/[riak_repl PR 652](https://github.com/basho/riak_repl/pull/652) and [riak_repl PR 725](https://github.com/basho/riak_repl/pull/725)] A memory leak problem was discovered in Riak's replication. The leak was discovered in a sink cluster. The real-time sink and source connection has been fixed to ensure cleaner shutdown of processes within real-time replication. Also, real-time queues have been refactored to ignore exceptions from pull delivery functions and the error for unregistered consumers now includes debugging information.
* [[Issue 796](https://github.com/basho/riak/issues/796)/[riak PR 798](https://github.com/basho/riak/pull/798)] The default Solaris 10 version of awk doesn't support gsub, so we've switched to xpg4 awk (nawk) instead. The tar on Solaris 10 has no support for creating compressed tar.gz files, so the tar files will be piped into gzip instead. And, finally, non-bash (e.g. ksh) shells may not have support for single instances of double quotes nested in single quotes, so we have escaped nested double quotes.
* [[Issue 804](https://github.com/basho/riak_core/issues/804)/[exometer PR 67](https://github.com/Feuerlabs/exometer_core/pull/67), [exometer PR 10](https://github.com/basho/exometer_core/pull/10), & [riak_core PR 817](https://github.com/basho/riak_core/pull/817)] When a node contains 2 vnodes, and each of those vnodes reports a 0 value for a statistic, those statistics (in exometer) were being thrown out due to some other special case handling. That handling has now been moved to the one function that needs it, rather than the general-purpose `exometer_histogram:get_value` where it was originally coded.
* [[riak_kv PR 1370](https://github.com/basho/riak_kv/pull/1370)] A race condition could cause small innacuracies in the stats if two processes tried to update the data for the same index at the same time. Write operations are now synchronized via the `global:trans/3` function.
* [[leveldb PR 197](https://github.com/basho/leveldb/pull/197)] MoveItems are eLevelDB's iterator objects and are reusable. MoveItems communicate the reuse desire to the hot threads logic via the resubmit() property. When resubmit() returns true, hot threads executes the same task again immediately. Prior to merging eLevelDB's hot threads with LevelDB's hot threads, only eLevelDB's code supported the resubmit() property. The support required an extra five lines of code within the thread loop routine. Unfortunately, leveldb had two thread loop routines. Only one of the two received the extra five lines during the merge. The additional five lines supporting the resubmit() property have been added to LevelDB's second thread loop.
* [[yokozuna commit](https://github.com/basho/yokozuna/commit/0b4486e9331048e371ea01aeb554fb42c5228d2f)]When making requests to Solr, if requests timed-out between the caller and ibrowse, ibrowse might still send a response slightly after the timeout. The post-timeout response would cause `yz_solrq_proc` to crash due to improperly handling the late reply message. This fix prevents late replies from causing crashes.
* [[yokozuna commit](https://github.com/basho/yokozuna/commit/f6e16f9cbf14b193ab447cfce0bb8d6971fb93a4)] When Riak search and active anti-entropy are both enabled, all keys written to Riak KV must be hashed and written to yokozuna hash trees, even if they are not written to Solr. This is done asynchronously, by passing the data along to the yokozuna batching infrastructure. Initially, the queues responsible for handling these non-indexed items were not included in the list of required queues, so they would be shut down by the synchronization mechanism that is designed to keep the queue processes running in sync with the partitions and indexes that are currently hosted by the node in question. Non-indexed queues are now included in the list of required queues so they stay active.



## Known Issues

### Replication Bucket Mismatch

When using MDC replication between Riak KV clusters with versions less than 2.2.0, replication may fail due to the following error:

```
riak_repl2_rtsink_conn:handle_info:236 drops due to missing or mismatched type
```

Please edit __/etc/riak/advanced.config__ and add the following on all Riak KV 2.2.0+ clusters:

```
{riak_repl, [
  {override_capability, [
    {default_bucket_props_hash, [{use, [consistent, datatype, n_val, allow_mult, last_write_wins]}] }
  ]}
]}
```

Once all of the Riak KV clusters have been upgraded to version 2.2.0 or greater, the workaround can be removed.



## Upgraded components

* Bitcask has been upgraded to version 2.0.4
* Cuttlefish has been upgraded to version 2.0.8
* eLevelDB has been upgraded to version 2.0.32
* Lager has been upgraded to version 3.2.2
* LevelDB has been upgraded to version 2.0.31
* node_package has been upgraded to version 3.1.1
* OTP has been upgraded to version R16B02_basho10
* Solr has been upgraded to version 4.10.4




## Deprecation Notification


* [Link Walking]({{<baseurl>}}riak/kv/2.2.0/developing/api/http/link-walking/) is deprecated and will not work if security is enabled.
* Key Filters are deprecated; we strongly discourage key listing in production due to the overhead involved, so it's better to maintain key indexes as values in Riak (see our [set data type]({{<baseurl>}}riak/kv/2.2.0/developing/data-types/sets/) as a useful tool for such indexes).
* JavaScript MapReduce is deprecated; we have expanded our [Erlang MapReduce]({{<baseurl>}}riak/kv/2.2.0/developing/app-guide/advanced-mapreduce/#mapreduce) documentation to assist with the transition.
* Riak search 1.0 is deprecated in favor of our Solr-based [Riak search 2.0]({{<baseurl>}}riak/kv/2.2.0/developing/usage/search/). Version 1.0 will not work if security is enabled.
* v2 replication (a component of Riak KV Enterprise) is superseded by v3 and will be removed in the future.
* Legacy vnode routing (an early mechanism for managing requests between servers) is deprecated. If `vnode_routing` is set to `legacy` via Riak KV's capability system, it should be removed to prevent upgrade problems in the future.
* Some users in the past have used Riak's internal API (e.g. `riak:local_client/1`); this API may change at any time, so we strongly recommend using our [Erlang client library](http://github.com/basho/riak-erlang-client/) (or [one of the other libraries]({{<baseurl>}}riak/kv/2.2.0/developing/client-libraries/) we support) instead.
