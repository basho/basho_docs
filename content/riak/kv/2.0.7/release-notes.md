---
title: "Riak KV 2.0.7 Release Notes"
description: ""
project: "riak_kv"
project_version: "2.0.7"
menu:
  riak_kv-2.0.7:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: true
aliases:
  - /riak/2.0.7/community/release-notes
---

Released June 27th, 2016.

This is an LTS feature release, including new updates to batching and Solr, Cuttlefish configurations, an OTP upgrade, and fixes for two [product advisories](#product-advisories).


## Product Advisories

This release includes fixes for two product advisories:

*  [LevelDB Segfault advisory]({{<baseurl>}}community/productadvisories/leveldbsegfault/) - The AddDB() call now occurs after all object initialization is complete to eliminate a race condition that leads to segfault. [[LevelDB PR #184](https://github.com/basho/leveldb/pull/184)] 
* [Code Injection on Riak Init File]({{<baseurl>}}community/productadvisories/codeinjectioninitfiles/) - A thorough review of file ownership across the Riak KV package was done and several files, including riak init, were changed to tighten the ownership to root:root instead of riak:riak to prevent a potential code injection across all supported operating systems. Additionally, node_package was bumped to version 3.0.1. [[node_package PR #196](https://github.com/basho/node_package/pull/196)]



## New Features

* We've introduced a new batching system for Riak Search so indexing calls are no longer made synchronously when data is written to Riak. This allows Solr to process the data in chunks and Riak to move forward accepting new work at the vnode level without waiting for the call to Solr to happen. Out-of-the-box performance should be similar to Riak 2.0.6 with Search enabled. However, additional configuration options (see "Cuttlefish configurations…" below) will allow you to set the batching parameters based on your needs and have, in certain cases, led to significantly higher write throughput to Solr.
  * [[PR #648](https://github.com/basho/yokozuna/pull/648)]
* Cuttlefish configurations have been updated to support the Riak Search batching updates. These configs are tunable via the riak.conf file. (Note: Changes to this file require a restart of Riak). You can control the behavior of batching through various [new Cuttlefish parameters]({{< baseurl >}}riak/kv/2.0.7/configuring/reference/#search). These parameters guide Cuttlefish operation, Solr integration, and statistics on Riak performance.
  * [[PR #614](https://github.com/basho/yokozuna/pull/614)]
* Our Erlang/OTP has been updated to version R16B02_basho10 and included in this release. This update includes bugfixes and improvements for ERTS, as well as bugfixes for SSL.
  * You can read the complete release notes for Erlang/OTP [here](https://github.com/basho/otp/blob/basho-otp-16/BASHO-RELEASES.md).
* We've also updated the Riak KV distribution on the Amazon Linux AMI to 2.0.7! Our AMI is built upon Amazon Linux 2015.03. Due to Amazon's processes, it will take several days after the release of 2.0.7 to show up on Amazon.


## Bugs Fixed

* [[Issue #1064](https://github.com/basho/riak_kv/issues/1064)/[PR #1331 ](https://github.com/basho/riak_kv/pull/1331)] When using the `max_memory` setting in the memory backend, a list of timers is kept in ETS. In certain circumstances, these timer references were not deleted when the item had expired, or when a new value was put to the table.The timer references are now appropriately deleted.
* [[Issue #663](https://github.com/basho/riak_repl/issues/663)/[riak_repl PR #706](https://github.com/basho/riak_repl/pull/706)] When using AAE fullsync, the fullsync source would crash when attempting to send a key with a bucket type and produce a long, unhelpful error. Fullsync has now been updated to use the new replication wire protocol to avoid this behavior. While this means that fullsync has better compatibility with bucket types, only fully-upgraded clusters on both ends of a replication connection will be able to leverage this new capability.
* [[Issue #666](https://github.com/basho/riak_repl/issues/666)/[riak_repl PR # 667](https://github.com/basho/riak_repl/pull/667)] In Riak 2.0.5, `riak_repl2_rtsource_conn` crashes with `function_clause` on a handle_call because its state is replaced by `{noreply, state}`. This extra noreply seems to originate from the reconnect function in the newly added rebalance code. The extraneous noreply has been removed from `riak_repl2_rtsource_conn:reconnect`. 
* [[Issue #1086](https://github.com/basho/riak_kv/issues/1086)/[PR #1167](https://github.com/basho/riak_kv/pull/1167)] `riak-admin status` and `riak-admin stat` failed after consistent PUT or GET. To avoid this crash, `riak_kv_stat` now handles undefined object size during consistent PUT/GET.
* [[PR #1282](https://github.com/basho/riak_kv/pull/1282)] Unregister per-vnode statistics when cleanly shutting down. However if the vnode crashes, the terminate callback will not be executed. 
* [[mochiweb PR #20](https://github.com/basho/mochiweb/pull/20)] Mochiweb had a bug wherein the  API could return answers out of order if an unexpected or out-of-state
message was sent to the endpoint. This bug also caused mochiweb to return an erroneous 400 response when an unexpected or out-of-state message was sent to the socket acceptor's mailbox. These issues seemed to occur predominantly during interaction with a load balancer. In some cases, the result of this bug was data crossing request boundaries. The acceptor now consumes the spurious messages preventing these issues. 
* [[Bitcask PR #229](https://github.com/basho/bitcask/pull/229)] This stops the `bitcask_file` process leak after a failure to open file and logs the file open failure.
* [[Issue #1069](https://github.com/basho/riak_kv/issues/1069)/[PR #1162](https://github.com/basho/riak_kv/pull/1162)] An empty list was passed to `hd` and  `most_recent_content` causing errors. A bucket property validation was added to ensure that  if last_write_wins is true then dvv_enabled cannot also be true. The bucket property validation applies to `create` and `update` on any buckets or bucket types. Since this validation applies to a combination of two separate properties in both the create and update scenarios, a new
`validate_post_merge` function was added, which allows for validation on properties after they have been fully resolved and merged.
* [[Issue #498](https://github.com/basho/yokozuna/issues/498)/[yokozuna PR #532](https://github.com/basho/yokozuna/pull/532)] In `yz_wm_search:search`, any error other than insufficient_vnodes_available will go to the {RespHeaders, Body} case, causing the request to fail in scrub_headers(error) and obscuring the real error. More informative errors have been added.
* [[Issue #1156](https://github.com/basho/riak_kv/issues/1156)/[PR #1363](https://github.com/basho/riak_kv/pull/1363)] During read repair, if the reconciled object exceeded sibling limits or object size it could not be repaired. A new read repair element has been added so that read-repair ignores limits.
* [[Issue #559](https://github.com/basho/yokozuna/issues/559)/[yokozuna PR #549](https://github.com/basho/yokozuna/pull/549) & [yokozuna PR #561](https://github.com/basho/yokozuna/pull/561)] Riak search returned inconsistent results for an unknown reason, though deleting data is assumed to have something to do with it.  To address the root causes of inconsistent search results, search delete operations are checked for crdts/lww=true, allow_mult=false, and strong consistency by checking for tombstones. 
* [[Issue #711](https://github.com/basho/riak_repl/issues/711)/[riak_repl PR #712](https://github.com/basho/riak_repl/pull/712)] Whenever there was a `conn_error`, `riak_core_connection_mgr_stats` created or updated a stat which included the entire error,  but  in the case of a noproc error, the PID and port number would be included as part of the error tuple essentially making the error unique. When a node hit such an error repeatedly, it could lead to hundreds of thousands folsom spirals and counters being created until all available memory was used by these statistics. To fix this, only the primary atom of the error reason is now saved in the statistic, since including the entire error reason tuple in the connection error can cause each connection error to create a new folsom statistic.
* [[Issue #796](https://github.com/basho/riak/issues/796)/[PR #798](https://github.com/basho/riak/pull/798)] The default Solaris 10 version of awk doesn't support gsub, so we've switched to xpg4 awk (nawk) instead. The tar on Solaris 10 has no support for creating compressed tar.gz files, so the tar files will be piped into gzip instead. And, finally, non-bash (e.g. ksh) shells may not have support for single instances of double quotes nested in single quotes, so we have escaped nested double quotes.
* [[Issue #804](https://github.com/basho/riak_core/issues/804)/[exometer PR #67](https://github.com/Feuerlabs/exometer_core/pull/67), [exometer PR #10](https://github.com/basho/exometer_core/pull/10), & [PR #817](https://github.com/basho/riak_core/pull/817)] When a node contains 2 vnodes, and each of those vnodes reports a 0 value for a statistic, those statistics (in exometer) were being thrown out due to some other special case handling. That handling has now been moved to the one function that needs it, rather than the general-purpose `exometer_histogram:get_value` where it was originally coded.
* [[PR #1370](https://github.com/basho/riak_kv/pull/1370)] A race condition could cause small innacuracies in the stats if two processes tried to update the data for the same index at the same time. Write operations are now synchronized via the `global:trans/3` function.
* [[Issue #503](https://github.com/basho/yokozuna/issues/503)/[PR #528](https://github.com/basho/yokozuna/pull/528)] Riak Search limited the maximum size of a search query if you used the highlight or facet feature, so we added the ability to handle POSTs for search queries when given the content-type `application/x-www-form-urlencoded`. A `415` error is returned if another content-type is used.
* [[Issue #1178](https://github.com/basho/riak_kv/issues/1178)/[repl PR #742](https://github.com/basho/riak_repl/pull/742)] **This fix applies ONLY to Riak Enterprise.** `riak_kv_get_fsm:start_link` did not consistently link the caller to the new FSM process. This would cause the supervisor to end up with an endlessly growing list of workers, since it had no way of seeing when a worker died. These issues could cause extended shutdown times as the supervisor attempts to iterate through millions of dead PIDs. To fix this issue, the process is now started directly rather than via the supervisor API call. Since these processes are normally started under a sidejob, there is no reason to run them under a supervisor. **Note:** We recommend not disabling overload protection. If you use replication and completely disable overload protection, you may run into issues.
* [[PR #830](https://github.com/basho/riak_core/pull/830)]  Several bugs were found with hash trees that, in rare cases, could cause AAE to fail to repair missing data.
* [[Bitcask PR #227](https://github.com/basho/bitcask/pull/227)] A try/after block has been added around the `hintfile_validate_loop/3` to keep descriptors from being leaked each time Bitcask is opened.
* [[PR #821](https://github.com/basho/riak_core/pull/821)] `ssl:getopt` has been removed because it can block SSL sockets, which would cause replication to become blocked and potentially bring down a large number of nodes in a cluster. All getopts have been combined into one call which is known to be safe and not block. 


## Other Changes

* RTQ has been refactored to ignore exceptions from pull delivery functions. The error that occurs in the event that the consumer is not registered to include information needed to debug RTQ has also been improved. [[riak_repl PR #725](https://github.com/basho/riak_repl/pull/725)]
* In case of downgrade from a version of Riak KV that uses new binary types other than term-to-binary or other binaries, this PR ensures that, at minimum, KV doesn't crash when trying to read those keys back. Also, if an "X-Riak-Val-Encoding" metadata tag is provided, the binary is stored with that value as its magic byte to provide round-trip support. [[PR #1228](https://github.com/basho/riak_kv/pull/1228)]
* The default for `ERL_MAX_PORTS (+Q) ` has been increased to 262144. This change should help mitigate a fairly rare issue where Erlang would run out of available ports. This issue was seen especially when using the multi-backend, as many more files could be opened depending on the multi-backend configuration.[[Issue #801](https://github.com/basho/riak/issues/801)/[Cuttlefish PR #208](https://github.com/basho/cuttlefish/pull/208)]. 
* This PR modified the riak_core_bucket all_n function to include n_vals from active bucket types, not just buckets stored in the ring. This function is used in riak_kv, and in some cases (e.g., riak_kv_ensemble), n_vals from bucket types are added. However, in the calculation of the set of AAE trees, n_vals from bucket types is conspicuously missing, when in fact it is required. This PR consolidates the logic of active n_val calculations in one place. [[PR # 832](https://github.com/basho/riak_core/pull/832) and [PR #1399](https://github.com/basho/riak_kv/pull/1399)]


## Upgraded components

* Bitcask has been upgraded to version 1.7.2p3
* Cuttlefish has been upgraded to version 2.0.2p2
* eLevelDB has been upgraded to version 2.0.22
* LevelDB has been upgraded to version 2.0.22
* node_package has been upgraded to version 3.0.1
* OTP has been upgraded to version R16B02_basho10
