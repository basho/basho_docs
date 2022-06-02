---
title: "Riak KV 2.0.4 Release Notes"
description: ""
project: "riak_kv"
project_version: "2.0.4"
menu:
  riak_kv-2.0.4:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: true
aliases:
  - /riak/2.0.4/community/release-notes
---

## Changes

* Improve AAE fullsync by estimating total number of keys. This allows
    for better sizing of the bloom filter when performing a
    bloom-fold-based exchange and enhances the adaptive exchange logic
    that was added previously in Riak 2.0.1. In that version, the new
    `riak_repl.fullsync_direct_percentage_limit` setting was added,
    which allowed the limit to be specified as a percentage of total
    keys rather than as a fixed number.
  * [riak_repl/623](https://github.com/basho/riak_repl/pull/623)
  * [riak_core/633](https://github.com/basho/riak_core/pull/633)
  * [riak_kv/1030](https://github.com/basho/riak_kv/pull/1030)
* Added `last_fullsync_complete` stat tracking [riak_repl/642](https://github.com/basho/riak_repl/pull/642)
* Expose AAE tree status in the logs
    [riak_kv/1056](https://github.com/basho/riak_kv/pull/1056)
* Improve AAE fullsync by using breadth-first exchange
  * [riak_kv/1026](https://github.com/basho/riak_kv/pull/1026)
  * [riak_core/629](https://github.com/basho/riak_core/pull/629)
* Riak now uses Exometer for collecting metrics and has an improved CLI
    for finding and displaying them
  * [riak/651](https://github.com/basho/riak/pull/651)
  * [riak_api/70](https://github.com/basho/riak_api/pull/70)
  * [riak_api/71](https://github.com/basho/riak_api/pull/71)
  * [riak_api/72](https://github.com/basho/riak_api/pull/72)
  * [riak_core/650](https://github.com/basho/riak_core/pull/650)
  * [riak_core/654](https://github.com/basho/riak_core/pull/654)
  * [riak_core/655](https://github.com/basho/riak_core/pull/655)
  * [riak_core/663](https://github.com/basho/riak_core/pull/663)
  * [riak_core/665](https://github.com/basho/riak_core/pull/665)
  * [riak_core/668](https://github.com/basho/riak_core/pull/650)
  * [riak_kv/1044](https://github.com/basho/riak_kv/pull/1044)
  * [riak_kv/1048](https://github.com/basho/riak_kv/pull/1048)
  * [riak_kv/1050](https://github.com/basho/riak_kv/pull/1050)
  * [riak_kv/1058](https://github.com/basho/riak_kv/pull/1058)
  * [riak_kv/1059](https://github.com/basho/riak_kv/pull/1059)
  * [riak_kv/1067](https://github.com/basho/riak_kv/pull/1067)
  * [riak_pipe/89](https://github.com/basho/riak_pipe/pull/89)
  * [riak_pipe/90](https://github.com/basho/riak_pipe/pull/90)
  * [riak_pipe/91](https://github.com/basho/riak_pipe/pull/91)
  * [riak_search/168](https://github.com/basho/riak_search/pull/168)
  * [riak_search/169](https://github.com/basho/riak_search/pull/169)
  * [riak_search/172](https://github.com/basho/riak_search/pull/172)
  * [yokozuna/434](https://github.com/basho/yokozuna/pull/434)
  * [yokozuna/435](https://github.com/basho/yokozuna/pull/435)
  * [yokozuna/440](https://github.com/basho/yokozuna/pull/440)
  * [yokozuna/441](https://github.com/basho/yokozuna/pull/441)
  * [riak_ee/296](https://github.com/basho/riak_ee/pull/296)
* Internal components can process partial configurations as if they were
    in the global config file. This supports the handoff visibility
    changes.
  * [cuttlefish/174](https://github.com/basho/cuttlefish/pull/174)
* Riak now uses [Clique](https://github.com/basho/clique) for managing
    CLI commands. The initial commands covered exist to increase
    visibility into the handoff process and provide Cuttlefish config
    support from the CLI.
  * [riak_core/669](https://github.com/basho/riak_core/pull/669)
  * [riak_core/674](https://github.com/basho/riak_core/pull/674)
  * [riak_core/675](https://github.com/basho/riak_core/pull/675)
  * [riak_core/677](https://github.com/basho/riak_core/pull/677)
  * [riak_core/678](https://github.com/basho/riak_core/pull/678)
  * [riak_core/679](https://github.com/basho/riak_core/pull/679)
  * [riak_core/680](https://github.com/basho/riak_core/pull/680)
  * [riak_core/681](https://github.com/basho/riak_core/pull/681)
  * [riak_core/682](https://github.com/basho/riak_core/pull/682)
  * [riak_core/685](https://github.com/basho/riak_core/pull/685)
  * [riak_core/686](https://github.com/basho/riak_core/pull/686)
  * [riak_core/692](https://github.com/basho/riak_core/pull/692)
  * [riak_kv/1068](https://github.com/basho/riak_kv/pull/1068)
  * [riak_core/695](https://github.com/basho/riak_core/pull/695)
  * [riak/654](https://github.com/basho/riak/pull/654)
  * [riak/655](https://github.com/basho/riak/pull/655)
  * [riak/656](https://github.com/basho/riak/pull/656)
  * [riak/657](https://github.com/basho/riak/pull/657)
  * [riak_ee/298](https://github.com/basho/riak_ee/pull/298)
  * [riak_ee/303](https://github.com/basho/riak_ee/pull/303)
  * [riak_ee/304](https://github.com/basho/riak_ee/pull/304)
  * [riak_ee/305](https://github.com/basho/riak_ee/pull/305)

## AAE Fullsync Performance Improvements

In version 2.0.4, a number of improvements to the AAE fullsync feature
were added, improvements that were initially introduced in Riak version
1.4.12 but had not yet been introduced into Riak 2.0.x. The following
improvements have been introduced:

* Data transfers are now pipelined instead of individually acknowledged,
    in the name of maximizing throughput
* The code now avoids redundant scans of replcated data. For a
    replication factor of, say, 3 (i.e. an `n_val` of 3), only a third
    of the relevant vnodes are scanned.
* The algorithm is now smarter about small differences

Performing an AAE fullsync operation between two identical clusters is
very fast. The time it takes to finish an AAE fullsync is much closer to
linear on the number of differences between the clusters. Below are the
results of one of our benchmarks. Two 8-node clusters were used, each
with the following characteristics:

* Each node has 630 GB of SSD storage
* Each node is running only two vnodes (16-partition ring)
* 23 million objects per vnode
* 99% of objects are 8 KB
* 1% of objects are a mix of 8 KB to 40 MB outliers
* 450 GB of data on disk per server

The results for each stage of fullsync:

* Empty cluster to empty cluster: 6 seconds
* Full cluster to empty cluster: 14 hours, 40 minutes
* 10% changes: 3 hours, 45 minutes
* 1% changes: 40.5 minutes
* No changes: 42.5 seconds

## Fixes

* Fix stats process crash if no leader
    [riak_repl/645](https://github.com/basho/riak_repl/pull/645)
* Address some minor bugs around establishing SSL connections
    [riak_repl/644](https://github.com/basho/riak_repl/pull/644)
* 2.0 port of AAE transient filesystem failures
    [riak_repl/640](https://github.com/basho/riak_repl/pull/640)
* Fix error/retry exit counts on location down messages
    [riak_repl/639](https://github.com/basho/riak_repl/pull/639)
* Fix deadlock when performing AAE fullsync over SSL (Erlang VM patch)
* Prevent servers from accepting SSLv3 connections (Erlang VM patch)
* The map Data Type is now more efficient when merging
  * [riak_dt/110](https://github.com/basho/riak_dt/pull/110)
  * [riak_kv/1063](https://github.com/basho/riak_kv/pull/1063)
  * [riak_kv/1065](https://github.com/basho/riak_kv/pull/1065)
* Fix a case in which sibling explosion could occur during handoff
  * [riak_core/672](https://github.com/basho/riak_core/pull/672)
  * [riak_kv/1062](https://github.com/basho/riak_kv/pull/1062)
* Special handling for the `net_ticktime` setting in admin scripts
    [node_package/166](https://github.com/basho/node_package/pull/166)
* Add a missing function clause in `riak_kv_node` that could result in
    crashes [riak_core/693](https://github.com/basho/riak_core/pull/693)
* Avoid timeouts when handoff sender is folding over a large number of
    keys [riak_core/627](https://github.com/basho/riak_core/pull/627)
* No more extra work for handoff sender after TCP error makes that work
    useless [riak_core/626](https://github.com/basho/riak_core/pull/626)
* Report error when failing to open file instead of crashing when
    calling `riak_core_util:replace_file/2`
    [riak_core/646](https://github.com/basho/riak_core/pull/646)
* Debian package fixes
  * [node_package/173](https://github.com/basho/node_package/pull/173)
  * [riak_ee/300](https://github.com/basho/riak_ee/pull/300)
* Ensure creation of ensembles when strongly consistent bucket types
    with different `n_val`s from default bucket type do not yet have
    buckets [riak_kv/1008](https://github.com/basho/riak_kv/pull/1008)
* Avoid SSL deadlocks that occur when sending data bidirectionally using
    Erlang SSL sockets. The fix is a patch to the Erlang VM shipped with
    the build.

## Merged Pull Requests

* bitcask/202: [Unpin cuttlefish from tag](https://github.com/basho/bitcask/pull/202)
* node_package/166: [add support for custom exit codes to nodetool rpc & friends](https://github.com/basho/node_package/pull/166)
* node_package/170: [Handle net_ticktime argument specially. Resolves RIAK-1281](https://github.com/basho/node_package/pull/170)
* node_package/173: [Fixes for debian control template](https://github.com/basho/node_package/pull/173)
* riak_kv/1008: [Use SC bucket types and buckets to know ensembles](https://github.com/basho/riak_kv/pull/1008)
* riak_kv/1026: [Update to use new breadth-first AAE exchange](https://github.com/basho/riak_kv/pull/1026)
* riak_kv/1027: [Attempt to make vnodeids on the same node unique](https://github.com/basho/riak_kv/pull/1027)
* riak_kv/1030: [Implement key count estimation via AAE trees](https://github.com/basho/riak_kv/pull/1030)
* riak_kv/1035: [Consuming the vnode_status output in stats was brittle](https://github.com/basho/riak_kv/pull/1035)
* riak_kv/1044: [Introduce exometer metrics into 2.0](https://github.com/basho/riak_kv/pull/1044)
* riak_kv/1048: [Remove afunix from EXOMETER_PACKAGES.](https://github.com/basho/riak_kv/pull/1048)
* riak_kv/1050: [Add skipped_repairs stat.](https://github.com/basho/riak_kv/pull/1050)
* riak_kv/1056: [Expose logs about AAE tree status.](https://github.com/basho/riak_kv/pull/1056)
* riak_kv/1058: [Use exometer_core](https://github.com/basho/riak_kv/pull/1058)
* riak_kv/1059: [add missing aliases](https://github.com/basho/riak_kv/pull/1059)
* riak_kv/1062: [Fixes sibling explosion bug caused by forwarding coordination](https://github.com/basho/riak_kv/pull/1062)
* riak_kv/1063: [Bugfix for precondition_context test failures.](https://github.com/basho/riak_kv/pull/1063)
* riak_kv/1065: [Fix bug related to riak_dt#110 serialization changes](https://github.com/basho/riak_kv/pull/1065)
* riak_kv/1067: [Make read-repair stats be 'proplist' instead of 'value'](https://github.com/basho/riak_kv/pull/1067)
* riak_kv/1068: [Set schema dirs for riak_core in riak_kv_test_util](https://github.com/basho/riak_kv/pull/1068)
* riak_core/626: [Allow handoff sender to abort handoff by throw'ing from fold fun](https://github.com/basho/riak_core/pull/626)
* riak_core/627: [Handoff sender sends sync periodically](https://github.com/basho/riak_core/pull/627)
* riak_core/629: [Add breadth-first AAE exchange](https://github.com/basho/riak_core/pull/629)
* riak_core/633: [Implement key count estimation via AAE trees](https://github.com/basho/riak_core/pull/633)
* riak_core/646: [Tell the caller when you fail to open a file, too](https://github.com/basho/riak_core/pull/646)
* riak_core/650: [Introduce exometer metrics into 2.0](https://github.com/basho/riak_core/pull/650)
* riak_core/654: [Remove afunix from EXOMETER_PACKAGES.](https://github.com/basho/riak_core/pull/654)
* riak_core/655: [add skipped_read_repairs to legacy stat map](https://github.com/basho/riak_core/pull/655)
* riak_core/663: [use exometer_core instead of exometer](https://github.com/basho/riak_core/pull/663)
* riak_core/665: [add missing aliases](https://github.com/basho/riak_core/pull/665)
* riak_core/668: [Replaces feuerlabs/exometer with basho/exometer](https://github.com/basho/riak_core/pull/668)
* riak_core/669: [Add transfer_limit config callback to handoff_cli](https://github.com/basho/riak_core/pull/669)
* riak_core/672: [Fixes sibling explosion bug caused by forwarding coordination requests d...](https://github.com/basho/riak_core/pull/672)
* riak_core/674: [Feature/revised/riak cli handoff status 1239](https://github.com/basho/riak_core/pull/674)
* riak_core/675: [rename riak_cli to clique](https://github.com/basho/riak_core/pull/675)
* riak_core/677: [Standardize usage/command registration](https://github.com/basho/riak_core/pull/677)
* riak_core/678: [Integration/riak admin handoff team](https://github.com/basho/riak_core/pull/678)
* riak_core/679: [Handle down nodes consistently](https://github.com/basho/riak_core/pull/679)
* riak_core/680: [Make riak_core_status:active_partitions safe if a node is down.](https://github.com/basho/riak_core/pull/680)
* riak_core/681: [Cleanup after review](https://github.com/basho/riak_core/pull/681)
* riak_core/682: [add riak-admin handoff config support](https://github.com/basho/riak_core/pull/682)
* riak_core/685: [Improve handoff enable/disable config naming](https://github.com/basho/riak_core/pull/685)
* riak_core/686: [add whitelisted config variables to riak_core_handoff_cli](https://github.com/basho/riak_core/pull/686)
* riak_core/692: [Allow schema loading with an environment variable](https://github.com/basho/riak_core/pull/692)
* riak_core/693: [mod_set_forwarding crashes [JIRA: RIAK-1459]](https://github.com/basho/riak_core/pull/693)
* riak_core/695: [change git url for clique to fix PR #694 [JIRA: RIAK-1460]](https://github.com/basho/riak_core/pull/695)
* riak/651: [Use exometer_core](https://github.com/basho/riak/pull/651)
* riak/654: [change riak_cli to clique](https://github.com/basho/riak/pull/654)
* riak/655: [Add handoff; remove cluster members](https://github.com/basho/riak/pull/655)
* riak/656: [Add set, show and describe to usage](https://github.com/basho/riak/pull/656)
* riak/657: [Integration/riak admin handoff team](https://github.com/basho/riak/pull/657)
* riak_api/66: [Do not treat errors as success](https://github.com/basho/riak_api/pull/66)
* riak_api/70: [Exometer metrics in 2.0](https://github.com/basho/riak_api/pull/70)
* riak_api/71: [Remove afunix from EXOMETER_PACKAGES.](https://github.com/basho/riak_api/pull/71)
* riak_api/72: [Use exometer_core & exometer aliases](https://github.com/basho/riak_api/pull/72)
* yokozuna/430: [merged develop into 2.0](https://github.com/basho/yokozuna/pull/430)
* yokozuna/431: [update-readme-to-up-to-date-riak-version](https://github.com/basho/yokozuna/pull/431)
* yokozuna/432: [merge dev into 2.0 for test readme updates](https://github.com/basho/yokozuna/pull/432)
* yokozuna/434: [Introduce exometer metrics into 2.0](https://github.com/basho/yokozuna/pull/434)
* yokozuna/435: [Remove afunix from EXOMETER_PACKAGES.](https://github.com/basho/yokozuna/pull/435)
* yokozuna/440: [Use exometer_core & exometer aliases](https://github.com/basho/yokozuna/pull/440)
* yokozuna/441: [minor alias typo](https://github.com/basho/yokozuna/pull/441)
* riak_search/168: [Introduce exometer metrics into 2.0](https://github.com/basho/riak_search/pull/168)
* riak_search/169: [Remove afunix from EXOMETER_PACKAGES.](https://github.com/basho/riak_search/pull/169)
* riak_search/172: [Fix dep error in rebar.config](https://github.com/basho/riak_search/pull/172)
* cuttlefish/165: [Add case for incomplete/syntax-error when parsing conf file.](https://github.com/basho/cuttlefish/pull/165)
* cuttlefish/166: [Add cuttlefish:warn/1 for use in translations.](https://github.com/basho/cuttlefish/pull/166)
* cuttlefish/172: [Backport #169 and #171 from develop](https://github.com/basho/cuttlefish/pull/172)
* cuttlefish/173: [Handle case where advanced.config contains > 1 terms](https://github.com/basho/cuttlefish/pull/173)
* cuttlefish/174: [Add cuttlefish_generator:minimal_map/2.](https://github.com/basho/cuttlefish/pull/174)
* riak_dt/110: [Swap orddict for dict in orswot and map](https://github.com/basho/riak_dt/pull/110)
* riak_pipe/89: [Introduce exometer metrics into 2.0](https://github.com/basho/riak_pipe/pull/89)
* riak_pipe/90: [Remove afunix from EXOMETER_PACKAGES.](https://github.com/basho/riak_pipe/pull/90)
* riak_pipe/91: [Use exometer_core & exometer aliases](https://github.com/basho/riak_pipe/pull/91)

## Added Repositories

* [Clique](https://github.com/basho/clique)
* [Exometer](https://github.com/basho/exometer_core)

## Known Issues

* Clique can't handle config values with Cuttlefish transformations

## Download

Please see our [downloads]({{< baseurl >}}riak/kv/latest/downloads/)
page.

## Feedback

We would love to hear from you. You can reach us at any of the following
links:

* http://lists.basho.com/mailman/listinfo/riak-users_lists.basho.com
* https://github.com/basho/basho_docs
* https://github.com/basho/riak

Or via email at **info@basho.com**.

## Riak 2.0.3 Release Notes

## Merged PRs
* riak/621: [Introduce exometer metrics into 2.0](https://github.com/basho/riak/pull/621)
* riak/623: [Remove afunix from EXOMETER_PACKAGES.](https://github.com/basho/riak/pull/623)
* riak/647: [remove afunix from reltool.config](https://github.com/basho/riak/pull/647)
