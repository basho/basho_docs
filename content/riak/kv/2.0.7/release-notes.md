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
canonical_link: "https://docs.basho.com/riak/kv/latest/release-notes"
---

Released June 30th, 2015.

This is a bugfix release addressing minor issues and making some improvements for speed and performance.

## Bugs Fixed

* [[Issue #481](https://github.com/basho/yokozuna/issues/481)/[PR #486](https://github.com/basho/yokozuna/pull/486)] - Riak Search was losing entries when YZ AAE trees expired. To address this, we fixed how we dealt with `default` bucket types when building yokozuna hashtrees.
* [[Issue #723](https://github.com/basho/riak/issues/723)/[PR #482](https://github.com/basho/yokozuna/pull/482) & [PR #773](https://github.com/basho/riak_test/pull/773)] - Search did not return consistent results when indexing a `bucket-type` with `sets` in a `map`. Now, a check for `map` embedded fields and counts is run, and the `default_schema` has been updated to return sets in query responses by storing them.
* [[Issue #70](https://github.com/basho/riak_ensemble/issues/70)/[PR #75](https://github.com/basho/riak_ensemble/pull/75)] - Some clusters were unable to start ensembles due to a block on ensemble peers within the leveldb synctree. Now leveldb synctree lock behavior is limited to local node.
* [[Issue #450](https://github.com/basho/yokozuna/issues/450)/[PR #459](https://github.com/basho/yokozuna/pull/459)] - Riak Search AAE was throwing errors when one was using keys/buckets/bucket_types with spaces.
* [[Issue #469](https://github.com/basho/yokozuna/pull/469)/[PR #470](https://github.com/basho/yokozuna/pull/470)] - Fix YZ stats name typo from 'throughtput' to 'throughput'.
* [[Issue #437](https://github.com/basho/yokozuna/issues/437)/[PR #458](https://github.com/basho/yokozuna/pull/458)] - `yz_events:handle_info` called with bad arguments.
* [[Issue #402](https://github.com/basho/yokozuna/pull/402)/[PR #463](https://github.com/basho/yokozuna/pull/463) & [PR #476](https://github.com/basho/yokozuna/pull/476) & [PR #515](https://github.com/basho/yokozuna/pull/515) & [PR #509](https://github.com/basho/yokozuna/pull/509)] - When creating a new search index via HTTP, HTTP responded before the index was available. Now you can change timeout via `index_put_timeout_ms` in the yokozuna section of advanced config.
* [Zendesk issue/[PR #487](https://github.com/basho/yokozuna/pull/487)] - Stops index creation loop on bad data.
* [[PR#732](https://github.com/basho/riak_core/pull/732)] - Handle long_schedule messages about ports correctly.
* [[Issue #1103](https://github.com/basho/riak_kv/issues/1103)/[PR #1143](https://github.com/basho/riak_kv/pull/1143)] - Corrupted or truncated KV vnode status files prevented nodes from starting up.
* **Riak Multi-Datacenter Replication** - A race condition bug was causing load testing log to show "Heartbeat is misconfigured..." error when no issue was present.
* **Riak Multi-Datacenter Replication** - When the provider process got stuck for an extended period of time, it would be sent a large amount of keep alive messages. Now, the keepalive timer is only reset upon receiving a keepalive request, and only one keepalive timer will ever be active.

## Improvements

* Extractor map is now stored using mochiglobal rather than the ring. [[PR #483](https://github.com/basho/yokozuna/pull/483)/[PR #779](https://github.com/basho/riak_test/pull/779)]
* Query plans are now cached by `n_val` rather than index name. [[PR #478](https://github.com/basho/yokozuna/pull/478)]
* Don't delete siblings in YZ when using SC or CRDTs. [[PR #452](https://github.com/basho/yokozuna/pull/452)]
* `keepalive` was added to prevent the `pb_listener` from hanging on to established connections in case of a network partition. [[PR #89](https://github.com/basho/riak_api/pull/89)]
* The `search.temp_dir` configuration variable can now set the temp directory used by Jetty. [[PR #413](https://github.com/basho/yokozuna/pull/413)]
* Add `riak_core` capability to handle newer versions fixing default `bucket_types` [[Issue #491](https://github.com/basho/yokozuna/issues/491)/[PR #492](https://github.com/basho/yokozuna/pull/492)]
* Enhance LevelDB write throttle to support single vnode loads (such as handoff) and other minor adjustments. [[PR #146](https://github.com/basho/leveldb/pull/146)]
* You can now use sh rather than bash for other *nixes.  An additional fetch call has been added (e.g. freebsd in download). [[PR #475](https://github.com/basho/yokozuna/pull/475)]
* Fullsync hardened for handoffs to avoid deadlocks. [[PR #1106](https://github.com/basho/riak_kv/pull/1106)/[PR #789](https://github.com/basho/riak_test/pull/789)]
* Log administrative stop, restart, reboot operations.[[PR #184](https://github.com/basho/node_package/pull/184)]
