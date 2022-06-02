---
title: "Riak KV 2.2.2 Release Notes"
description: ""
project: "riak_kv"
project_version: "2.2.2"
menu:
  riak_kv-2.2.2:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: true
aliases:
  - /riak/2.2.2/community/release-notes
  - /riak/kv/2.2.2/intro-v20
  - /riak/2.2.2/intro-v20
  - /riak/kv/2.2.2/introduction
---


Released March 28, 2017.

In KV 2.2.1 we [fixed](https://github.com/basho/node_package/pull/183) an  [issue](https://github.com/basho/riak/issues/509) with nodetool, allowing it to work with SSL options in vm.args. However, `exec` was, in specific cases, calling the new NODETOOL alias, and four riak-admin commands (`handoff`, `get`, `set`, and `describe`) were using `exec` to allow them to run properly on all of our supported operating systems, meaning running those commands caused the following error message:
```
[root@node1 ~]# riak-admin handoff
/usr/sbin/riak-admin: line 1072: exec: NODETOOL: not found
```
This bugfix release resolves the issues with `exec` and `NODETOOL`, allowing `handoff`, `get`, `set`, and `describe` to function properly. We have located the broken QA test that misidentified these commands as working and have corrected it for future releases.


## Bugs Fixed

* [[riak PR 904](https://github.com/basho/riak/pull/904)] Remove `exec` from calls to NODETOOL alias entirely.
* [[yokozuna PR 731](https://github.com/basho/yokozuna/pull/731)] The default solrconfig.xml file changed slightly in 2.0, which was unaccounted for in 2.2's upgrade logic. Seamless upgrades to new solrconfigs from all 2.0.x versions are now possible.


## Riak KV 2.2.1 Release Notes


This is a bugfix release built on Riak KV 2.2.0. It addresses an issue specific to Riak KV 2.2, where a reboot of the cluster might be required to resume the upgrade of AAE trees when  nodes are joined to the cluster during a hashtree upgrade. This release also continues our work to resolve technical debt and some long-standing issues.


### Bugs Fixed

* [[riak Issue 509](https://github.com/basho/riak/issues/509)/[node_package PR 183](https://github.com/basho/node_package/pull/183)] SSL options in vm.args would let nodetool stop working. This was fixed by having node_package use an alias for nodetool rather than an environment variable.
* [[riak_core PR 781](https://github.com/basho/riak_core/pull/781)] The `dropped_vnode_requests_total` stat was not incremented correctly in Riak stats due to being incorrectly projected in `riak_core_stat`. This has been fixed by setting stat name correctly as `dropped_vnode_requests`.
* [[riak PR 886](https://github.com/basho/riak/pull/886), [riak_ee PR 412](https://github.com/basho/riak_ee/pull/412), and [node_package PR 210](https://github.com/basho/node_package/pull/210)] Atom usage in `riak` and `riak-admin` commands has been restricted to 1000. Previously, the OS PID was being used as a pseudo-random number generator, but the range was too large since each nodename used would generate an entry in the atom table. `riak-admin top` uses $$ to randomize the name used to connect to the local Riak node, and the large range of possible OS PIDs can result in atom table exhaustion on long running nodes/clusters. The nodename used by `riak top` has been changed to match `riak-admin top` convention, using `$RANDOM` with the range restricted to 1-1000.
* [[riak_core Issue 855](https://github.com/basho/riak_core/issues/855)/[riak_core PR 886](https://github.com/basho/riak_core/pull/886)] If updates to the same key in the ring metadata occurred on different nodes during the same second, they were not reconciled. This could lead to nodes flip-flopping the value and many gossip messages causing extremely high message queues and heap usage by the gossip processes. Node names have been added to the `merge_meta` comparison to avoid this issue.
* [[bitcask Issue 251](https://github.com/basho/bitcask/issues/25) & [bitcask Issue 242](https://github.com/basho/bitcask/issues/242)/ [bitcask PR 252](https://github.com/basho/bitcask/pull/252)] Attempting to open a removed hintfile would either crash or create the file depending on what mode (Erlang or NIF) you were in. To resolve this, the O_CREAT flag is used when opening files to match the behavior of Erlang mode.
* [[riak_kv Issue 1537](https://github.com/basho/riak_kv/issues/1537) / [ riak_kv commit 296d3](https://github.com/basho/riak_kv/commit/354ed7d353f06eab472d53677551edcdada296d3)] When adding a new node with legacy trees to the cluster after the index_hashtree upgrade has started or finished, the new node could end up in a situation where the trees will never upgrade. To remedy this, logic has been added to skip waiting for all exchanges to complete if other nodes in the cluster have already flipped either their `version` or `pending_version` variable. A single node reporting back an upgraded variable is good enough to go ahead and start the upgrade.
* [[riak_kv Issue 1561](https://github.com/basho/riak_kv/issues/1561) / [riak_kv PR 1633](https://github.com/basho/riak_kv/pull/1633)  A bug was found that prevents riak_ensemble from writing PUTs during handoff. It has been resolved so that PUTs proceed as expected during handoff.
* [[riak_core PR 887](https://github.com/basho/riak_core/pull/887)] If `riak_core_vnode_master:sync_spawn_command` was called, it would send a `gen_event all-state` event to the proxy for forwarding. However, in `handle_overload`, the code only had case clauses for '$gen_event' and not '$gen_all_state_event', meaning the event would be passed to `handle_overload_info` instead of `handle_overload_reques` which would skip the vnode callback code that sends a response. Then certain operations would hang during overload since the caller to `sync_spawn_command` would never be sent a response. A clause for `$gen_all_state_event` has been added to fix potential hang due to vnode proxy overload bug.
* [[riak_repl PR 766](https://github.com/basho/riak_repl/pull/766)] Status output is expected to be a list. When the repl leader is undefined the status will now match the regular format: a list.
* [[riak_kv PR 1527](https://github.com/basho/riak_kv/pull/1527)] A race condition was occurring where a `gen_fsm` timeout event was not reliably sent, even when the timeout was set to zero, and another message or event could preempt or unset the timeout. To fix this, a timeout event is manually sent using `gen_fsm:send_event`.


### Known Issues

#### Replication Bucket Mismatch

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



### Other Changes

* Debug logging for ring metadata merges and ring membership has been added. There have been a few issues where rapidly updating the ring results in suboptimal behavior, and it has been difficult to debug due to the lack of logging in the riak_core_ring module. This logging can be enabled as needed. [[riak_core PR 901](https://github.com/basho/riak_core/pull/901)]
* riak_kv has been changed such that updating an object also sends the old object being replaced. From that old object, we can extract any siblings and generate associated document ids to delete in Solr. [[riak_kv PR 1520](https://github.com/basho/riak_kv/pull/1520)]


### Upgraded components

* Bitcask has been upgraded to version 2.0.8
* cluster_info has been upgraded to version 2.0.5
* cuttlefish has been upgraded to version 2.0.11
* eLevelDB has been upgraded to version 2.0.34
* exometer has been upgraded to version basho9
* lager has been upgraded to version 3.2.2
* LevelDB has been upgraded to version 2.0.34
* node_package has been upgraded to version 4.0.1
* OTP remains at version R16B02_basho10
* riak_api has been upgraded to version 2.1.6
* riak_control has been upgraded to version 2.1.6
* riak_core has been upgraded to version 2.1.9
* riak_ensemble has been upgraded to version 2.1.8
* riak_kv has been upgraded to version 2.1.7
* riak_pipe has been upgraded to version 2.1.5
* yokozuna has been upgraded to version 2.1.9


### Deprecation Notification

* [Link Walking]({{<baseurl>}}riak/kv/2.2.2/developing/api/http/link-walking/) is deprecated and will not work if security is enabled.
* Key Filters are deprecated; we strongly discourage key listing in production due to the overhead involved, so it's better to maintain key indexes as values in Riak (see our [set data type]({{<baseurl>}}riak/kv/2.2.2/developing/data-types/sets/) as a useful tool for such indexes).
* JavaScript MapReduce is deprecated; we have expanded our [Erlang MapReduce]({{<baseurl>}}riak/kv/2.2.2/developing/app-guide/advanced-mapreduce/#mapreduce) documentation to assist with the transition.
* Riak search 1.0 is deprecated in favor of our Solr-based [Riak search 2.0]({{<baseurl>}}riak/kv/2.2.2/developing/usage/search/). Version 1.0 will not work if security is enabled.
* v2 replication (a component of Riak KV Enterprise) is superseded by v3 and will be removed in the future.
* Legacy vnode routing (an early mechanism for managing requests between servers) is deprecated. If `vnode_routing` is set to `legacy` via Riak KV's capability system, it should be removed to prevent upgrade problems in the future.
* Some users in the past have used Riak's internal API (e.g. `riak:local_client/1`); this API may change at any time, so we strongly recommend using our [Erlang client library](http://github.com/basho/riak-erlang-client/) (or [one of the other libraries]({{<baseurl>}}riak/kv/2.2.2/developing/client-libraries/) we support) instead.