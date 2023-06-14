---
title: "Riak KV 2.0.5 Release Notes"
description: ""
project: "riak_kv"
project_version: "2.0.5"
menu:
  riak_kv-2.0.5:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: true
aliases:
  - /riak/2.0.5/community/release-notes
---

## Changes

* RIAK-1431 - Adding --format=csv to allow some administrative commands to be output as csv files
  * [clique/pull/42](https://github.com/basho/clique/pull/42)
  * [clique/pull/45](https://github.com/basho/clique/pull/45)

* Riak-1477 - Sequential tuning
  Improving leveldb for when a single database (vnode) receives a large write volume. This situation commonly occurs during a Riak handoff operation.
  * [leveldb/pull/146](https://github.com/basho/leveldb/pull/146)

* Riak-1493 Add optional timeout for handle_handoff_data callback and improve logging
  * [riak_core/pull/642](https://github.com/basho/riak_core/pull/642)

## Fixes

* RIAK-1127 - L0 Realtime connection balancing. N v3 repl realtime connections are chosen randomly which can lead to large imbalances. Previous implementations did a better job of ensuring there was good balance. The goal of this work is to bring that back to v3 realtime replication.
  * [riak/issues/625](https://github.com/basho/riak/issues/625)

* Riak-1455 - If Solr complains that the index exists, update Riak's metadata to reflect it
  * [yokozuna/pull/444](https://github.com/basho/yokozuna/pull/444)

* Riak-1479 - node_get_fsm_time and node_put_fsm_time stats are incorrect. node_get_fsm_time and node_put_fsm_time values are an order of magnitude smaller than 2.0.2
  * [riak_kv/pull/1075](https://github.com/basho/riak_kv/pull/1075)

* Add a log message for when a pipe fitting fails during a map reduce job
  * [riak_kv/pull/1073](https://github.com/basho/riak_kv/pull/1073)
  * [riak_pipe/pull/92](https://github.com/basho/riak_pipe/pull/92)

* Fix erroneous tag syntax in rebar.conf for erlydtl depedency of riak_control
  * [riak_control/pull/183](https://github.com/basho/riak_control/pull/183)

* Fixed parsing of global flags by delegating parsing to clique instead of riak-admin script
  * [riak/pull/665](https://github.com/basho/riak/pull/665)

* Reduce overload of sink nodes by balancing inbound connections across all nodes, including nodes not up due to network partition.
  * [riak_repl/pull/651](https://github.com/basho/riak_repl/pull/651)

## RIAK-1482 - Maps and sets incompatibility in 2.0.4

There was an internal format incompatibility introduced in Riak 2.0.4 such that Map and Set datatypes were unreadable if upgraded from 2.0.2 or earlier. Similarly, datatypes modified on a 2.0.4 node would be unreadable by a 2.0.2 or earlier nodes. Clusters in a rolling upgrade state and MDC setups where the source and sink clusters had different versions would also be affected.

Riak 2.0.5 introduces a negotiated capability and manual feature switch so that clusters with mixed versions and MDC setups can continue to operate with different internal representations. When all nodes are upgraded to 2.0.5, the operator can change the feature switch to enable the new format. The feature switch is only necessary in MDC setup; non-replicated clusters can simply do a rolling upgrade.

New 2.0.5 clusters and MDC setups will operate unaffected by this problem using the new internal format.

Instructions for upgrade of MDC setups:

1. On all 2.0.2 or earlier nodes, add the following to the advanced.config file (according to its syntax rules):
`[{riak_kv, [{mdc_crdt_epoch, 1}]}].`

2. All nodes can remain up while this change is made. It does not need to take effect until the node is restarted under 2.0.5.
Upgrade to 2.0.5 in a rolling fashion as recommended by our documentation.
3. When all nodes in all datacenters are at version 2.0.5 or later, remove the configuration from advanced.config and execute this snippet on a single node via riak attach:
`riak> rpc:multicall(application, set_env, [riak_kv, mdc_crdt_epoch, 2]).`

  * [riak_dt/pull/111](https://github.com/basho/riak_dt/pull/111)
  * [riak_test/pull/728](https://github.com/basho/riak_test/pull/728)
  * [riak_kv/pull/1076](https://github.com/basho/riak_kv/pull/1076)

## Known Issues

(Updated 02/27/2015) Yokozuna’s Active Anti-Entropy encounters `badarg` errors when decoding entropy data containing Riak keys that include spaces, documented in Github issues [450](https://github.com/basho/yokozuna/issues/450) and [436](https://github.com/basho/yokozuna/issues/436). The workaround, currently, is to avoid using keys with spaces if you’re using Yokozuna. This issue has been confirmed, and we are working on a patch for affected users.

## Notes on upgrading

Although undocumented, versions of Riak prior to 2.0 did not prevent the use of the Erlang VM's -sname configuration parameter. As of 2.0 this is no longer permitted. Permitted in 2.0 are nodename in riak.conf and -name in vm.args. If you are upgrading from a previous version of Riak to 2.0 and are using -sname in your vm.args, the below steps are required to migrate away from -sname.

1. Upgrade to Riak 1.4.12.
2. Back up the ring directory on each node, typically located in /var/lib/riak/ring.
3. Stop all nodes in your cluster.
4. Run `riak-admin reip <old_nodename> <new_nodename>|riak-admin Command Line#reip` on each node in your cluster, for each node in your cluster. For example, in a 5 node cluster this will be run 25 total times, 5 times on each node. The <old_nodename> is the current shortname, and the <new_nodename> is the new fully qualified hostname.
5. Change riak.conf or vm.args, depending on which configuration system you're using, to use the new fully qualified hostname on each node.
Start each node in your cluster.

## Merged Pull Requests

* riak_kv/1071: [Reflect error-handling changes in cuttlefish](https://github.com/basho/riak_kv/pull/1071)
* riak_kv/1075: [The aliases for [riak_kv,vnode,gets|puts,...] were wrong](https://github.com/basho/riak_kv/pull/1075)
* riak_kv/1076: [Add capability and env_var to control binary format of map/set (riak#667)](https://github.com/basho/riak_kv/pull/1076)
* eleveldb/132: [Move spec to correct line so we can gen docs](https://github.com/basho/eleveldb/pull/132)
* riak_core/642: [Add optional timeout for handle_handoff_data callback and improve logging [JIRA: RIAK-1493]](https://github.com/basho/riak_core/pull/642)
* riak_core/696: [Bump clique to 0.2.5](https://github.com/basho/riak_core/pull/696)
* yokozuna/438: [Compile bb before misc/bench compile](https://github.com/basho/yokozuna/pull/438)
* yokozuna/444: [Deal with a Solr Core / Riak Index metadata mismatch](https://github.com/basho/yokozuna/pull/444)
* clique/37: [Add config formatter support to clique](https://github.com/basho/clique/pull/37)
* clique/39: [Track pending changes to cuttlefish error handling](https://github.com/basho/clique/pull/39)
* clique/40: [Allow registration of functions as usage](https://github.com/basho/clique/pull/40)
* clique/42: [Add CSV formatter and support for other custom formats in general](https://github.com/basho/clique/pull/42)
* clique/45: [Introduce spec record for args and flags, datatypes, and validators.](https://github.com/basho/clique/pull/45)
* clique/46: [Print alerts to stderr when using the CSV writer](https://github.com/basho/clique/pull/46)
* clique/50: [Fix regression that broke --help and --format](https://github.com/basho/clique/pull/50)
* cuttlefish/180: [Rework error handling to turn errors into smarter nested tuples](https://github.com/basho/cuttlefish/pull/180)
* riak_dt/111: [Make map/set interoperable with previous versions ](https://github.com/basho/riak_dt/pull/111)
* riak_repl/653: [Cleanup riak_repl tests to prevent spurious errors](https://github.com/basho/riak_repl/pull/653)
