---
title: "Monitoring Strong Consistency"
description: ""
project: "riak_kv"
project_version: "2.2.3"
menu:
  riak_kv-2.2.3:
    name: "Monitoring Strong Consistency"
    identifier: "cluster_operations_strong_consistency"
    weight: 110
    parent: "managing_cluster_operations"
toc: true
---

{{% note title="Please Note:" %}}
Riak KV's strong consistency is an experimental feature and may be removed
from the product in the future. Strong consistency is not commercially
supported or production-ready. Strong consistency is incompatible with
Multi-Datacenter Replication, Riak Search, Bitcask Expiration, LevelDB
Secondary Indexes, Riak Data Types and Commit Hooks. We do not recommend its
usage in any production environment.
{{% /note %}}

## Monitoring Strong Consistency

Riak provides a wide variety of data related to the current operating
status of a node. This data is available by running the [`riak-admin status`]({{<baseurl>}}riak/kv/2.2.3/using/admin/riak-admin/#status) command. That data now
includes statistics specific to strongly consistent operations.

A full listing of these stats is available in [Inspecting a Node]({{<baseurl>}}riak/kv/2.2.3/using/cluster-operations/inspecting-node).
All strong consistency-related stats are prefixed with `consistent_`,
e.g. `consistent_gets`, `consistent_puts`, etc. Many of these stats are
so-called "one-minute stats," meaning that they reflect node activity in
the last minute.

Strong consistency stats fall into two categories: GET-related and
PUT-related stats.

### GET-related stats

Stat | Description
:----|:-----------
`consistent_gets` | Number of strongly consistent GETs coordinated by this node in the last minute
`consistent_gets_total` | Total number of strongly consistent GETs coordinated by this node
`consistent_get_objsize_mean` | Mean object size for strongly consistent GETs on this node in the last minute
`consistent_get_objsize_median` | Median object size for strongly consistent GETs on this node in the last minute
`consistent_get_objsize_95` | 95th-percentile object size for strongly consistent GETs on this node in the last minute
`consistent_get_objsize_99` | 99th-percentile object size for strongly consistent GETs on this node in the last minute
`consistent_get_objsize_100` | 100th-percentile object size for strongly consistent GETs on this node in the last minute
`consistent_get_time_mean` | Mean time between reception of client GETs to strongly consistent keys and subsequent response
`consistent_get_time_median` | Median time between reception of client GETs to strongly consistent keys and subsequent response
`consistent_get_time_95` | 95th-percentile time between reception of client GETs to strongly consistent keys and subsequent response
`consistent_get_time_99` | 99th-percentile time between reception of client GETs to strongly consistent keys and subsequent response
`consistent_get_time_100` | 100th-percentile time between reception of client GETs to strongly consistent keys and subsequent response

### PUT-related stats

Stat | Description
:----|:-----------
`consistent_puts` | Number of strongly consistent PUTs coordinated by this node in the last minute
`consistent_puts_total` | Total number of strongly consistent PUTs coordinated by this node
`consistent_put_objsize_mean` | Mean object size for strongly consistent PUTs on this node in the last minute
`consistent_put_objsize_median` | Median object size for strongly consistent PUTs on this node in the last minute
`consistent_put_objsize_95` | 95th-percentile object size for strongly consistent PUTs on this node in the last minute
`consistent_put_objsize_99` | 99th-percentile object size for strongly consistent PUTs on this node in the last minute
`consistent_put_objsize_100` | 100th-percentile object size for strongly consistent PUTs on this node in the last minute
`consistent_put_time_mean` | Mean time between reception of client PUTs to strongly consistent keys and subsequent response
`consistent_put_time_median` | Median time between reception of client PUTs to strongly consistent keys and subsequent response
`consistent_put_time_95` | 95th-percentile time between reception of client PUTs to strongly consistent keys and subsequent response
`consistent_put_time_99` | 99th-percentile time between reception of client PUTs to strongly consistent keys and subsequent response
`consistent_put_time_100` | 100th-percentile time between reception of client PUTs to strongly consistent keys and subsequent response
