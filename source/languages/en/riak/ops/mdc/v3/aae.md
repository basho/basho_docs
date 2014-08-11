---
title: "Multi Data Center Replication v3 With AAE"
project: riak
header: riakee
version: 1.4.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, aae, entropy, fullsync]
moved: {
    '2.0.0-': 'riakee:/cookbooks/Multi-Data-Center-Replication-v3-With-AAE'
}
---

<div class="info">
The Active Anti-Entropy fullsync strategy, as it pertains to replication, is currently in <em>Technology Preview</em>. This means that it hasn't been tested at large scale and that there may be issues Basho must address prior to a general release. Please don't use on a production system without professional services or customer service engineering support.
</div>

## Overview

Riak Enterprise Multi-Datacenter (MDC) Replication Version 3 (Riak Enterprise version 1.4.0+) can now take advantage of Riak Active Anti-Entropy (AAE), which was first introduced as a technology preview in Riak 1.3.0. 

AAE + Replication uses existing Riak AAE hash trees stored in LevelDB, so if AAE is already active, there is no additional startup delay for enabling the `aae` fullsync strategy. AAE can also be enabled for the first time on a cluster, although some custom settings can enhance performance in this case to help AAE trees be built more quickly. See [[Configuration/AAE Tree Build Optimization|Multi Data Center Replication v3 With AAE#aae-tree-build-optimization]].

## Requirements:

* Riak Enterprise version 1.4.0 or later installed on source and sink clusters.
* Riak Enterprise MDC Replication Version 3 enabled on source and sink clusters.
* Both source and sink clusters must be of the *same ring size*.
* AAE must be enabled on both source and sink clusters.
* `fullsync_strategy` in the `riak_repl` section of the app.config must be set to `aae` on both source and sink clusters.
* AAE trees must have been built on both source and sink clusters.  In the event an AAE tree is not built on both the source and sink, fullsync will default to the `keylisting` fullsync strategy for that partition.


## Configuration

### Enable Active Anti-Entropy
To enable this functionality, AAE in Riak's key/value store must first be enabled on both source and sink clusters. If not, the `keylist` strategy will be used.

To enable AAE in Riak KV:

```erlang
{riak_kv,
        [ {anti_entropy, {on, []}},
   ... ]}
```

By default, it could take a couple of days for the cluster to build all of the necessary hash trees because the default *build rate* of trees is to build 1 partition per hour, per node. With a ring size of 256 and 5 nodes, that is 2 days.

Changing the rate of tree building can speed up this process, with the caveat that rebuilding a tree takes processing time from the cluster, and this should not be done without assessing the possible impact on get/put latencies for normal cluster operations. For a production cluster, it is recommended that you leave the default in place.

For a test cluster, the build rate can be changed with `anti_entropy_build_limit` and `anti_entropy_concurrency`. If a partition has not had its AAE tree built yet, it will default to using `keylist` replication strategy.

<div id="aae-tree-build-optimization"></div>
#### AAE Tree Build Optimization

To speed up AAE tree build rate on test clusters:

```erlang
{riak_kv,
        [ {anti_entropy, {on, []}},
          {anti_entropy_build_limit, {10, 3600000}}, %% up to 10 per hour
          {anti_entropy_concurrency, 10}             %% up to 10 concurrent builds
    ... ]}
```

### Enable AAE Fullsync Replication Strategy

Finally, the replication fullsync strategy must be set to use `aae` on both source and sink clusters. If not, the `keylist` replication strategy will be used.

To enable AAE w/ Version 3 MDC Replication:

```erlang
{riak_repl,
          [ {fullsync_strategy, aae},
      ... ]}
```
