---
title: "Multi Data Center Replication v3 With AAE"
project: riakee
version: 1.4.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, aae, entropy, fullsync]
---

<div class="info">
Replication w/ Active Anti-Entropy (AAE) technology is currently a *Technology Preview*. This means that it hasn't been tested at large scale and there may be issues that need to be addressed by Basho before a general release. Please *do not* use on a production system without professional services or customer service engineering support.
</div>

## Overview

Riak Enterprise MDC Replication Version 3 (Riak EE version 1.4.0+) can now take advantage of Riak Active Anti-Entropy (AAE), which was first introduced as a technology preview in Riak 1.3.0. The use of AAE in replication can greatly increase the performance of a fullsync. Comparison time between two clusters can become linear with the percentage of differences.

AAE + Replication uses existing Riak AAE hash trees stored in LevelDB, so if AAE is already active, there is no additional startup delay for enabling the ```aae``` fullsync strategy. AAE can also be enabled for the first time on a cluster; although some custom settings can increase performance in this case to help AAE trees get built faster. See [Configuration/AAE Tree Build Optimization](#aae-tree-build-optimization).

## Requirements:

* Riak Enterprise version 1.4.0 or later installed on source and sink clusters.
* Riak Enterprise MDC Replication Version 3 enabled on source and sink clusters.
* Both source and sink clusters must be of the *same ring size*. If a different ring size is detected, version 3 replication will fallback to a keylist comparison strategy (which can be much slower than an AAE exchange).
* AAE must be enabled on both source and sink clusters
* fullsync_strategy keylist set to `aae` on both source and sink clusters
* AAE trees must have been built on both source and sink clusters


## Configuration

### Enable Active Anti-Entropy
To enable this functionality, AAE in Riak's key/value store must first be enabled on both source and sink clusters. If not, the ```keylist``` strategy will be used.

To enable AAE in Riak KV:

    {riak_kv,
            [ {anti_entropy, {on, []}},
       ... ]}

By default it could take a couple of days for the cluster to build all of the necessary hash trees because the default "build rate" of trees is to build 1 partition per hour per node. With a ring size of 256 and 5 nodes, that is 2 days. This process can be sped up greatly by changing the rate of tree building, with the caveat that re-building a tree takes processing time from the cluster and this should not be done without assessing the possible impact on latencies on get/put times for normal cluster operations. For a production cluster, it is recommended to leave the default in place. For a test cluster, the build rate can be changed with ```anti_entropy_build_limit``` and ```anti_entropy_concurrency```. If a partition has not had its AAE tree built yet, it will default to using ```keylist``` replication strategy.

#### AAE Tree Build Optimization
To speed up AAE tree build rate on test clusters (Riak version 1.3.0+):

	{riak_kv,
            [ {anti_entropy, {on, []}},
		      {anti_entropy_build_limit, {10, 3600000}}, %% up to 10 per hour
              {anti_entropy_concurrency, 10}             %% up to 10 concurrent builds
        ... ]}

### Enable AAE Fullsync Replication Strategy
Finally, the replication fullsync strategy must be set to use ```aae``` on both source and sink clusters. If not, the ```keylist``` replication strategy will be used.

To enable AAE w/ Version 3 MDC Replication:

    {riak_repl,
              [ {fullsync_strategy, aae},
          ... ]}
