---
title: "Multi Data Center Replication v3 With AAE"
project: riakee
version: 1.4.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, aae, entropy, fullsync]
---

## Status

**Replication w/ Active Anti-Entropy (AAE) technology is currently a *Technology Preview*. This means that it hasn't been tested at large scale and there may be issues that need to be addressed by Basho before a general release. Please *do not* use on a production system.**

### Overview

Riak Enterprise MDC Replication Version 3 can now take advantage of Riak Active Anti-Entropy (AAE) technology that was first released as part of Riak 1.3.0. The use of AAE in replication can greatly increase the performance of a fullsync. Comparison time between two clusters can become linear with the percentage of differences.

AAE + Replication uses existing Riak AAE LevelDB files.

### Requirements:

* Riak Enterprise MDC Replication Version 3 enabled on source and sink clusters.

* Both source and sink clusters must be of the *same ring size*. If a different ring size is detected, version 3 replication will fallback to a keylist comparison strategy (which can be much slower than an AAE exchange).

* AAE must be enabled on both source and sink clusters

* fullsync_strategy keylist set to `aae` on BOTH SIDES


### Configuration

To enable this functionality, AAE in Riak's key/value store must be enabled. By default it will take several days for the cluster to build all of the necessary hash trees, so do this well in advance of enabling AAE in replication.

To enable AAE in Riak KV:

        {riak_kv,
                [ {anti_entropy, {on, []}},
            ... ]}


To enable AAE w/ Version 3 MDC Replication:

        {riak_repl,
                [ {fullsync_strategy, aae},
                ... ]}
