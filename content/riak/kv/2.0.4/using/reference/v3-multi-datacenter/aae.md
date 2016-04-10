---
title: "V3 Multi-Datacenter Replication Reference: With Active Anti-Entropy"
description: ""
project: "riak_kv"
project_version: "2.0.4"
menu:
  riak_kv-2.0.4:
    name: "Active Anti-Entropy"
    identifier: "managing_ref_v3_aae"
    weight: 101
    parent: "managing_ref_v3"
toc: true
aliases:
  - /riak/2.1.3/ops/mdc/v3/aae
canonical_link: "docs.basho.com/riak/kv/latest/using/reference/v3-multi-datacenter/aae.md"
---

[glossary aae]: /riak/kv/2.0.4/learn/glossary/#Active-Anti-Entropy-AAE-
[config reference#advanced]: /riak/kv/2.0.4/configuring/reference/#Advanced-Configuration
[concept clusters]: /riak/kv/2.0.4/learn/concepts/clusters

> **Note: Technical preview**
>
> The active anti-entropy fullsync strategy, as it pertains to
replication, is currently in **technical preview** mode. This means that
it hasn't been tested at large scale and that there may be issues that
Basho must address prior to a general release. Please don't use this
feature on a production system without professional services or customer
service engineering support.

## Overview

Riak Enterprise Multi-Datacenter (MDC) Replication version 3 (Riak
Enterprise version 1.4.0+) can now take advantage of Riak's [active anti-entropy][glossary aae] \(AAE) subsystem, which was first introduced as a
technology preview in Riak 1.3.0.

AAE plus Replication uses existing Riak AAE hash trees stored in
LevelDB, so if AAE is already active, there is no additional startup
delay for enabling the `aae` fullsync strategy. AAE can also be enabled
for the first time on a cluster, although some custom settings can
enhance performance in this case to help AAE trees be built more
quickly. See [Configuration/AAE Tree Build Optimization](#aae-tree-build-optimization).

## Requirements:

* Riak Enterprise version 1.4.0 or later installed on source and sink
  clusters
* Riak Enterprise MDC Replication Version 3 enabled on source and sink
  clusters
* Both source and sink clusters must be of the same ring size
* AAE must be enabled on both source and sink clusters
* `fullsync_strategy` in the `riak_repl` section of the
  `advanced.config` configuration file must be set to `aae` on both
  source and sink clusters
* AAE trees must have been built on both source and sink clusters. In
  the event that an AAE tree is not built on both the source and sink,
  fullsync will default to the `keylisting` fullsync strategy for that
  partition.

## Configuration

If you are using Riak Enterprise version 2.0, configuration is managed
using the `advanced.config` files on
each node. The semantics of the `advanced.config` file are similar to
the formerly used `app.config` file. For more information and for a list
of configurable parameters, see our documentation on [Advanced Configuration][config reference#advanced].

## Enable Active Anti-Entropy

To enable [active anti-entropy][glossary aae] \(AAE) in Riak Enterprise, you must enable it in Riak in both source and sink clusters. If it is not
enabled, the `keylist` strategy will be used.

To enable AAERiak KV:

```advancedconfig
{riak_kv, [
    % ...
    {anti_entropy, {on, []}},
    % ...
    ]}
```

By default, it could take a couple of days for the cluster to build all
of the necessary hash trees because the default **build rate** of trees
is to build 1 partition per hour, per node. With a
[ring size][concept clusters] of 256 and 5 nodes, that is 2 days.

Changing the rate of tree building can speed up this process, with the
caveat that rebuilding a tree takes processing time from the cluster,
and this should not be done without assessing the possible impact on
get/put latencies for normal cluster operations. For a production
cluster, we recommend leaving the default in place.

For a test cluster, the build rate can be changed with
`anti_entropy_build_limit` and `anti_entropy_concurrency`. If a
partition has not had its AAE tree built yet, it will default to using
the `keylist` replication strategy. Instructions on these settings can
be found in the section directly below.

<div id="aae-tree-build-optimization"></div>

### AAE Tree Build Optimization

You can speed up the build rate for AAE-related hash trees by raising
the frequency dictated by the `anti_entropy_build_limit` setting.

```advancedconfig
{riak_kv, [
    % ...
    {anti_entropy_build_limit, {10, 3600000}}, %% up to 10 per hour
    {anti_entropy_concurrency, 10}             %% up to 10 concurrent builds
    % ...
    ]}
```

### Enable AAE Fullsync Replication Strategy

Finally, the replication fullsync strategy must be set to use `aae` on
both source and sink clusters. If not, the `keylist` replication
strategy will be used.

To enable AAE w/ Version 3 MDC Replication:

```advancedconfig
{riak_repl, [
             % ...
             {fullsync_strategy, aae},
             % ...
            ]}
```
