---
title: "Multi-Datacenter Replication Reference: Comparsion"
description: ""
project: "riak_kv"
project_version: 2.9.7
menu:
  riak_kv-2.9.7:
    name: "Comparison"
    identifier: "managing_ref_mdc_comparison"
    weight: 103
    parent: "managing_ref_mdc"
toc: true
aliases:
  - /riak/2.9.7/ops/mdc/comparison
  - /riak/kv/2.9.7/ops/mdc/comparison
---

This document is a systematic comparison of [Version 2]({{<baseurl>}}riak/kv/2.9.7/using/reference/v2-multi-datacenter) and [Version 3]({{<baseurl>}}riak/kv/2.9.7/using/reference/v3-multi-datacenter) of Riak's Multi-Datacenter
Replication capabilities.

{{% note title="Important note on mixing versions" %}}
If you are installing Riak anew, you should use version 3
replication. Under no circumstances should you mix version 2 and version 3
replication. This comparison is meant only to list improvements introduced in
version 3.
{{% /note %}}

## Version 2

* Version 2 replication relies upon the twin concepts of **listeners**
  and **sites**. Listeners are the sources of replication data, while
  sites are the destination of replication data. Sites and listeners are
  manually configured on each node in a cluster. This can be a burden to
  the administrator as clusters become larger.
* A single connection tied to the **cluster leader** manages all
  replication communications. This can cause performance problems on the
  leader and is a bottleneck for realtime and fullsync replication data.
* Connections are established from site to listener. This can be
  confusing for firewall administrators.
* The realtime replication queue will be lost if the replication
  connection breaks, even if it's re-established. Reconciling data in
  this situation would require manual intervention using either of the
  following:
  * a fullsync
  * another Riak write to the key/value on the listener, thus
      re-queueing the object
* Riak CS MDC `proxy_get` connections can only request data from a
  single leader node

### When to use version 2 replication

* If you are running clusters below version 1.3.0 of Riak Enterprise,
  version 2 replication is the only method of replication available.
* In the Riak 1.3 series, version 3 replication was provided as a
  technology preview and did not have feature parity with version 2.
  This was provided in the Riak 1.4 series.

## Version 3

* Version 3 replication uses the twin concepts of **sources** and
  **sinks**. A source is considered the primary provider of replication
  data, whereas a sink is the destination of replication data.
* Establishing replication connections between clusters has been
  greatly simplified. A single `riak-repl connect` command needs to be
  issued from a source cluster to a sink cluster. IP and port
  information of all nodes that can participate in replication on both
  source and sink clusters are exchanged by the **replication cluster
  manager**. The replication cluster manager also tracks nodes joining
  and leaving the cluster dynamically.
* If the source has M nodes, and the sink has N nodes, there will be M
  realtime connections. Connections aren't tied to a leader node as they
  are with version 2 replication.
* Communications for realtime, fullsync, and `proxy_get` operations are
  multiplexed over the same connection for each node participating in
  replication. This reduces the amount of firewall configuration on both
  sources and sinks.
* A fullsync coordinator runs on a leader of the source cluster. The
  coordinator assigns work across nodes in the sources cluster in an
  optimized fashion.
* Realtime replication establishes a bounded queue on each source node
  that is shared between *all* sinks. This queue requires consumers to
  acknowledge objects when they have been replicated. Dropped TCP
  connections won't drop objects from the queue.
* If a node in the source cluster is shut down via the command line, a
  realtime replication queue is migrated to other running nodes in the
  source cluster.
* Network statistics are kept per socket.
* Fullsyncs between clusters can be tuned to control the maximum number
  of workers that will run on a source node, a sink node, and across the
  entire source cluster. This allows for limiting impact on the cluster
  and dialing in fullsync performance.
* Version 3 is able to take advantage of [Active Anti-Entropy]({{<baseurl>}}riak/kv/2.9.7/learn/concepts/active-anti-entropy/) \(AAE)
  technology, which can greatly improve fullsync performance.
* Riak CS MDC `proxy_get` connections will be distributed across the
  source cluster (as CS blocks are requested from the sink cluster in
  this scenario).

