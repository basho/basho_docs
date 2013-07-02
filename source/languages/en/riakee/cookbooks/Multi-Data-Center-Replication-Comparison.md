---
title: "Multi Data Center Replication: Comparison"
project: riakee
version: 1.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl]
---

### Version 2

* Version 2 replication uses *listeners* and *sites* concepts. Listeners are the sources of replication data, while sites are the destination of replication data. These are manually configured on each node in a cluster. This can be a burden to the administrator as clusters become larger. 
* A single connection tied to the *cluster leader* manages all replication communications. This can cause performance problems on the leader, and is a bottleneck for realtime and fullsync replication data.
* Connections are established from *site* to *listener*. This can be confusing for firewall administrators.
* The realtime replication queue will be lost if the replication connection breaks (even if it's reestablished). Reconciling data in this situation would require manual intervention using either:
	* a fullsync
	* another Riak write to the key/value on the listener, thus requeueing the object


#### When to use version 2 replication

If you are running clusters below version 1.3.0 of Riak Enterprise, then version 2 replication is the only method of replication available.

### Version 3
* Version 3 replication uses *source* and *sink* concepts. A source is considered the primary provider of replication data, where a sink is the destination of replication data.
* Establishing replication connections between clusters has been greatly simplified. A single `riak-repl connect` command needs to be issued from a source cluster to a sink cluster. IP and port information of all nodes that can partipate in replication on both source and sink clusters are exchanged by the *replication cluster manager*. The replication cluster manager also tracks nodes joining and leaving the cluster dynamically.
* There may be up to **M** connections from a source cluster with **N** nodes to a sink cluster with **M** nodes. Connections aren't tied to a leader node as they are with version 2 replication. 
* Communications for realtime, fullsync and proxy_get are multiplexed over the same connection for each node participating in replication. This reduces the amount of firewall configuration on sources and sinks.
* A fullsync coordinator runs on a leader of the source cluster. The coordinator optimally assigns work across nodes in the sources cluster. 
* Realtime replication establishes a bounded queue on each source node for ever sink. This queue requires consumers to acknowledge objects when they have been replicated. Dropped TCP connections won't drop objects from the queue. 
* If a node in the source cluster is shutdown via the command line, a realtime replication queue is migrated to other running nodes in the source cluster.
* Network statistics are kept per socket.
* Fullsyncs between clusters can be tuned to control the maximum number of workers that will run on a source node, a sink node, and across the entire source cluster. This allows for dialing in fullsync performance.
* Version 3 is able to take advantage of AAE technology, which can greatly improve fullsync performance.
