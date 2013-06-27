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

* listeners, sites
* single connection via leader (bottleneck)
* sites connect back to sink
* queue tied to a specific TCP/IP connection


### Version 3

* Connections between named clusters
* There may be up to M connections from a source cluster with N nodes to a sink cluster with M nodes. Connections aren't tied to a leader node as they are w/ V2 replication. 
* communications for realtime, fullsync and proxy get are multiplexed over the same connection. This helps w/ firewall configuration.
* A fullsync coordinator runs on a leader of the source cluster. (TODO: add more here)

* Realtime replication uses a bounded queue per node/per sink. This queue requires consumers to acknowledge objects when they have been replicated. Dropped TCP connections don't drop objects from the queue. 
* Network statistics are kept per socket
* Fullsyncs between clusters can be tuned to control the maximum number of workers that will run on a source node, a sink node, and across the entire source cluster. This allows for dialing in fullsync performance.
* Version 3 is able to take advantage of AAE technology, which can greatly improve fullsync performance.