---
title: "Multi Data Center Replication: Concepts"
project: riakee
version: 1.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl]
---

## How Replication Works
Riak replication copies all data from a primary cluster to a secondary cluster. Currently it is unidirectional. You can configure a pair of connections to sync bidirectionally between two clusters. All data is synchronized on initial connection (configurable), followed by streamed updates to the secondary cluster and periodic full syncs.

## Concepts
### Listeners
Replication **listener** nodes, also called **servers**, are Riak cluster nodes that will listen on an external IP address and port for the purpose of handling Replication requests. Any node in a Riak cluster can participate as a listener - adding more nodes will increase the fault-tolerance of the replication process in the presence of individual node failures. Listeners are started on the primary cluster.

### Sites
Replication **site** nodes, also called **clients**, are Riak cluster nodes that connect to listeners and initiate Replication requests. Site nodes must be given a listener node to connect to when started.

Sites are started on the secondary cluster.

### Leadership
Riak replication uses a leadership-election protocol to determine which node in the cluster will participate in replication. Only one node in each cluster will be responsible for serving as the client or server to any other clusters.

If a client/site connects to a listener that is not the leader, it will be redirected to the listener node that is currently the leader.

### Full-sync
On initial connection, the primary cluster (listener/server) will initiate a full synchronization with the secondary cluster (site/client), which computes hashes of all keys stored in each partition of the primary cluster and sends them to the secondary cluster. The secondary cluster then calculates its own hashes and requests updates for keys that are missing or stale.

New writes on the primary cluster will be streamed to the secondary cluster between synchronization of individual partitions.

Full-syncs are also performed on a periodic basis, configurable in the riak_repl section of app.config, under the fullsync_interval parameter. See the [[Multi Data Center Replication Configuration]] document for more details.
