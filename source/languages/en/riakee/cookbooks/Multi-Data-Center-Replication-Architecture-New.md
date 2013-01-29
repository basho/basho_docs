---
title: "Multi Data Center Replication: Architecture"
project: riakee
version: 1.3.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, bnw]
---

## How Riak EE 1.3 Replication Works

In multi-datacenter replication, one cluster acts as the "source cluster". The source cluster sends replication data to one or more "sink clusters" (generally located in datacenters in other regions or countries). If the datacenter with the source cluster goes down, a sink cluster can take over as the primary cluster. In this sense, Riak's multi-datacenter capabilities are "masterless." 

In multi-datacenter replication, there are two primary modes of operation: full-sync and real-time. In full-sync mode, a complete synchronization occurs between source and sink cluster(s). In real-time mode, continual, incremental synchronization occurs - replication is triggered by successful writing of new updates on the source. Full-sync can be performed upon initial connection of a sink cluster. 

Full-sync and real-time modes are described in detail below. 

## Concepts


### Sources

A source refers to a cluster that is the primary producer of replication data. A source can also refer to any node that is part of the source cluster. Source clusters push data to sink clusters. 

### Sinks

A sink refers to a cluster that is the primary consumer of replication data. A sink can also refer to any node that is part of the sink cluster. Sink clusters receive data from source clusters.

### Cluster Manager

The cluster manager is a Riak EE service that provides information regarding nodes and protocols supported by the sink and source clusters. This information is primarily consumed by the `riak-repl connect` command. 

### Fullsync Coordinator

In fullsync replication, a node on the source cluster is elected to be the *fullsync coordinator*. This node is responsible for starting and stopping replication to the sink cluster. It will also communicates with the sink cluster to exchange key lists, and ultimately transfer data across a TCP connection. If a fullsync coordinator is terminated as the result of an error, it will automatically restart on the current node. In the node becomes unresponsive, then a leader election will take place within 5 seconds to select a new node from the cluster to become the coordinator.


## Full-Sync Replication

Fullsync replication scans through the list of partitions in a Riak cluster, and determines which objects in the sink cluster need to be updated. A source partition is synchronized to a node on the sink cluster containing the current partition.
 
<br>
![MDC Full-Sync](/images/MDC_BNW_Full-sync-small.png)
<br>

## Real-time Replication

In real-time replication, a node in the source cluster will forward data to the sink cluster. A node in the source cluster does not necessarily connect to a node containing the same *vnode* on the sink cluster. This allows Riak to spread out realtime replication across the entire cluster, thus improving throughput and and making replication more fault-tolerant.

In the diagram below, the following steps occur:

* 1) A TCP connection is opened by the Riak connection manager between the source and sink clusters.

* 2) The client sends an object to store on the source cluster.

* 3) Riak writes N replicas on the source cluster.

* 4) The object is stored in the real-time queue and copied to the sink cluster.

* 5) The sink cluster recieves the object via the real-time connection and writes the object to N nodes.


<br>
![MDC Full-Sync](/images/MDC_BNW-real-time-sync-small.png)
<br>

