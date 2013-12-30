---
title: "Multi Data Center Replication v3 Quick Start"
project: riakee
version: 1.3.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, bnw]
---

The Riak Multi-Datacenter Replication Quick Start will walk you through the process of configuring Riak's v3 Replication to perform replication between two sample Riak clusters on separate networks. This guide will also cover bidirectional replication, which is accomplished by setting up unidirectional replication in both directions between the clusters.

### Prerequisites
This Guide assumes that you have completed the following steps:

* Install [[Riak Enterprise]]
* Perform [[System Turning|Linux Performance Tuning]]
* Review [[Configuration|Multi Data Center Replication v3 Configuration]]

### About v3 Replication in 1.3
In Riak's v3 Replication in 1.3, the nomenclature for Source and Site clusters has changed. To more accurately reflect the behavior of each of the clusters, "listeners" and "sites" are now known as "sources" and "sinks." Data transfer now originates at the "source" and replicates to the "sink;" initiation is always from the primary (source) to the backup (sink) data center.

Additionally, knowledge of the state of each cluster is now managed by a "cluster manager" process, which greatly simplifies the setup and maintenance of MDC replication.

### Scenario

Configure Riak MDC to perform replication, given the following two (2) three-node Riak Enterprise Clusters: 

#### Cluster 1 
name  | ip          | nodename
------|--------------|-----------------
`node1` | `10.60.67.149` | `riak@10.60.67.149`
`node2` | `10.60.83.39`  | `riak@10.60.83.39`
`node3` | `10.60.90.252` | `riak@10.60.90.252`

#### Cluster 2
name  | ip          | nodename
------|-------------|-----------------
`node4` | `10.60.77.10` | `riak@10.60.77.10`
`node5` | `10.60.84.41` | `riak@10.60.84.41`
`node6` | `10.60.92.44` | `riak@10.60.92.44`


### Set up Cluster1 &rarr; Cluster2 Connection

#### Set up the Source on Cluster1

On a node in Cluster1, `node1` for example, initiate and name this cluster with `riak-repl clustername <name>`:

```bash
$ riak-repl clustername Cluster1
```
	
#### Setup the Sink on Cluster2

On a node in Cluster2, `node4` for example, initiation and name this cluster with `riak-repl clustername <name>`:

```bash
$ riak-repl clustername Cluster2
```
	
#### Connect the Source to the Sink

From Cluster1, connect to the IP and port of Cluster2 with `riak-repl  connect <sink_ip>:<port>`:

```bash
$ riak-repl connect 10.60.77.10:9080
```

> The port can be found in the `riak_core` section of the `app.config` under `cluster_mgr`.

#### View your active connections

From Cluster1, view your active connections with `riak-repl connections`:

```bash
Sink             Cluster Name         <Ctrl-Pid>      [Members]
----             ------------         ----------      ---------
Cluster2          Cluster2            <0.7985.0>      ["10.60.77.10:9080"] (via 10.60.77.10:9080)
```

### Set up Cluster2 &rarr; Cluster1 Connection (if bidirectional replication is desired)

#### Connect the Source to the Sink

From Cluster2, connect to the IP and port of Cluster1 with `riak-repl  connect <sink_ip>:<port>`:

```bash
$ riak-repl connect 10.60.67.149:9080
```
	
#### View Your Active Connections

From Cluster2, view your active connections with `riak-repl connections`:

```bash
Sink             Cluster Name         <Ctrl-Pid>      [Members]
----             ------------         ----------      ---------
Cluster1          Cluster1            <0.4456.0>      ["10.60.67.149:9080"] (via 10.60.67.149:9080)
```

> At this point, if you do not have connections, then replication will not work. Check your IP bindings by running `netstat -a` on all nodes. You should see `*:9080 LISTENING`. If not, you have configuration problems.

### Enable Realtime Replication

From Cluster1, run `riak-repl realtime enable <clustername>` to start queuing updates on Cluster1 for replication:

```bash
$ riak-repl realtime enable Cluster2
```
	
Also on Cluster1, run `riak-repl realtime start <clustername>` to establish connectivity from Cluster1 to Cluster2 to push queued updates:

```bash
$ riak-repl realtime start Cluster2
```
	
To enable bidirectional replication, do the reverse from Cluster2.  Once this is done, bidirectional replication should be operating.
	
### More information

For a full list of commands, you may enter `riak-repl` to see full instructions on usage, or check the [[Operations|Multi Data Center Replication v3 Operations]] documentation.
