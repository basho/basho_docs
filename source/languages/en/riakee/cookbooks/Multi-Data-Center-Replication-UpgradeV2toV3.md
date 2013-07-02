---
title: "Multi Data Center Replication: Upgrading from V2 to V3"
project: riakee
version: 1.3.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl]
---

This guide walks through the process of upgrading Riak Enterprise MDC Replication from version 2 to version 3. Please read the [[Multi Data Center Replication UpgradeV2toV3]] guide for terminology differences between versions.

### Upgrade Process
***TODO: Number steps***

* ensure all system backups have completed and are accurate
* stop fullsync replication on every cluster participating in MDC

`riak-repl cancel-fullsync`

* name source and sink clusters
	
On the source cluster:

`riak-repl clustername neywork`
	
On the sink cluster:
	
`riak-repl clustername boston`

You can verify the cluster names have been established on each cluster by issuing the `riak-repl clustername` command without parameters.

* connect the source cluster to the sink cluster

On any node in the source cluster:
`riak-repl connect <sink_cluster_name>`

where <sink_cluster_name> is replaced with the sink cluster name from step #TODO.

* ensure cluster connections have been established:
`riak-repl connections`

#### realtime
* To begin queuing objects on the source cluster for realtime replication:
`riak-repl realtime enable <sink_cluster_name>`

Running `riak-repl status` on any node of the source will show which sinks are currently enabled for realtime.

* To start realtime replication from source to sink:
`riak-repl realtime start <sink_cluster_name>`

Running `riak-repl status` on any node of the source will show which sinks are currently running for realtime. #TODO mention queue/socket stats

#### fullsync
* To prepare a source cluster for fullsync replication:
`riak-repl fullsync enable <sink_cluster_name>`

Running `riak-repl status` on any node of the source will show which sinks are currently enabled for realtime.

* To start fullsync replication from source to sink:
`riak-repl fullsync start <sink_cluster_name>`

Running `riak-repl status` on any node of the source will show which sinks are currently running a fullsync. #TODO mention queue/socket stats

#### proxy_get for Riak CS Enterprise

`riak-repl proxy_get enable <sink_cluster_name>`




