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

* ensure all system backups have completed, are accurate, and are readily available
* stop fullsync replication on every cluster participating in MDC

`riak-repl cancel-fullsync`

* name the source and sink clusters that will be participating in replication. In the following examples, the source cluster will be named `newyork`, while the sink cluster will be named `boston`.
	
On the source cluster:

`riak-repl clustername neywork`
	
On the sink cluster:
	
`riak-repl clustername boston`

You can verify the cluster names have been established on each cluster by issuing the `riak-repl clustername` command without parameters.

* connect the source cluster to the sink cluster

On any node in the source cluster:

`riak-repl connect <sink_cluster_name>`

For example:

	riak-repl connect boston
	
where <sink_cluster_name> is replaced with the name of the sink cluster.

* ensure cluster connections have been established:
`riak-repl connections`

#####To establish bidirectional replication:
Bidirectional replication between two named clusters can be established by connecting from *sink* to *source*.

On any node in the **sink** cluster:

`riak-repl connect <source_cluster_name>`

For example:

	riak-repl connect newyork


#### Realtime Replication
* To begin queuing objects (but not yet replicating) on the source cluster for realtime replication:
`riak-repl realtime enable <sink_cluster_name>`

Running `riak-repl status` on any node of the source will show which sinks are currently enabled for realtime.

* To start realtime replication from source to sink:
`riak-repl realtime start <sink_cluster_name>`

This will start process any objects in the replication queue, as well as any updated objects in the source cluster.

Running `riak-repl status` on any node of the source will show which sinks are currently running for realtime. #TODO mention queue/socket stats

#### Fullsync Replication
* To prepare a source cluster for fullsync replication:
`riak-repl fullsync enable <sink_cluster_name>`

Running `riak-repl status` on any node of the source will show which sinks are currently enabled for realtime.

* To start fullsync replication from source to sink:
`riak-repl fullsync start <sink_cluster_name>`

Running `riak-repl status` on any node of the source will show which sinks are currently running a fullsync. #TODO mention queue/socket stats

#### proxy_get for Riak CS Enterprise

proxy_get connections for version 3 replication are enabled at runtime using the following command:

`riak-repl proxy_get enable <sink_cluster_name>`

Example:

	riak-repl proxy_get enable boston
	
See the [[Multi Data Center Replication v3 Operations]] and [[Multi Data Center Replication v3 Configuration]] guides for more information.

##### Remove existing listeners/sites

Remove all configured listeners and sites from both source and sink clusters using the `del-listener` and `del-site` commands:

* *Example:* `riak-repl del-site newyork`
* *Example:* `riak-repl del-listener riak@10.0.1.156 10.0.1.156 9010`

Currently configured listeners and sites can be seen using `riak-repl status` on each node in the source and sink clusters.

See the [[Multi Data Center Replication Operations]] guide for more information on `del-listener` and `del-site` commands.

##### Remove and/or update cron tasks

If you are using `cron` to schedule replication fullsyncs, please update the crontab to use the version 3 fullsync syntax

For example, version 2 replication is started with `riak-repl start-fullsync`, while version 3 replication is started with `riak-repl fullsync start boston` (using *boston* as an example sink cluster name).

See the [[Multi Data Center Replication v3 Scheduling Full Sync]] guide for configuring scheduled replication fullsyncs.

##### Disable version 2 replication bucket hooks

By default, Riak 1.3.+ has replication bucket hooks enabled for both version 2 and version 3. The version 2 replication bucket hook can be disabled. This step is not required, but leaving the version 2 replication bucket hook enabled can cause inaccurate `objects_dropped_no_leader` and `objects_dropped_no_clients` statistics.

* To disable the version 2 replication bucket hook:

`riak-repl modes mode_repl13`

Example:

	$riak-repl modes
	Current replication modes: [mode_repl12,mode_repl13]

	$riak-repl modes mode_repl13
	Current replication modes: [mode_repl13]


