---
title: "Multi Data Center Replication: Operations (Advanced)"
project: riakee
version: 1.4.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, operator, bnw]
---

This document shows how to manage replication with the `riak-repl` command. Many of these commands can be set or behavior altered by setting appropriate [[Configuration Guide|Multi Data-Center Replication Configuration New]] values.

## Cluster Connectivity

**clustername**

Set the `clustername` for all nodes in a Riak cluster. *This only needs to be run once on a single node of a cluster for the changes to propagate to all other nodes.* The IP and port to connect to can be found in the `app.config` of the remote cluster, under `riak_core` » `cluster_mgr`.

To **set** the clustername:

* *Syntax:* `riak-repl clustername <clustername>`
* *Example:* `riak-repl clustername Boston`

To **get** the clustername:

* *Syntax:* `riak-repl clustername`
* *Example:* `riak-repl clustername`

**connect**

The `connect` command establishes communications from a source cluster to a sink cluster. The `host:port` of the sink cluster is used for this.

Host can be either an IP address,

* *Syntax:* `riak-repl connect <ip>:<port>`
* *Example:* `riak-repl connect 192.168.2.1:8085`

...or a hostname that will resolve to one.

* *Syntax:* `riak-repl connect <host>:<port>`
* *Example:* `riak-repl connect Austin:8085`


**disconnect**

Disconnecting a source cluster from a sink cluster. 

You may define a `host:port` combination,

* *Syntax:* `riak-repl disconnect <host>:<port>`
* *Example:* `riak-repl disconnect 192.168.2.1:8085`

...or use the *name* of the cluster.

* *Syntax:* `riak-repl disconnect <sink_clustername>`
* *Example:* `riak-repl disconnect Austin`


## Realtime Replication Configuration

**realtime enable**

Enable realtime replication from a source cluster to sink clusters.

* *Syntax:* `riak-repl realtime enable <sink_clustername>`
* *Example:* `riak-repl realtime enable Austin`

**realtime disable**

Disable realtime replication from a source cluster to sink clusters.

* *Syntax:* `riak-repl realtime disable <sink_clustername>`
* *Example:* `riak-repl realtime disable Austin`


**realtime start**

Start realtime replication from a source cluster to sink clusters.

* *Syntax:* `riak-repl realtime start <sink_clustername>`
* *Example:* `riak-repl realtime start Austin`

**realtime stop**

Stop realtime replication from a source cluster to sink clusters.

* *Syntax* `riak-repl realtime stop <sink_clustername>`
* *Example* `riak-repl realtime stop Austin`


## Fullsync Replication Configuration

These behaviors can be altered by using the app.config `fullsync_on_connect`. See the [[Configuration guide|Multi Data-Center Replication Configuration New]] for more information.

**fullsync enable**

Enable fullsync replication from a source cluster to sink clusters. By default, a fullsync will start as soon as a connection is established to the remote cluster.

* *Syntax:* `riak-repl fullsync enable <sink_clustername>`
* *Example:* `riak-repl fullsync enable Austin`

**fullsync disable**

Disables fullsync for this cluster.

* *Syntax:* `riak-repl fullsync disable <sink_clustername>`
* *Example:* `riak-repl fullsync disable Austin`

**fullsync start**

Starts a fullsync. If the application configuration `fullsync_on_connect` is set to `false`, a fullsync needs to be started manually. This is also used to periodically fullsync using a cron job. While a fullsync is in progress, a `start` command is ignored; a message is logged.

* *Syntax:* `riak-repl fullsync start <sink_clustername>`
* *Example:* `riak-repl fullsync start Austin`

**fullsync stop**

Stops a fullsync.

* *Syntax:* `riak-repl fullsync stop <sink_clustername>`
* *Example:* `riak-repl fullsync stop Austin`


## Cascading Realtime Writes
See the [[Configuration guide|Multi Data-Center Replication Cascading Writes New]]

**realtime cascades**

Shows the current cascading realtime setting.

* *Syntax*: `realtime cascades`
* *Example*: `riak-repl realtime cascades`

**realtime cascades always**

Enable realtime cascading writes.

* *Syntax*: `realtime cascades always`
* *Example*: `riak-repl realtime cascades always`


**realtime cascades never**

Disable realtime cascading writes.

* *Syntax*: `realtime cascades never`
* *Example*: `riak-repl realtime cascades never`


## NAT
See the [[Configuration guide|Multi Data-Center Replication NAT New]]

**nat-map show**
Show the current NAT mapping table.

* *Syntax:* `nat-map show`
* *Example:* `riak-repl nat-map show`

**nat-map add**
Adds a NAT map from the external IP, with an optional port, to an internal IP.

* *Syntax:* `nat-map add <externalip>[:port] <internalip>`
* *Example:* `riak-repl nat-map add 128.205.106.1:5555 192.168.1.2`

**nat-map del**
Deletes a specific NAT map entry.

* *Syntax:* `nat-map del <externalip>[:port] <internalip>`
* *Example:* `riak-repl nat-map del 128.205.106.1:5555 192.168.1.2`

NAT changes will be applied once fullsync and/or realtime replication is stopped and started.


## Riak CS MDC Gets
See the [[Configuration guide|TODO]]

* **riak-repl proxy-get enable**
Enable Riak CS proxy_get requests from a **sink** cluster (if `proxy_get` has been enabled in `app.config`).

	* *Syntax:* `proxy-get enable  <sink_clustername>`
	* *Example:* `riak-repl proxy-get enable  newyorkbackup` 

* **riak-repl proxy-get disable**
Disable Riak CS proxy_get requests from a **sink** cluster (if `proxy_get` has been enabled in `app.config`).

	* *Syntax:* `proxy-get disable <sink_clustername>`
	* *Example:* `riak-repl proxy-get disable newyorkbackup`


## riak-repl Status Output

Details about the `riak-repl status` command can be found under [[Replication Statistics|Multi Data Center Replication Status]].


## Tuning

These tuning values may also be set via the node's `app.config` file. See the [[Configuration guide|Multi Data-Center Replication Configuration New]] for more information.

**fullsync max_fssource_node**

This limits the number of fullsync workers that will be running on each individual node in a source cluster.  This is a hard limit for all fullsyncs enabled; additional fullsync configurations will not increase the number of fullsync workers allowed to run on any node. This only affects nodes on the source cluster on which this parameter is defined via the configuration file or command line.

* *Syntax:* `riak-repl fullsync max_fssource_node <value>`
* *Default:* `1`
* *Example:* `riak-repl fullsync max_fssource_node 2`

**fullsync max_fssource_cluster**

This is the hard limit of fullsync workers that will be running on the source side of a cluster across all nodes on that cluster for a fullsync to a sink cluster.  This means if one has configured fullsync for two different clusters, both with a max_fssource_cluster of 5, 10 fullsync workers can be in progress. Only affects nodes on the source cluster on which this parameter is defined via the configuration file or command line.

* *Syntax:* `riak-repl fullsync max_fssource_cluster <value>`
* *Default:* `5`
* *Example:* `riak-repl fullsync max_fssource_cluster 5`


**fullsync max_fssink_node**

This limits the number of fullsync workers allowed to run on each individual node in a sink cluster.  This is a hard limit for all fullsync sources interacting with the sink cluster. Thus, multiple simultaneous source connections to the sink cluster will have to share the sink node’s number of maximum connections. Only affects nodes on the sink cluster on which this parameter is defined via the configuration file or command line.


* *Syntax* `riak-repl fullsync max_fssink_cluster <value>`
* *Default* `1`
* *Example* `riak-repl fullsync max_fssink_cluster 5`


## Mixing Default Replication with Advanced Replication

Riak Default Replication and Advanced Replication can be safely used at the same time. If you choose to move to 1.3 Replication completely, it is recommended to disable Default realtime replication bucket hooks with the `riak-repl modes` command. 

**riak-repl modes**

`modelist` is one or both of `mode_repl12` (Default) or `mode_repl13` (Advanced) separated by *spaces* (without commas).

* *Syntax:* `riak-repl modes <modelist>` 
* *Example:* 

    ```
    riak-repl modes mode_repl12 mode_repl13
    Current replication modes: [mode_repl12,mode_repl13]
    ```

To check the current replication modes:

* *Syntax:* `riak-repl modes` 
* *Example:*

    ```
    riak-repl modes
    Current replication modes: [mode_repl12,mode_repl13]
    ```
