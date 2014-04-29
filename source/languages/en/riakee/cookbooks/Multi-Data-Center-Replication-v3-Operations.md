---
title: "Multi Data Center Replication v3 Operations"
project: riakee
version: 1.3.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, operator, bnw]
moved: {
    '1.4.0-': '/cookbooks/Multi-Data-Center-Replication-Operations-New'
}
---

This document explains how to manage replication with the `riak-repl` command. Some of these commands can be set or behavior altered by setting appropriate [[configuration|Multi Data Center Replication v3 Configuration]] values.

All commands need to be run only once on a single node of a cluster for the changes to propagate to all other nodes. All changes will persist across node restarts and will automatically take effect when nodes are added to the cluster.

## Cluster Connectivity

#### `clustername`

Set the `clustername` for all nodes in a Riak cluster. The IP and port to connect to can be found in the `app.config` of the remote cluster, under `riak_core` » `cluster_mgr`.

* Without a parameter, returns the current name of the cluster
* With a parameter, names the current cluster

To **set** the `clustername`:

* *Syntax:* `riak-repl clustername <clustername>`
* *Example:* `riak-repl clustername Boston`

To **get** the `clustername`:

* *Syntax:* `riak-repl clustername`
* *Example:* `riak-repl clustername`

#### `connect`

The `connect` command establishes communications from a source cluster to a sink cluster of the same ring size. The `host:port` of the sink cluster is used for this.

The `host` can be either an IP address

* *Syntax:* `riak-repl connect <ip>:<port>`
* *Example:* `riak-repl connect 192.168.2.1:9080`

...or a hostname that will resolve to an IP address.

* *Syntax:* `riak-repl connect <host>:<port>`
* *Example:* `riak-repl connect Austin:9080`

#### `disconnect`

Disconnecting a source cluster from a sink cluster.

You may define a `host:port` combination

* *Syntax:* `riak-repl disconnect <host>:<port>`
* *Example:* `riak-repl disconnect 192.168.2.1:9080`

...or use the *name* of the cluster.

* *Syntax:* `riak-repl disconnect <sink_clustername>`
* *Example:* `riak-repl disconnect Austin`

#### `connections`

Display a list of connections between source and sink clusters.

* *Syntax:* `riak-repl connections`
* *Example:* `riak-repl connections`

**clusterstats**

Displays current cluster stats using an optional `ip:port` as well as an optional `protocol-id`.

`protocol-id` can be one of the following:

* `cluster_mgr`
* `rt_repl`
* `fs_repl`

The `clusterstats` command in use:

* *Syntax:* `riak-repl clusterstats <host>:<port> <protocol-id>`
* *Example:* `riak-repl clusterstats 192.168.2.1:9080`
* *Example:* `riak-repl clusterstats 192.168.2.1:9080 fs_repl`


## Realtime Replication Commands

#### `realtime enable`

Enable realtime replication from a source cluster to sink clusters.

This will start queuing updates for replication. The cluster will still require an invocation of `realtime start` for replication to occur.

* *Syntax:* `riak-repl realtime enable <sink_clustername>`
* *Example:* `riak-repl realtime enable Austin`

#### `realtime disable`

Disable realtime replication from a source cluster to sink clusters.

* *Syntax:* `riak-repl realtime disable <sink_clustername>`
* *Example:* `riak-repl realtime disable Austin`


#### `realtime start`

Start realtime replication connections from a source cluster to sink clusters. See also `realtime enable` (above).

* *Syntax:* `riak-repl realtime start <sink_clustername>`
* *Example:* `riak-repl realtime start Austin`

#### `realtime stop`

Stop realtime replication from a source cluster to sink clusters.

* *Syntax* `riak-repl realtime stop <sink_clustername>`
* *Example* `riak-repl realtime stop Austin`


## Fullsync Replication Commands

These behaviors can be altered by using the `app.config` `fullsync_on_connect` parameter. See the [[Configuration Guide|Multi Data Center Replication v3 Configuration]] for more information.

#### `fullsync enable`

Enable fullsync replication from a source cluster to sink clusters. By default, a fullsync will begin as soon as a connection to the remote cluster is established.

* *Syntax:* `riak-repl fullsync enable <sink_clustername>`
* *Example:* `riak-repl fullsync enable Austin`

#### `fullsync disable`

Disables fullsync for a cluster.

* *Syntax:* `riak-repl fullsync disable <sink_clustername>`
* *Example:* `riak-repl fullsync disable Austin`

#### `fullsync start`

Starts a fullsync. If the application configuration `fullsync_on_connect` is set to `false`, a fullsync needs to be started manually. This is also used to trigger a periodic fullsync using a cron job. While a fullsync is in progress, a `start` command is ignored and a message is logged.

* *Syntax:* `riak-repl fullsync start <sink_clustername>`
* *Example:* `riak-repl fullsync start Austin`

#### `fullsync stop`

Stops a fullsync.

* *Syntax:* `riak-repl fullsync stop <sink_clustername>`
* *Example:* `riak-repl fullsync stop Austin`

## Cascading Realtime Writes

See the [[Multi Data Center Replication: Cascading Realtime Writes]] guide

#### `realtime cascades`

Shows the current cascading realtime setting.

* *Syntax*: `realtime cascades`
* *Example*: `riak-repl realtime cascades`

#### `realtime cascades always`

Enable realtime cascading writes.

* *Syntax*: `realtime cascades always`
* *Example*: `riak-repl realtime cascades always`

**realtime cascades never**

Disable realtime cascading writes.

* *Syntax*: `realtime cascades never`
* *Example*: `riak-repl realtime cascades never`


## NAT

**Note**: See the [[Configuration Guide|Multi Data Center Replication v3 With NAT]] for more information

#### `nat-map show`
Show the current NAT mapping table.

* *Syntax:* `nat-map show`
* *Example:* `riak-repl nat-map show`

#### `nat-map add`
Adds a NAT map from the external IP, with an optional port, to an internal IP.

* *Syntax:* `nat-map add <externalip>[:port] <internalip>`
* *Example:* `riak-repl nat-map add 128.205.106.1:5555 192.168.1.2`

#### `nat-map del`
Deletes a specific NAT map entry.

* *Syntax:* `nat-map del <externalip>[:port] <internalip>`
* *Example:* `riak-repl nat-map del 128.205.106.1:5555 192.168.1.2`

NAT changes will be applied once fullsync and/or realtime replication has been stopped and started.


## Riak CS MDC Gets

#### `proxy-get enable`
Enable Riak CS `proxy_get` requests from a **sink** cluster (if `proxy_get` has been enabled in `app.config`).

* *Syntax:* `proxy-get enable  <sink_clustername>`
* *Example:* `riak-repl proxy-get enable  newyorkbackup`

#### `proxy-get disable`
Disable Riak CS `proxy_get` requests from a **sink** cluster (if `proxy_get` has been enabled in `app.config`).

* *Syntax:* `proxy-get disable <sink_clustername>`
* *Example:* `riak-repl proxy-get disable newyorkbackup`

#### `add-block-provider-redirect`
Provide a redirection to the <to-cluster-id> for proxy_get if the <from-cluster> is going to be decommissioned.

* *Syntax:* `riak-repl add-block-provider-redirect <from-cluster> <to-cluster>`
* *Example:* `riak-repl add-block-provider-redirect "{'dev1@127.0.0.1',{1391,544501,519016}}" "{'dev3@127.0.0.1',{1299,512501,511032}}"`

#### `show-block-provider-redirect`
Show the mapping for a given cluster-id redirect.

* *Syntax:* `riak-repl show-block-provider-redirect <from-cluster>`
* *Example:* `riak-repl show-block-provider-redirect "{'dev1@127.0.0.1',{1391,544501,519016}}"`

#### `delete-block-provider-redirect`
Delete a existing redirect such that proxy_gets go again to the original provider cluster id.

* *Syntax:* `riak-repl delete-block-provider-redirect <from-cluster>`
* *Example:* `riak-repl delete-block-provider-redirect "{'dev1@127.0.0.1', {1391,544501,519016}}"`

#### `show-local-cluster-id`
Display this cluster's cluster-id tuple, for use with the *-block-provider-redirect commands. **Note**: a cluster-id is surrounded by double quotes, which need to be included when passed to *-block-provider-redirect

* *Syntax:* `riak-repl show-local-cluster-id`
* *Example:* 

```
$ riak-repl show-local-cluster-id
local cluster id: "{'dev1@127.0.0.1',{1391,544501,519016}}"
```

## `riak-repl` Status Output

Details about the `riak-repl status` command can be found under [[Statistics|Multi Data Center Replication: Statistics]].


## Tuning

These tuning values may also be set via the node's `app.config` file. See the [[Configuration Guide|Multi Data Center Replication v3 Configuration]] for more information.

#### `fullsync max_fssource_node`

This limits the number of fullsync workers that will be running on each individual node in a source cluster. This is a hard limit for *all* fullsyncs that are enabled. Additional fullsync configurations will *not* increase the number of fullsync workers allowed to run on any node. This only affects nodes on the source cluster on which this parameter is defined via the configuration file or command line.

* *Syntax:* `riak-repl fullsync max_fssource_node <value>`
* *Default:* `1`
* *Example:* `riak-repl fullsync max_fssource_node 2`

#### `fullsync max_fssource_cluster`

This is the hard limit of fullsync workers that will be running on the source side of a cluster across all nodes on that cluster for a fullsync to a sink cluster. This means if one has configured fullsync for two different clusters, both with a max_fssource_cluster of 5, 10 fullsync workers can be in progress. Only affects nodes on the source cluster on which this parameter is defined via the configuration file or the command line.

* *Syntax:* `riak-repl fullsync max_fssource_cluster <value>`
* *Default:* `5`
* *Example:* `riak-repl fullsync max_fssource_cluster 5`


**fullsync max_fssink_node**

This limits the number of fullsync workers allowed to run on each individual node in a sink cluster. This is a hard limit for each fullsync source node interacting with a sink node. Thus, multiple simultaneous source connections to a sink node will have to share the sink node’s number of maximum connections. Only affects nodes on the sink cluster on which this parameter is defined via the configuration file or command line.

* *Syntax* `riak-repl fullsync max_fssink_cluster <value>`
* *Default* `1`
* *Example* `riak-repl fullsync max_fssink_cluster 5`


## Mixing Version 2 Replication with Version 3 Replication

Riak Version 2 Replication and Version 3 Replication can be safely used at the same time. If you choose to move to Version 3 Replication completely, it is recommended that you disable Version 2 realtime replication bucket hooks with the `riak-repl modes` command.

#### `riak-repl modes`

`modelist` is one or both of `mode_repl12` (Version 2) or `mode_repl13` (Version 3) separated by *spaces* (without commas).

* *Syntax:* `riak-repl modes <modelist>` 
* *Example:* 

    ```bash
    $ riak-repl modes mode_repl12 mode_repl13
    Current replication modes: [mode_repl12,mode_repl13]
    ```

To check the current replication modes:

* *Syntax:* `riak-repl modes` 
* *Example:*

    ```bash
    $ riak-repl modes
    Current replication modes: [mode_repl12,mode_repl13]
    ```
