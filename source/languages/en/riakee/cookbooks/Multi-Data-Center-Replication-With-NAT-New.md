---
title: "Multi Data Center Replication: With NAT"
project: riakee
version: 1.4.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, nat]
---

Riak Enterprise Advanced Replication supports replication of data on networks that use static NAT.

This can be used for replicating data over the internet where servers have both internal and public IP addresses (see [[Riak REPL SSL|Multi Data Center Replication SSL New]] if you replicate data over a public network).

### Requirements:
In order for Replication to work on a server configured with NAT, the NAT addresses must be configured **statically**.


### Configuration

NAT rules can be configured at runtime, from the command line.

* `nat-map show`

	Shows the current NAT mapping table

* `nat-map add <externalip>[:port] <internalip>`
	
	Adds a NAT map from the external IP, with an optional port, to an internal IP. The port number referrs to the #####TODO 

* `nat-map del <externalip>[:port] <internalip>`

	Deletes a specific NAT map entry.

* `riak-repl realtime stop <clustername>`
* `riak-repl realtime start <clustername>`

	Realtime NAT replication changes will be applied once realtime is stopped and started.

* `riak-repl fullsync stop <clustername>`
* `riak-repl fullsync start <clustername>`
	
	Fullsync NAT replication changes will be applied on the next run of a fullsync, or you can stop and start the current fullsync.



### Example

Cluster_A is the **source** of replicated data.

Cluster_B and Cluster_C would like to be **sinks** of the replicated data.

A node from Cluster_A is setup with *static NAT*, configured for IP addresses:

  * `192.168.1.10` (internal) and `50.16.238.123` (public)

A node from Cluster_B will be configured as follows:

  * the internal IP address `192.168.1.10`, with a `cluster_mgr` default port of 9080
  * the public IP address `50.16.238.123`, port 9081

A node from Cluster_B is setup with a single public IP address: `50.16.238.200`

  * Cluster_B will receive a connection from Cluster_A at the public IP address 50.16.238.123, port *9081*

A node from Cluster_C is setup with a single internal IP address: `192.168.1.20`

  * Cluster_C will receive a connection from Cluster_A at the public IP address 192.168.1.20, port *9080* (the default `cluster_mgr` port).

```   
	# on any node of Cluster A
	riak-repl clustername Server_A

	# on any node of Cluster B
	riak-repl clustername Server_B

	# on any node of Cluster C
	riak-repl clustername Server_C

	# on a node of Cluster A with the following IP's:
	nat-map add 50.16.238.123:9081 192.168.1.10

	# Connect replication from Cluster_A to Cluster_B:
	# on any node of Cluster_A
	riak-repl connect 50.16.238.200

	# Connect replication from Cluster_A to Cluster_C:
	# on any node of Cluster_A	
	riak-repl connect 192.168.1.20
```

