---
title_supertext: "V3 Multi-Datacenter Replication:"
title: "With NAT"
description: ""
project: "riak_kv"
project_version: 2.9.8
menu:
  riak_kv-2.9.8:
    name: "With NAT"
    identifier: "configuring_v3_replication_nat"
    weight: 101
    parent: "configuring_v3"
toc: true
commercial_offering: true
aliases:
  - /riak/2.9.8/ops/mdc/v3/nat
  - /riak/kv/2.9.8/ops/mdc/v3/nat
---

[config v3 ssl]: {{<baseurl>}}riak/kv/2.9.8/configuring/v3-multi-datacenter/ssl

Riak's Version 3 Replication supports replication of data on
networks that use static NAT.

This can be used for replicating data over the internet where servers
have both internal and public IP addresses (see the [Replication SSL docs][config v3 ssl] if you replicate data over a public network).

### Requirements

In order for Replication to work on a server configured with NAT, the
NAT addresses must be configured *statically*.

## Configuration

NAT rules can be configured at runtime, from the command line.

* `riak-repl nat-map show`

    Shows the current NAT mapping table

* `riak-repl nat-map add <externalip>[:port] <internalip>`

    Adds a NAT map from the external IP, with an optional port, to an
    internal IP. The port number refers to a port that is automatically
    mapped to the internal `cluster_mgr` port number.

* `riak-repl nat-map del <externalip>[:port] <internalip>`

    Deletes a specific NAT map entry.

### Applying Changes at Runtime

* Realtime NAT replication changes will be applied once realtime is
  stopped and started using the following command:

    * `riak-repl realtime stop <clustername>`
    * `riak-repl realtime start <clustername>`

* Fullsync NAT replication changes will be applied on the next run of a
  fullsync, or you can stop and start the current fullsync.

    * `riak-repl fullsync stop <clustername>`
    * `riak-repl fullsync start <clustername>`


## Example

* Cluster_A is the **source** of replicated data.
* Cluster_B and Cluster_C are the **sinks** of the replicated data.

### Cluster_A Setup

Cluster_A is set up with nodes using the following **internal** IP
addresses:

Internal IP    | Public IP
---------------|-------------------
`192.168.1.20` | -
`192.168.1.21` | -
`192.168.1.22` | -
`192.168.1.23` | -
`192.168.1.24` | -

### Cluster_B Setup

A node from Cluster_B will be configured as follows:

Internal IP    | Public IP
---------------|-------------------
`192.168.2.40` | `50.16.238.120:5555`
`192.168.2.41` | `50.16.238.121:5555`
`192.168.2.42` | `50.16.238.122:5555`
`192.168.2.43` | `50.16.238.123:5555`
`192.168.2.44` | `50.16.238.124:5555`

In this example, the `cluster_mgr` port number is the default of `9080`,
while the configured NAT port listens on `5555`.

### Cluster_C Setup

A node from Cluster_C is set up with **static NAT**, configured with the
following IP addresses:

Internal IP    | Public IP
---------------|-------------------
`192.168.3.60` | `50.16.238.200:5550`
`192.168.3.61` | `50.16.238.200:5551`
`192.168.3.62` | `50.16.238.200:5552`
`192.168.3.63` | `50.16.238.200:5553`
`192.168.3.64` | `50.16.238.200:5554`

In this example, the `cluster_mgr` port number is the default of `9080`,
while the configured NAT port listens on `5566`.

```bash
# on any node of Cluster_A
riak-repl clustername Server_A

# on any node of Cluster_B
riak-repl clustername Server_B

# on any node of Cluster_C
riak-repl clustername Server_C

# on 50.16.238.120 of Cluster_B
riak-repl nat-map add 50.16.238.120:5555 192.168.2.40
# on 50.16.238.121 of Cluster_B
riak-repl nat-map add 50.16.238.121:5555 192.168.2.41
# on 50.16.238.122 of Cluster_B
riak-repl nat-map add 50.16.238.122:5555 192.168.2.42
# on 50.16.238.123 of Cluster_B
riak-repl nat-map add 50.16.238.123:5555 192.168.2.43
# on 50.16.238.124 of Cluster_B
riak-repl nat-map add 50.16.238.124:5555 192.168.2.44

# on 192.168.3.60 of Cluster_C
riak-repl nat-map add 50.16.238.200:5550 192.168.3.60
# on 192.168.3.61 of Cluster_C
riak-repl nat-map add 50.16.238.200:5551 192.168.3.61
# on 192.168.3.62 of Cluster_C
riak-repl nat-map add 50.16.238.200:5552 192.168.3.62
# on 192.168.3.63 of Cluster_C
riak-repl nat-map add 50.16.238.200:5553 192.168.3.63
# on 192.168.3.64 of Cluster_C
riak-repl nat-map add 50.16.238.200:5554 192.168.3.64


# Connect replication from Cluster_A to Cluster_B:
# on any node of Cluster_A
riak-repl connect 50.16.238.120:5555
# You can connect to any node in Cluster_B with NAT mapped IP's/ports
# This command only needs to be run *once* for a cluster.

# Connect replication from Cluster_A to Cluster_C:
# on any node of Cluster_A
riak-repl connect 50.16.238.200:5550
# You can connect to any node in Cluster_C with NAT mapped IP's/ports
# This command only needs to be run *once* for a cluster.


# on any node from Cluster_A
riak-repl realtime enable Cluster_B
riak-repl realtime enable Cluster_C

riak-repl realtime start Cluster_B
riak-repl realtime start Cluster_C
```




