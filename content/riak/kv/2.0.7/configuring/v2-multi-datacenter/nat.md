---
title_supertext: "V2 Multi-Datacenter Replication:"
title: "With NAT"
description: ""
project: "riak_kv"
project_version: "2.0.7"
menu:
  riak_kv-2.0.7:
    name: "With NAT"
    identifier: "configuring_v2_replication_nat"
    weight: 101
    parent: "configuring_v2"
toc: true
commercial_offering: true
aliases:
  - /riak/2.0.7/ops/mdc/v2/nat
  - /riak/kv/2.0.7/ops/mdc/v2/nat
---

[config v2 ssl]: {{<baseurl>}}riak/kv/2.0.7/configuring/v2-multi-datacenter/ssl

Riak Enterprise supports replication of data on networks that use static
NAT. This capability can be used for replicating data over the internet
where servers have both internal and public IP addresses (see [Riak
REPL SSL][config v2 ssl] if you replicate data over a public network).

## Requirements

In order for Multi-Datacenter Replication to work on a server configured
with NAT, the NAT addresses must be configured statically.

## Example

Imagine the following scenario:

* Server A is the source of replicated data
* Servers B and C would like to be clients of the replicated data

Server A is set up with static NAT, configured for IP addresses:

  * `192.168.1.10` (internal) and `50.16.238.123` (public)

Server A replication will listen on:

  * the internal IP address `192.168.1.10`, port `9010`
  * the public IP address `50.16.238.123`, port `9011`

Server B is set up with a single public IP address: `50.16.238.200`

  * Server B replication will connect as a client to the public IP
    address `50.16.238.123`, port `9011`

Server C is set up with a single internal IP address: `192.168.1.20`

  * Server C replication will connect as a client to the internal IP
    address of `192.168.1.10`, port `9010`

Configure a listener on Server A:

```bash
riak-repl add-nat-listener riak@192.168.1.10 192.168.1.10 9010 50.16.238.123 9011
```

Configure a site (client) on Server B:

```bash
riak-repl add-site 50.16.238.123 9011 server_a_to_b
```

Configure a site (client) on Server C:

```bash
riak-repl add-site 192.168.1.10 9010 server_a_to_c
```
