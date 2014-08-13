---
title: "Multi Data Center Replication: With NAT"
project: riak
header: riakee
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, nat]
moved: {
    '2.0.0-': 'riakee:/cookbooks/Multi-Data-Center-Replication-With-NAT'
}
---

Riak Enterprise supports replication of data on networks that use static
NAT. This capability can be used for replicating data over the internet
where servers have both internal and public IP addresses (see [[Riak
REPL SSL|Multi Data Center Replication: SSL]] if you replicate data over
a public network).

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
