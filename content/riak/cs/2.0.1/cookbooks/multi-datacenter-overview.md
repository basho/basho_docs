---
title: "Riak CS Multi-Datacenter Overview"
description: ""
menu:
  riak_cs-2.0.1:
    name: "Riak CS Enterprise"
    identifier: "mdc_overview"
    weight: 600
    pre: cloud
project: "riak_cs"
project_version: "2.0.1"
aliases:
  - /riakcs/2.0.1/cookbooks/MDC-Overview/
  - /riak/cs/2.0.1/cookbooks/MDC-Overview/
---

## Riak CS Enterprise

Riak CS Enterprise extends Riak CS with Multi-Datacenter Replication,
monitoring, and 24×7 support. Customers may use Multi-Datacenter
Replication to serve global traffic, create availability zones, maintain
active backups, or meet disaster recovery and regulatory requirements.
Multi-Datacenter Replication can be used in two or more sites, and data
can be replicated across datacenters using realtime or fullsync
synchronization.

If you are interested, sign up for a [developer trial](http://info.basho.com/RiakCS1.1_DeveloperTrialRequest.html) of Riak CS Enterprise or [contact us](http://basho.com/contact/) for more information.

<div class="note">
<div class="title">Riak CS Enterprise requires a separate download</div>
Please note that Riak CS Enterprise requires a download separate from
the open-source Riak CS, which will not work in conjunction with Riak
Enterprise.
</div>

## Multi-Datacenter Replication

Multi-Datacenter Replication in Riak CS provides two modes of object
replication: **fullsync** and **realtime sync**. Data is streamed over a
TCP connection and Multi-Datacenter Replication in Riak CS has support
for SSL so that data can be securely replicated between sites.

In Riak CS, large objects are broken into blocks and streamed to the
underlying Riak cluster on write, where they are replicated for high
availability (3 replicas by default). A manifest for each object is
maintained so that blocks can be retrieved from the cluster and the full
object presented to clients. For multi-site replication in Riak CS,
global information for users, bucket information, and manifests are
streamed in realtime from a primary implementation (a **source**
cluster) to a secondary site (a **sink** cluster) so that global state
is maintained across locations. Objects can then be replicated in either
fullsync or realtime sync mode.

## Fullsync Mode

In a fullsync operation, objects are replicated from a primary Riak CS
implementation to a secondary site on a configurable interval (the
default is 6 hours). In fullsync replication, each cluster computes a
hash for each key’s block value. Key/block pairs are compared and the
primary site streams any missing blocks or updates needed to the
secondary site.

## Realtime Mode

Realtime sync is triggered when an update is sent from a client to a
primary Riak CS implementation. Once replicated in the first location,
the updated manifests are streamed in real time to the secondary site.
But what happens if a client requests an object from the secondary
cluster and not all of its blocks have been replicated to that cluster?
With Riak multi-site replication, the secondary cluster will request any
missing blocks via `proxy_get` from the primary cluster so that the
client can be served.
