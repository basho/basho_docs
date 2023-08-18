---
title: "Riak CS Multi-Datacenter Overview"
description: ""
menu:
  riak_cs-3.0.0:
    name: "Riak CS"
    identifier: "mdc_overview"
    weight: 600
    pre: cloud
project: "riak_cs"
project_version: "3.0.0"
aliases:
  - /riakcs/3.0.0/cookbooks/mdc-overview/
  - /riak/cs/3.0.0/cookbooks/mdc-overview/


---

## Multi-Datacenter Replication

Customers may use Multi-Datacenter Replication to serve global traffic,
create availability zones, maintain active backups, or meet disaster
recovery and regulatory requirements. Multi-Datacenter Replication can
be used in two or more sites, and data can be replicated across
datacenters using realtime and/or fullsync synchronization.

Multi-Datacenter Replication in Riak CS provides two modes of object
replication: **fullsync** and **realtime sync**. Data is streamed over a
TCP connection and Multi-Datacenter Replication in Riak CS has support
for SSL so that data can be securely replicated between sites. It is
also possible to stream the data unencrypted if required but it is generally
recommended to do so over VPN if that is the case.

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
