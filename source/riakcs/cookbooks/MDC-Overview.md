---
title: Riak CS Multi Data Center Overview
project: riakcs
version: 1.3.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator]
---

Multi-datacenter replication in Riak CS provides two modes of object
replication: full sync and real-time sync. Data is streamed over a TCP
connection, and multi-datacenter replication in Riak CS has support for SSL
so data can be securely replicated between sites.

In Riak CS, large objects are broken into blocks and streamed to the
underlying Riak cluster on write, where they are replicated for high
availability (3 replicas by default). A manifest for each object is
maintained so that blocks can be retrieved from the cluster and the full
object presented to clients. For multi-site replication in Riak CS, global
information for users, bucket information and manifests are streamed in
real-time from a primary implementation to a secondary site so global state
is maintained across locations. Objects can then be replicated in either full
sync or real-time sync mode.

## Full Sync Mode
In full sync, objects are replicated from a primary Riak CS implementation to
a secondary site on a configurable interval - the default is 6 hours. In
full-sync replication, each cluster computes a hash for each key’s block
value. Key/block pairs are compared, and the primary site streams any missing
blocks or updates needed to the secondary site.

## Real-Time Sync Mode
Real-time sync is triggered when an update is sent from a client to a primary
Riak CS implementation. Once replicated in the first location, the updates
are streamed in real-time to the secondary site. But what happens if a client
requests an object from the secondary cluster and not all of its blocks have
been replicated to that cluster? With Riak multi-site replication, the
secondary cluster will request any missing blocks from the primary cluster so
that the client can be served.
