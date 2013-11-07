---
title: What is Riak CS
project: riakcs
document: tutorial
audience: beginner
keywords: [tutorial, fast-track, installing]
prev: "[[The Riak CS Fast Track]]"
up:   "[[The Riak CS Fast Track]]"
next: "[[Building a Local Test Environment]]"
versions: false
---

This page introduces the architecture behind Riak CS. If you already know this, you can skip it and progress to [[Building a Local Test Environment]] or [[Building a Virtual Testing Environment]]

## Architecture
Riak CS is built on Riak. When an object is uploaded, Riak CS breaks the object into smaller blocks that are streamed, stored, and replicated in the underlying Riak cluster. Each block is associated with metadata for retrieval. Since data is replicated, and other nodes automatically take over responsibilities of nodes that go down, data remains available even in failure conditions.

### How It Works
In a Riak CS system, any node can respond to client requests - there is no master node and each node has the same responsibilities. Since data is replicated (three replicas per object by default), and other nodes automatically take over the responsibility of failed or non-communicative nodes, data remains available even in the event of node failure or network partition.

When an object is uploaded via the [[storage API|RiakCS Storage API]], Riak CS breaks the object into smaller chunks that are streamed, written, and replicated in Riak. Each chunk is associated with metadata for later retrieval. The diagram below provides a visualization. 

![Riak CS Chunking](/images/Riak-CS-Overview.png)

## Riak CS Enterprise
Riak CS Enterprise extends Riak CS with multi-datacenter replication, monitoring, and 24×7 support. Customers use multi-datacenter replication to serve global traffic, create availability zones, maintain active backups, or meet disaster recovery and regulatory requirements. Multi-datacenter replication can be used in two or more sites. Data can be replicated across data centers using realtime or fullsync replication. To try out Riak CS Enterprise, sign up for a [[developer trial|http://info.basho.com/RiakCS1.1_DeveloperTrialRequest.html]].

