---
title: "What Is Riak CS"
description: ""
menu:
  riak_cs-3.0.1:
    name: "What Is Riak CS?"
    identifier: "fast_track_what"
    weight: 100
    parent: "fast_track"
project: "riak_cs"
project_version: "3.0.1"
lastmod: 2022-06-10T00:00:00-00:00
sitemap:
  priority: 0.9
aliases:
  - /riakcs/3.0.1/cookbooks/tutorials/fast-track/What-is-Riak-CS/
  - /riak/cs/3.0.1/cookbooks/tutorials/fast-track/What-is-Riak-CS/
  - /riak/cs/latest/tutorials/fast-track/what-is-riak-cs/
---

This page introduces the architecture behind Riak CS. If you already
know this, you can skip it and progress to [Building a Local Test Environment](../local-testing-environment) or [Building a Virtual Testing Environment](../virtual-test-environment).

## Architecture

Riak CS is built on Riak. When an object is uploaded, Riak CS breaks the
object into smaller blocks that are streamed, stored, and replicated in
the underlying Riak cluster. Each block is associated with metadata for
retrieval. Since data is replicated, and other nodes automatically take
over responsibilities of nodes that go down, data remains available even
in failure conditions.

### How It Works

In a Riak CS system, any node can respond to client requests - there is
no master node and each node has the same responsibilities. Since data
is replicated (three replicas per object by default), and other nodes
automatically take over the responsibility of failed or
non-communicative nodes, data remains available even in the event of
node failure or network partition.

When an object is uploaded via the [storage API]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage), Riak CS breaks the object into smaller chunks that are streamed,
written, and replicated in Riak. Each chunk is associated with metadata
for later retrieval. The diagram below provides a visualization.

![Riak CS Chunking]({{<baseurl>}}images/Riak-CS-Overview.png)

## Multi-DataCenter Replication

Riak CS also features Multi-Datacenter Replication. Customers use
Multi-Datacenter Replication to serve global traffic, create
availability zones, maintain active backups, or meet disaster recovery
 and regulatory requirements. Multi-Datacenter Replication can be used
in two or more sites. Data can be replicated across data centers using
realtime or fullsync replication.
