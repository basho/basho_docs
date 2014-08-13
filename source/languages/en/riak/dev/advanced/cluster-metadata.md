---
title: Cluster Metadata
project: riak
version: 2.0.0+
document: guide
toc: true
audience: advanced
keywords: [developers, advanced, cluster-metadata]
---

Cluster metadata is a subsystem inside of Riak that enables systems built on top of [`riak_core`](https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata.erl) to work with information that is stored cluster wide and can be read without blocking on communication over the network.

One notable example, among many, of a subsystem of Riak relying on cluster metadata is Riak's [[bucket types|Using Bucket Types]] feature, which requires that a particular form of key/value pairs, namely bucket types and their associated bucket properties, be asynchronously broadcast to all nodes in a Riak cluster.

Though it is different in crucial respects, [etcd](https://coreos.com/docs/cluster-management/setup/getting-started-with-etcd/) is a roughly analogous cluster metadata key/value store for [CoreOS](https://coreos.com/) clusters.

## How Cluster Metadata Works

Cluster metadata is different from other Riak data in two essential ways:

1. Cluster metadata is intended only for internal Riak applications that require metadata shared on a system-wide basis. Regular stored data, on the other hand, is intended for use outside of Riak.
2. Because it is intended for use by internal Riak applications, cluster metadata can be accessed only via the Erlang interface provided by the [`riak_core_metadata`](https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata.erl) module and not via HTTP or protocol buffers.

The storage system backing cluster metadata is a simple key/value store that is capable of asynchronously replicating information to all nodes in a cluster when it is stored or modified. Writes require acknowledgment from only a single node (`w=1`), while reads return values only from the local node (`r=1`). All updates are eventually consistent and propagated to all nodes, including nodes that join the cluster after the update has already reached all nodes in the previous set of members.

All cluster metadata is stored both in memory and on disk, but it should be noted that reads are only from memory, while writes are made both to memory and to disk. Logical clocks, namely [[dotted version vectors]], are used in place of vclocks or timestamps to resolve value conflicts. Values stored as cluster metadata are opaque Erlang terms addressed by both prefix and a key.

## Erlang Code Interface

If you'd like to use cluster metadata for an internal Riak application, the Erlang interface is defined in the [`riak_core_metadata`](https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata.erl) module, which allows you to perform a variety of operations, including retrieving, modifying, and deleting metadata and iterating through metadata keys.