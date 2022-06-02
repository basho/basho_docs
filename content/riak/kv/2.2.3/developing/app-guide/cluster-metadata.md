---
title: "Cluster Metadata"
description: ""
project: "riak_kv"
project_version: "2.2.3"
menu:
  riak_kv-2.2.3:
    name: "Cluster Metadata"
    identifier: "app_guide_cluster_metadata"
    weight: 104
    parent: "developing_app_guide"
toc: true
---

Cluster metadata is a subsystem inside of Riak that enables systems
built on top of
[`riak_core`](https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata.erl)
to work with information that is stored cluster wide and can be read
without blocking on communication over the network.

One notable example of a subsystem of Riak relying on cluster metadata
is Riak's [bucket types]({{<baseurl>}}riak/kv/2.2.3/using/reference/bucket-types) feature. This feature
requires that a particular form of key/value pairs, namely bucket type
names (the key) and their associated bucket properties (the value), be
asynchronously broadcast to all nodes in a Riak cluster.

Though it is different in crucial respects,
[etcd](https://coreos.com/docs/cluster-management/setup/getting-started-with-etcd/)
is a roughly analogous cluster metadata key/value store developed for
use in [CoreOS](https://coreos.com/) clusters.

## How Cluster Metadata Works

Cluster metadata is different from other Riak data in two essential
respects:

1. Cluster metadata is intended only for internal Riak applications that
   require metadata shared on a system-wide basis. Regular stored data,
   on the other hand, is intended for use outside of Riak.
2. Because it is intended for use only by applications internal to Riak,
   cluster metadata can be accessed only internally, via the Erlang
   interface provided by the
   [`riak_core_metadata`](https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata.erl)
   module; it cannot be accessed externally via HTTP or Protocol Buffers.

The storage system backing cluster metadata is a simple key/value store
that is capable of asynchronously replicating information to all nodes
in a cluster when it is stored or modified. Writes require
acknowledgment from only a single node (equivalent to `w=1` in normal
Riak), while reads return values only from the local node (equivalent to
`r=1`). All updates are eventually consistent and propagated to all
nodes, including nodes that join the cluster after the update has
already reached all nodes in the previous set of members.

All cluster metadata is eventually stored both in memory and on disk,
but it should be noted that reads are only from memory, while writes are
made both to memory and to disk. Logical clocks, namely [dotted version vectors]({{<baseurl>}}riak/kv/2.2.3/learn/concepts/causal-context/#dotted-version-vectors), are used in place of [vector clocks]({{<baseurl>}}riak/kv/2.2.3/learn/concepts/causal-context/#vector-clocks) or timestamps to resolve value conflicts. Values stored as cluster metadata are opaque Erlang
terms addressed by both prefix and a key.

## Erlang Code Interface

If you'd like to use cluster metadata for an internal Riak application,
the Erlang interface is defined in the
[`riak_core_metadata`](https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata.erl)
module, which allows you to perform a variety of cluster metadata
operations, including retrieving, modifying, and deleting metadata and
iterating through metadata keys.
