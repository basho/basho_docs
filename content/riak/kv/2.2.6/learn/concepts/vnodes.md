---
title: "Vnodes"
description: ""
project: "riak_kv"
project_version: "2.2.6"
menu:
  riak_kv-2.2.6:
    name: "Vnodes"
    identifier: "learn_concepts_vnodes"
    weight: 109
    parent: "learn_concepts"
toc: true
aliases:
  - /riak/2.2.6/theory/concepts/vnodes
  - /riak/kv/2.2.6/theory/concepts/vnodes
---


[concept causal context]: {{<baseurl>}}riak/kv/2.2.6/learn/concepts/causal-context
[concept clusters ring]: {{<baseurl>}}riak/kv/2.2.6/learn/concepts/clusters/#the-ring
[concept replication]: {{<baseurl>}}riak/kv/2.2.6/learn/concepts/replication
[concept strong consistency]: {{<baseurl>}}riak/kv/2.2.6/learn/concepts/strong-consistency
[glossary node]: {{<baseurl>}}riak/kv/2.2.6/learn/glossary/#node
[glossary ring]: {{<baseurl>}}riak/kv/2.2.6/learn/glossary/#ring
[plan backend]: {{<baseurl>}}riak/kv/2.2.6/setup/planning/backend
[plan cluster capacity]: {{<baseurl>}}riak/kv/2.2.6/setup/planning/cluster-capacity
[use admin riak cli]: {{<baseurl>}}riak/kv/2.2.6/using/admin/riak-cli


Virtual nodes, more commonly referred to as **vnodes**, are processes
that manage partitions in the Riak [ring][glossary ring]. Each data
partition in a Riak cluster has a vnode that **claims** that partition.
Vnodes perform a wide variety of operations, from K/V storage operations
to guaranteeing [strong consistency][concept strong consistency] if you choose to use that
feature.

## The Number of Vnodes in a Cluster

The term [node][glossary node] refers to a full instance of Riak,
be it on its own physical machine or alongside others on a single
machine, as in a development cluster on your laptop. Each Riak node
contains multiple vnodes. The number per node is the [ring
size][concept clusters ring] divided by the number of nodes in the cluster.

This means that in some clusters different nodes will have different
numbers of data partitions (and hence a different number of vnodes),
because (ring size / number of nodes) will not produce an even integer.
If the ring size of your cluster is 64 and you are running three nodes,
two of your nodes will have 21 vnodes, while the third node holds 22
vnodes.

The output of the [`riak-admin member-status`][use admin riak cli]
command shows this:

```
================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
valid      34.4%      --      'dev1@127.0.0.1'
valid      32.8%      --      'dev2@127.0.0.1'
valid      32.8%      --      'dev3@127.0.0.1'
-------------------------------------------------------------------------------
Valid: 3 / Leaving:0 / Exiting:0 / Joining:0 / Down:0
```

In this cluster, one node accounts for 34.4% of the ring, i.e. 22 out of
64 partitions, while the other two nodes account for 32.8%, i.e. 21 out
of 64 partitions. This is normal and expected behavior in Riak.

We strongly recommend setting the appropriate ring size, and by
extension the number of vnodes, prior to building a cluster. A full
guide can be found in our [cluster planning][plan cluster capacity] documentation.

## The Role of Vnodes

Vnodes essentially watch over a designated subset of a cluster's key
space. Riak computes a 160-bit binary hash of each bucket/key pair and
maps this value to a position on an ordered [ring][concept clusters ring]
of all such values. The illustration below provides a visual
representation of the Riak ring:

![The Riak
Ring]({{<baseurl>}}images/shared/riak-ring.png)

You can think of vnodes as managers, responsible for handling incoming
requests from other nodes/vnodes, storing objects in the appropriate
storage backend, fetching objects from backends, interpreting [causal
context][concept causal context] metadata for objects, acting as [strong consistency
ensembles][concept strong consistency] and much
more.  At the system level, vnodes are Erlang processes build on top of
the [`gen_fsm`](http://www.erlang.org/doc/design_principles/fsm.html)
abstraction in Erlang, i.e. you can think of vnodes as **finite state
machines** that are constantly at work ensuring that Riak's key
goals---high availability, fault tolerance, etc.---are guaranteed for
their allotted portion of the cluster's key space. Whereas nodes are
essentially a passive container for a wide variety of Riak processes,
vnodes are the true workhorses of Riak.

While each vnode has a main Erlang process undergirding it, vnodes may
also spawn new worker processes (i.e. new Erlang actors) to perform
asynchronous tasks on behalf of the vnode.

If you're navigating through the file system of a Riak node, you'll
notice that each node's `/data` directory holds a variety of
subdirectories. If you're using, say, [Bitcask]({{<baseurl>}}riak/kv/2.2.6/setup/planning/backend/bitcask) as a backend, navigate
into the `/bitcask` directory (you'll also see a `/ring` directory and
several others). If you open up the `/bitcask` directory, you'll see a
wide assortment of directories with numbers as names, e.g. `0` or
`1004782375664995756265033322492444576013453623296`. These directories
each house the data from a particular partition.

## Vnodes and Replication Properties

In our documentation on [replication properties][concept replication], we make frequent
mention of users' ability to choose how many nodes store copies of
data, how many nodes must respond for a read request to succeed, and so
on. This is slightly misleading, as the fundamental units of replication
are not nodes but rather vnodes.

This can be illustrated by way of a potential user error.  If you store
an object and set N=5, this means that you want the object to be stored
on 5 different nodes. But imagine that your cluster only has 3 nodes.
Setting N=5 on a 3-node cluster is actually just fine. The data will be
managed by 5 vnodes, but some of that data may end up being stored more
than once on different nodes. A likely scenario is that two nodes will
store two copies of the data a piece, while the third node will store
only one. Absent such an error, however, nodes will not contain multiple
vnodes responsible for the same partition.

## Vnode Status

You can check the current status of all vnodes in your cluster using the
[`riak-admin vnode-status`][use admin riak cli]
command. When you run that command, you will see a series of reports on
each of the vnodes active on the local node. The output of this command
consists of a series of reports on each active vnode. The report for a
specific vnode should look something like this:

```
VNode: 1278813932664540053428224228626747642198940975104
Backend: riak_kv_bitcask_backend
Status:
[{key_count, 275},
 {status,[{"./data/bitcask/1278813932664540053428224228626747642198940975104/2.bitcask.data",
           0,0,335}]}]
Status:
{vnodeid,<<"ÅR±\vi80\f">>}
```

The meaning of each field is given in the table below.

Field | Description
:-----|:-----------
`VNode` | The ID of the vnode in question
`Backend` | The storage [backend][plan backend] utilized by the vnode
`Status` | The number of keys managed by the vnode and the file where the vnode stores its data. The other information can be ignored.
