---
title: Vnodes
project: riak
version: 2.0.0+
document: appendix
audience: intermediate
keywords: [appendix, concepts, vnodes, partition]
---

Virtual nodes, typically referred to as **vnodes**, are responsible for
claiming a partition in the Riak [[ring|Clusters#The-Ring]]. Each
physical machine, i.e. **node**, in the cluster contains many vnodes.
The number of vnodes per node is determined by the total number of
vnodes and the number of active physical nodes in the cluster. Riak
automatically balances the assignment of vnodes across the physical
nodes in the cluster.

## The Role of Vnodes

Vnodes essentially watch over a designated subset of a cluster's key
space. Riak computes a 160-bit binary hash of each bucket/key pair and
maps this value to a position on an ordered [[ring|Clusters#The-Ring]]
of all such values.

You can think of vnodes as managers, responsible for storing objects,
fetching objects, interpreting [[causal context]] metadata for objects,
and much more. Vnodes are Erlang processes build on top of the
[`gen_fsm`](http://www.erlang.org/doc/design_principles/fsm.html)
abstraction in Erlang, i.e. you can think of vnodes as **finite state
machines**.
Vnodes handle incoming requests; the fundamental unit of concurrency,
replication, and fault tolerance
Each node has a master vnode responsible for keeping track of all active
vnodes on the node
"workhorse" of the `riak_core` that undergirds a great deal of Riak's
functionality; nodes are essentially a passive container for very active
vnodes

It's important to note that different nodes will have differing numbers
of vnodes if ring size / number of nodes leaves a remainder. So if your
ring size is 64 and you're running a 5-node cluster, 4 of those nodes
will have 13 vnodes, while one node will have only 12, which means that
4 nodes will claim roughly 20.3% of the ring a piece while the fifth node
will claim roughly 18.8%.


If you're navigating through the file system of a Riak node, you'll
notice that each node's `/data` directory holds a variety of
subdirectories. If you're using, say, [[Bitcask]] as a backend, navigate
into the `/bitcask` directory (you'll also see a `/ring` directory and
several others). If you open up the `/bitcask` directory, you'll see a
wide assortment of directories with numbers as names, e.g. `0` or
`1004782375664995756265033322492444576013453623296`. These directories
each house the data from a particular partition.

## Vnodes and Partitioning

Consistent hashing -> distribution of the key space

The relationship between vnodes and the Riak ring is simple: there will
always be a 1:1 correspondence between the number of vnodes and the size
of the ring. A ring size of 64 means 64 vnodes, 128 means 128, and so
on. At all times, the different nodes in the cluster are aware of which
nodes own which vnodes, which is essentialy to processes like [[hinted
handoff|Riak Glossary#Hinted-Handoff]] and, by extension, to Riak's
high availability architecture.

## Vnodes and Replication Properties

In our documentation on [[replication properties]], we make frequent
mention of users' ability to choose how many nodes store copies of
data, how many nodes must respond for a read request to succeed, and so
on. This is slightly misleading, as the fundamental units of replication
are not nodes but rather vnodes.

This can be illustrated by way of a potential user error.
If you store an object and set N=5, this means that you want the object
to be stored on 5 different nodes. But imagine that your cluster only
has 3 nodes. Setting N=5 on a 3-node cluster is actually just fine. The
data will be managed by 5 vnodes, but some of that data may end up being
stored more than once on different nodes. A likely scenario is that two
nodes will store two copies of the data a piece, while the third node
will store only one.

## Notes

* Think of vnodes as long-running _processes_
* Vnodes are actually an interface that is exposed so that any
    applications built on top of Riak can implement; an example is
    `riak_ensemble`; there is a definition of what a vnode needs to be
    able to do, and developers inside or outside of Basho have access to
    the vnode abstraction, and thus to much of the core bits of Riak;
    more specifically, the vnode interface dictates that there is a set
    of commands that each vnode needs to be able to complete, and then
    each application that uses the vnode abstraction has to specify what
    those commands are
* What vnodes do _not_ need to do is distribute load to themselves via
    hinted handoff, assign themselves portions of the ring, etc.; those
    tasks are handled by the `riak_core_vnode_manager`
    ([link](https://github.com/basho/riak_core/blob/master/src/riak_core_vnode_manager.erl)),
    which
* While each vnode has a main Erlang process undergirding it, vnodes may
    also spin up new worker processes (actors in Erlang) to perform
    asynchronous tasks on behalf of the vnode
* You'll see many references in our documentation to vnodes having a
    _claim_ on a data partition
* Riak has an experimental [[ring resizing]] feature that enables you to
    change the ring size of a cluster, and hence to change the number of
    data partitions/vnodes, but we do not yet recommend using this
    feature.

## The Vnode Interface

The code is [on
GitHub](https://github.com/basho/riak_core/blob/develop/src/riak_core_vnode.erl).
To implement a vnode for a specific use/application, you need to define
a series of callback functions, e.g.
[`init`](https://github.com/basho/riak_core/blob/develop/src/riak_core_vnode.erl#L71),

## Vnode Status

You can check the current status of all vnodes in your cluster using the
`[[riak-admin vnode-status|riak-admin Command Line#vnode-status]]`
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
`VNode` | The ID of the vnode in question, as calculated based on the 
