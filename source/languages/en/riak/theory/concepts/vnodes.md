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


If you're navigating through the file system of a Riak node, you'll
notice that each node's `/data` directory holds a variety of
subdirectories. If you're using, say, [[Bitcask]] as a backend, navigate
into the `/bitcask` directory (you'll also see a `/ring` directory and
several others). If you open up the `/bitcask` directory, you'll see a
wide assortment of directories with numbers as names, e.g. `0` or
`1004782375664995756265033322492444576013453623296`. These directories
each house the data from a particular partition.

## Vnodes and Partitioning

1:1 correspondence between vnodes and partitions; there are always
`ring_size` vnodes in a cluster
