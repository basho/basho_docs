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


