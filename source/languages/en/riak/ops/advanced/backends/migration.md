---
title: Backend Migration
project: riak
version: 2.0.0+
document: tutorials
audience: advanced
keywords: [backends, migration]
---

Riak offers four storage backend options: [[Bitcask]] (the default),
[[LevelDB]], [[Memory]], and [[Multi]] (which enables you to configure
and use multiple backends in a single cluster).

Because each backend has its own strengths and weaknesses and because
not all features are available in all backends---e.g.
[[secondary indexes|Using Secondary Indexes]] are available only in
LevelDB, while object expiry is available only in Bitcask---it may be
necessary under certain conditions to migrate some or all of your data
from one storage backend to another.

## Basic Steps

1. Stand up an additional node _or_ `cluster leave` a single node
2. Configure the default backend on the unattached node
3. Start the new node and `cluster join`
4. Cluster replace <cluster member> <new node>
5. `cluster commit`
6. Wait for transfers to complete, meanwhile adjusting transfer limits as necessary. Riak will stop on the replaced node once the transfer is complete.
7. Return to step 2 and repeat for each other node in the cluster until all nodes have been replaced
8. If a node was removed in step 1 (instead of `cluster leave`-ing a node), rejoin that node now using `cluster join`