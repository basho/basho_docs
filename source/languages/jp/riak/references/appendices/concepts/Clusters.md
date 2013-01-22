---
title: Clusters
project: riak
version: 0.10.0+
document: appendix
audience: intermediate
keywords: [appendix, concepts]
---

Riak's default mode of operation is in a cluster. A Riak cluster is
generally run on a set of well-connected physical hosts.  Each host in
the cluster runs one Riak node.  Each Riak node runs a set of virtual
nodes, or "vnodes", that are each responsible for storing a separate
portion of the key space.

Nodes are not clones of each other, nor do they all participate in
fulfilling every request.  The extent to which data is replicated, and
when, and with what merge strategy and failure model, is configurable
at runtime.

## The Ring

Much of this section is discussed in the Dynamo paper, but it's a good
summary of how Riak implements the necessities.

Riak's client interface speaks of [[buckets|Buckets]] and
[[keys|Keys and Objects]].  Internally, Riak computes a 160-bit binary
hash of the bucket/key pair, and maps this value to a position on an
ordered "ring" of all such values. This ring is divided into
partitions.  Each Riak vnode is responsible for a partition (we say
that it "claims" that partition).

The nodes of a Riak cluster each attempt to run roughly an equal
number of vnodes.  In the general case, this means that each node in
the cluster is responsible for 1/(number of nodes) of the ring, or
(number of partitions)/(number of nodes) vnodes.  For example, if two
nodes define a 16-partition cluster, then each node will run 8
vnodes. Nodes attempt to claim their partitions at intervals around
the ring such that there is even distribution amongst the member nodes
and that no node is responsible for more than one replica of a key.

When a value is being stored in the cluster, any node may participate
as the coordinator for the request.  The coordinating node consults
the ring state to determine which vnode owns the partition in which
the value's key belongs, then sends the "put" request to that vnode,
as well as the vnodes responsible for the next N-1 partitions in the
ring, where N is a bucket-configurable parameter that describes how
many copies of the value to store.  The put request may also specify
that at least W (=< N) of those vnodes reply with success, and that DW
(=< W) reply with success only after durably storing the value.

A fetch, or "get", request operates similarly, sending requests to the
vnode that "claims" the partition in which the key resides, as well as
to the next N-1 partitions.  The request also specifies R (=< N), the
number of vnodes that must reply before a response is returned.

## Gossiping

The ring state is shared around the cluster by means of a "gossip
protocol".  Whenever a node changes its claim on the ring, it
announces its change via this protocol.  It also periodically
re-announces what it knows about the ring, in case any nodes missed
previous updates.
