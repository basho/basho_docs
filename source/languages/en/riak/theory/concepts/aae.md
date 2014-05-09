---
title: Active Anti-Entropy
project: riak
version: 1.4.8+
document: appendix
audience: intermediate
keywords: [aae, active anti-entropy]
---

In a [[clustered|Clusters]], [[eventually consistent|Eventual
Consistency]] system like Riak, conflicts between object replicas stored
on different nodes are an expected byproduct of node failure, concurrent
client updates, physical data loss and corruption, and other events that
distributed systems are built to handle. These conflicts occur when
objects are:

* **missing**, as when one node holds a replica of the object and another node does not, or
* **divergent**, as when the values of an existing object differ across nodes

Riak offers two means of resolving object conflicts: read repair and
active anti-entropy.

## Read Repair vs. AAE

In versions of Riak prior to 1.3, replica conflicts were were healed via
[[read repair|Riak Glossary#read-repair]] alone, which is a _passive_
anti-entropy mechanism that heals object conflicts only when a read
request reaches Riak from a client. If the [[vnode|Riak Glossary#vnode]]
coordinating the read request determines that different nodes hold
divergent values for the object, the repair process will be set in
motion.

One advantage of using read repair alone is that it is less expensive
for CPU and network resources. The drawback of this approach, however,
is that the healing process only reaches those objects that are read by
clients.

The _active_ anti-entropy (AAE) subsystem was added to Riak in
versions 1.3 and later to enable conflict resolution to run as a
continuous background process, in contrast with read repair, which does
not run continuously. AAE is most useful in clusters containing so-
called "cold data" that may not be read for long periods of time, even
months or years, and is thus not reachable by read repair.

Although AAE is enabled by default, it can be turned off if necessary.

## AAE and Scalability

In order to compare object values between replicas without using more
resources than necessary, Riak relies on [Merkle tree](http://en.wikipedia.org/wiki/Merkle_tree)
hash exchanges between nodes. With this system, the amount of
information that must be exchanged across nodes during repair AAE
operations is proportional to how large the differences between the
replicas are.

This enables Riak to compare hashes of values instead of
the entire value of objects, which ensures that roughly the same amount
of information is exchanged when there are 10 differing replicas out of
1 million keys as when there are 10 differing replicas out of 1 billion
keys. The result is that AAE is able to efficiently run repair
operations regardless of how many objects are stored in a cluster.

In contrast with related systems, Riak uses persistent, on-disk hash
trees instead of in-memory hash trees. The advantages of this approach
are twofold:

* Riak can run AAE operations with a minimal impact on memory usage
* Riak nodes can be restarted without needing to rebuild hash trees

In addition, hash trees are updated in real time as new writes come in,
which reduces the time that it takes for detect and repair missing or
divergent replicas. And as an additional fallback measure, Riak
periodically clears and regenerates all hash trees from on-disk key/
value data, which enables Riak to detect silent data corruption to on-
disk data arising from disk failure, faulty hardware, and other sources.
The default time period for this regeneration is one week, but this can
be adjusted in each node's [[configuration file|Configuration Files#active-anti-entropy]].

## Resources

* [Active Anti-Entropy video](http://coffee.jtuple.com/video/AAE.html) by Basho engineer [Joseph Blomstedt](https://github.com/jtuple)

