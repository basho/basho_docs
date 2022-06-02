---
title: "Active Anti-Entropy"
description: ""
project: "riak_kv"
project_version: 2.9.4
menu:
  riak_kv-2.9.4:
    name: "Active Anti-Entropy"
    identifier: "learn_concepts_aae"
    weight: 100
    parent: "learn_concepts"
toc: true
aliases:
  - /riak/2.9.4/theory/concepts/aae
  - /riak/kv/2.9.4/theory/concepts/aae
---

[cluster ops v3 mdc]: {{<baseurl>}}riak/kv/2.9.4/using/cluster-operations/v3-multi-datacenter
[cluster ops aae]: {{<baseurl>}}riak/kv/2.9.4/using/cluster-operations/active-anti-entropy
[concept clusters]: {{<baseurl>}}riak/kv/2.9.4/learn/concepts/clusters
[concept eventual consistency]: {{<baseurl>}}riak/kv/2.9.4/learn/concepts/eventual-consistency
[config aae]: {{<baseurl>}}riak/kv/2.9.4/configuring/reference/#active-anti-entropy
[glossary read rep]: {{<baseurl>}}riak/kv/2.9.4/learn/glossary/#read-repair
[glossary vnode]: {{<baseurl>}}riak/kv/2.9.4/learn/glossary/#vnode
[Merkle tree]: http://en.wikipedia.org/wiki/Merkle_tree
[usage search]: {{<baseurl>}}riak/kv/2.9.4/developing/usage/search


In a [clustered][concept clusters], [eventually consistent][concept eventual consistency] system like Riak, conflicts between object replicas stored
on different nodes are an expected byproduct of node failure, concurrent
client updates, physical data loss and corruption, and other events that
distributed systems are built to handle. These conflicts occur when
objects are either

* **missing**, as when one node holds a replica of the object and
  another node does not, or
* **divergent**, as when the values of an existing object differ across
  nodes.

Riak KV offers two means of resolving object conflicts: read repair and
active anti-entropy (AAE). Both of these conflict resolution mechanisms
apply both to normal key/value data in Riak as well as to
[search indexes][usage search]


## Read Repair vs. Active Anti-Entropy

In versions of Riak prior to 1.3, replica conflicts were healed via
[read repair][glossary read rep] which is a _passive_
anti-entropy mechanism that heals object conflicts only when a read
request reaches Riak from a client. Under read repair, if the
[vnode][glossary vnode] coordinating the read request determines
that different nodes hold divergent values for the object, the repair
process will be set in motion.

One advantage of using read repair alone is that it doesn't require any
kind of background process to take effect, which can cut down on CPU
resource usage. The drawback of the read repair-only approach, however,
is that the healing process only can only ever reach those objects that
are read by clients. Any conflicts in objects that are not read by
clients will go undetected.

The _active_ anti-entropy (AAE) subsystem was added to Riak in
versions 1.3 and later to enable conflict resolution to run as a
continuous background process, in contrast with read repair, which does
not run continuously. AAE is most useful in clusters containing so-
called "cold data" that may not be read for long periods of time, even
months or years, and is thus not reachable by read repair.

Although AAE is enabled by default, it can be turned off if necessary.
See our documentation on [managing active anti-entropy][cluster ops aae]
for information on how to enable and disable AAE, as well as on configuring
and monitoring AAE.

## Active Anti-Entropy and Hash Tree Exchange

In order to compare object values between replicas without using more
resources than necessary, Riak relies on [Merkle
tree] hash exchanges between
nodes.

Using this type of exchange enables Riak to compare a balanced tree of
Riak object hashes. Any difference at a higher level in the hierarchy
means that at least one value has changed at a lower level. AAE
recursively compares the tree, level by level, until it pinpoints exact
values with a difference between nodes. The result is that AAE is able
to run repair operations efficiently regardless of how many objects are
stored in a cluster, since it need only repair specific objects instead
of all objects.

In contrast with related systems, Riak uses persistent, on-disk hash
trees instead of in-memory hash trees. The advantages of this approach
are twofold:

* Riak can run AAE operations with a minimal impact on memory usage
* Riak nodes can be restarted without needing to rebuild hash trees

In addition, hash trees are updated in real time as new writes come in,
which reduces the time that it takes to detect and repair missing or
divergent replicas.

As an additional fallback measure, Riak periodically clears and
regenerates all hash trees from on-disk key/value data, which enables
Riak to detect silent data corruption to on-disk data arising from disk
failure, faulty hardware, and other sources. The default time period for
this regeneration is one week, but this can be adjusted in each node's
[configuration file][config aae].

