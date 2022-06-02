---
title: "Strong Consistency"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Strong Consistency"
    identifier: "learn_concepts_strong_consistency"
    weight: 109
    parent: "learn_concepts"
toc: true
aliases:
  - /riak/2.1.4/theory/concepts/strong-consistency
  - /riak/kv/2.1.4/theory/concepts/strong-consistency
---

[usage bucket types]: {{<baseurl>}}riak/kv/2.1.4/developing/usage/bucket-types
[concept eventual consistency]: {{<baseurl>}}riak/kv/2.1.4/learn/concepts/eventual-consistency

{{% note title="Please Note:" %}}
Riak KV's strong consistency is an experimental feature and may be removed
from the product in the future. Strong consistency is not commercially
supported or production-ready. Strong consistency is incompatible with
Multi-Datacenter Replication, Riak Search, Bitcask Expiration, LevelDB
Secondary Indexes, Riak Data Types and Commit Hooks. We do not recommend its
usage in any production environment.
{{% /note %}}

Riak was originally designed as an [eventually consistent]({{<baseurl>}}riak/kv/2.1.4/learn/concepts/eventual-consistency) system, fundamentally geared toward providing partition
(i.e. fault) tolerance and high read and write availability.

While this focus on high availability is a great fit for many data
storage needs, there are also many use cases for which strong data
consistency is more important than availability. Basho introduced a new
strong consistency option in version 2.0 to address these use cases.
In Riak, strong consistency is applied [using bucket types][usage bucket types], which
enables developers to apply strong consistency guarantees on a per-key
basis.

Elsewhere in the documentation there are instructions for [enabling and using]({{<baseurl>}}riak/kv/2.1.4/developing/app-guide/strong-consistency/) strong consistency, as well as a [guide for operators]({{<baseurl>}}riak/kv/2.1.4/configuring/strong-consistency) looking to manage,
configure, and monitor strong consistency.

## Strong vs. Eventual Consistency

If you successfully write a value to a key in a strongly consistent
system, the next successful read of that key is guaranteed to show that
write. A client will never see out-of-date values. The drawback is that
some operations may fail if an insufficient number of object replicas
are available. More on this in the section on [trade-offs](#trade-offs).

In an eventually consistent system, on the other hand, a read may return
an out-of-date value, particularly during system or network failures.
The advantage of this approach is that reads and writes can succeed even
when a cluster is experiencing significant service degradation.

### Example

Building on the example presented in the [eventual consistency][concept eventual consistency] doc,
imagine that information about who manages Manchester United is stored
in Riak, in the key `manchester-manager`. In the eventual consistency
example, the value associated with this key was originally
`David Moyes`, meaning that that was the first successful write to that
key. But then `Louis van Gaal` became Man U's manager, and a write was
executed to change the value of `manchester-manager`.

Now imagine that this write failed on one node in a multi-node cluster.
Thus, all nodes report that the value of `manchester-manager` is `Louis
van Gaal` except for one. On the errant node, the value of the
`manchester-manager` key is still `David Moyes`. An eventually
consistent system is one in which a get request will most likely return
`Louis van Gaal` but could return the outdated value `David Moyes`.

In a strongly consistent system, conversely, any successful read on
`manchester-manager` will return `Louis van Gaal` and never `David Moyes`.
Reads will return `Louis van Gaal` every single time until Man U gets a new
manager and someone performs a successful write to `manchester-manager`
to change its value.

It might also be useful to imagine it a bit more abstractly. The
following causal sequence would characterize a strongly consistent
system:

1. The value of the key `k` is set to `v`
2. All successful reads on `k` return `v`
3. The value of `k` is changed to `v2`
4. All successful reads on `k` return `v2`
5. And so forth

At no point in time does this system return an out-of-date value.

The following sequence could characterize an eventually consistent
system:

1. A write is made that sets the value of the key `k` to `v`
2. Nearly all reads to `k` return `v`, but a small percentage return
   `not found`
3. A write to `k` changes the value to `v2`
4. Nearly all reads to `k` now return `v2`, but a small number return
   the outdated `v` (or even `not found`) because the newer value hasn't
   yet been replicated to all nodes