---
title: "Strong Consistency Reference"
description: ""
project: "riak_kv"
project_version: "2.2.2"
menu:
  riak_kv-2.2.2:
    name: "Strong Consistency"
    identifier: "managing_ref_strong_consistency"
    weight: 112
    parent: "managing_ref"
toc: true
---

[usage bucket types]: {{<baseurl>}}riak/kv/2.2.2/developing/usage/bucket-types
[concept eventual consistency]: {{<baseurl>}}riak/kv/2.2.2/learn/concepts/eventual-consistency

Riak was originally designed as an [eventually consistent]({{<baseurl>}}riak/kv/2.2.2/learn/concepts/eventual-consistency) system, fundamentally geared toward providing partition
(i.e. fault) tolerance and high read and write availability.

While this focus on high availability is a great fit for many data
storage needs, there are also many use cases for which strong data
consistency is more important than availability. Basho introduced a new
strong consistency option in version 2.0 to address these use cases.
In Riak, strong consistency is applied [using bucket types][usage bucket types], which
enables developers to apply strong consistency guarantees on a per-key
basis.

Elsewhere in the documentation there are instructions for [enabling and using]({{<baseurl>}}riak/kv/2.2.2/developing/app-guide/strong-consistency/) strong consistency, as well as a [guide for operators]({{<baseurl>}}riak/kv/2.2.2/configuring/strong-consistency) looking to manage,
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

## Making the Strong vs. Eventual Decision

The first system described above may sound like the undisputed champion,
and the second system undesirable. However:

1. Reads and writes on the first system will often be slower---if only
   by a few milliseconds---because the system needs to manage reads and
   writes more carefully. If performance is of primary concern, the
   first system might not be worth the sacrifice.
2. Reads and writes on the first system may fail entirely if enough
   servers are unavailable. If high availability is the top priority,
   then the second system has a significant advantage.

So when deciding whether to use strong consistency in Riak, the
following question needs to be asked:

#### For the specific use case at hand, is it better for reads to fail than to return a potentially out-of-date value?

If the answer is yes, then you should seriously consider using Riak in a
strongly consistent way for the data that demands it, while bearing in
mind that other data can still be stored in Riak in an eventually
consistent way.

## Trade-offs

Using Riak in a strongly consistent fashion comes with two unavoidable
trade-offs:

1. Less availability
2. Slightly slower performance

Strongly consistent operations are necessarily less highly available
than eventually consistent operations because they require a **quorum**
of available object replicas to succeed. Quorum is defined as N / 2 + 1,
or `n_val` / 2 + 1. If N is set to 7, at least 4 object replicas must be
available, 2 must be available if N=3, etc.

If there is a network partition that leaves less than a quorum of object
replicas available within an ensemble, strongly consistent operations
against the keys managed by that ensemble will fail.

Nonetheless, consistent operations do provide a great deal of fault
tolerance. Consistent operations can still succeed when a minority of
replicas in each ensemble can be offline, faulty, or unreachable. In
other words, **strongly consistent operations will succeed as long as
quorum is maintained**. A fuller discussion can be found in the
[operations]({{<baseurl>}}riak/kv/2.2.2/configuring/strong-consistency/#fault-tolerance)
documentation.

A second trade-off regards performance. Riak's implementation of strong
consistency involves a complex [consensus subsystem]({{<baseurl>}}riak/kv/2.2.2/using/reference/strong-consistency/#implementation-details) that typically requires more communication between Riak nodes than eventually consistent operations,
which can entail a performance hit of varying proportions, depending on
a variety of factors.

Ways to address this issue can be found in [strong consistency and performance]({{<baseurl>}}riak/kv/2.2.2/configuring/strong-consistency/#performance).
