---
title: Strong Consistency
project: riak
version: 2.0.0+
document: appendix
audience: intermediate
keyword: [appendix, concepts]
---

Riak was originally designed as an [[eventually consistent|Eventual
Consistency]] system, fundamentally geared toward providing partition
(i.e. fault) tolerance and high read and write availability.

While this focus on high availability is a great fit for many data
storage needs, there are also many use cases for which strong data
consistency is more important than availability. Basho introduced a new
strong consistency option in version 2.0 to address these use cases,
enabling developers to apply strong consistency on a per-key, i.e.
per-object, basis.

## Strong vs. Eventual Consistency

If you successfully write a value to a key in a strongly consistent
system, the next successful read of that key is guaranteed to show that
write. A client will never see out-of-date values. The drawback is that
some writes may fail if a node is unreachable.

In an eventually consistent system, on the other hand, a read may return
an out-of-date value, particularly during system or network failures.
The advantage of this approach is that reads and writes can succeed even
when a cluster is experiencing significant service degradation.

### Example

Building on the example presented in the [[eventual consistency]] doc,
imagine that information about who manages Manchester United is stored
in Riak, in the key `manchester-manager`. In the eventual consistency
example, the value associated with this key was originally `Alex
Ferguson`, meaning that that was the first successful write to that key.
But then `David Moyes` became Man U's manager, and a write was executed
to change the value of `manchester-manager`.

Now imagine that this write failed on one node in a multi-node cluster.
Thus, all nodes report that the value of `manchester-manager` is `David
Moyes` except for one. On the errant node, the value of the
`manchester-manager` key is still `Alex Ferguson`. An eventually
consistent system is one in which a get request will most likely return
`David Moyes` but could return the outdated value `Alex Ferguson`.

In a strongly consistent system, conversely, any successful read on
`manchester-manager` will return `David Moyes` and never `Alex
Ferguson`. Reads will return `David Moyes` every single time until Man U
gets a new manager and someone performs a successful write to
`manchester-manager` to change its value.

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
2. Nearly all reads to `k` return `v`, but a small percentage return `not found`
3. A write to `k` changes the value to `v2`
4. Nearly all reads to `k` now return `v2`, but a small number return the outdated `v` (or even `not found`) because the newer value hasn't yet been replicated to all nodes

## Making the Strong vs. Eventual Decision

The first system described above may sound like the undisputed champion,
and the second system undesirable. However:

1. Reads and writes on the first system will often be slower---if only by a few milliseconds---because the system needs to manage reads and writes more carefully. If performance is of primary concern, the first system might not be worth the sacrifice.
2. Reads and writes on the first system may fail entirely if enough servers are unavailable. If high availability is the top priority, then the second system has a significant advantage.

So when deciding whether to use strong consistency in Riak, the
following question needs to be asked: _for the specific use case at
hand, is it better for reads to fail than to return a potentially
out-of-date value?_ If the answer is yes, then you should seriously
consider using Riak in a strongly consistent way for the data that
demands it, while bearing in mind that other data can still be stored in
Riak in an eventually consistent way.

## Strong Consistency in Riak

In Riak, strong consistency is applied [[using bucket types]], which
enables you to specify which objects in your cluster will have strong
consistency guarantees applied to them. In strongly consistent buckets,
there are four types of atomic operations on objects:

* **Get** operations work just as they do against non-strongly-consistent keys, but with two crucial differences:
  1. connecting clients are guaranteed to return the most recently written value
  2. reads on strongly consistent keys *never* return siblings
* **Conditional put** operations write an object only if no object currently exists in that key. The operation will fail if the key already exists; if the key was never written or has been deleted, the operation succeeds.
* **Conditional modify** operations are compare-and-swap (CAS) operations that succeed only if the value of a key has not changed since it was previously read.
* **Delete** operations work just as they do against non-strongly-consistent keys.

From the standpoint of clients connecting to Riak, there is no
difference between strongly and non-strongly consistent data. The
operations performed on objects---reads, writes, deletes, etc.---are the
same, which means that the client API for strong consistency is exactly
the same.

## Implementation Details

Strong consistency in Riak is handled by a subsystem called
[`riak_ensemble`](https://github.com/basho/riak_ensemble/tree/feature/add-docs/doc).
Documentation on enabling it is found in [[Using Strong
Consistency|Using Strong Consistency#Enabling-Strong-Consistency]].

There are a few formal models for provable strong consistency in a
distributed system. Riak uses multi-Paxos, adapted for the database's
key/value architecture. The salient point for developers is that the
metadata field historically used for causal consistency in Riak,
[[vector clocks]], is repurposed as a context for strongly-consistent
writes. If a value exists in Riak, **it cannot be modified unless the
most recent context value is supplied with a write operation**.

Thus, likely outcomes from a write:

* A quorum of servers that own the key are not available, thus the write fails.
* The key does not exist: any write is successful.
* The key does exist:
    * All writes that supply no context fail.
    * All writes that supply an out-of-date context fail.
    * Any write with the most recent context succeeds, and any future writers must read the new context to be able to successfully write a newer value.

## Important Caveats

The following Riak features are not currently available in strongly
consistent buckets:

* [[Secondary indexes|Using Secondary Indexes]]
* [[Riak Data Types|Using Data Types]]

Strongly-consistent writes operate only on single keys. There is
currently no support within Riak for strongly consistent operations
against multiple keys, although it is always possible to incorporate
write and read locks in an application that uses strongly-consistent
Riak.
