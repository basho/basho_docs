---
title: Strong Consistency
project: riak
version: 2.0.0+
document: appendix
audience: intermediate
keyword: [appendix, concepts]
---

Riak was originally designed as an [[eventually consistent|Eventual Consistency]] system, fundamentally geared toward providing partition (i.e. fault) tolerance and high read and write availability.

While a system that prioritizes A and P, to borrow the language of the [CAP theorem](http://en.wikipedia.org/wiki/CAP_theorem) is a great fit for many data storage needs, there are many use cases in which strong data consistency is more important than availability. It is with those use in cases in mind that a strong consistency feature was added to Riak in version 2.0.

A concern for consistency has always been a part of Riak, which has always enabled users to sacrifice availability in favor of stronger consistency if they wish by adjusting [[various read and write parameters|Eventual Consistency#Replication-properties-and-request-tuning]] to find a point on the continuum between AP and CP that fits their particular use cases.

Yet one option that was not available in versions prior to 2.0 was the option of going all the way and using Riak as a strict CP system. That has now changed. _In versions 2.0 or greater, Riak can be used as a_ strongly _rather than_ eventually _consistent system_.

## Strong vs. Eventual Consistency

If you successfully write a key in a strongly consistent system, the next successful read of that key is guaranteed to show that write. Writes that meet this criterion are known as [atomic updates](http://en.wikipedia.org/wiki/Atomicity_(database_systems)). Strong consistency means that a client simply never sees out-of-date values. The drawback is that some writes may fail if a node is unreachable.

In an eventually consistent system, on the other hand, a read may return an out-of-date value if the object is read from a yet-to-be-replicated node. The advantage of this approach is that data can remain highly available even when some nodes in the cluster are down.

### Example

Building on the example presented in the [[eventual consistency]] doc, imagine that information about who manages Manchester United is stored in Riak, in the key `manchester-manager`. In the eventual consistency example, the value associated with this key was originally `Alex Ferguson`, meaning that that was the first successful write to that key. But then `David Moyes` became Man U's manager, and somebody performed a write to change the value of `manchester-manager`.

Now imagine that this write was only successful on one node in a multi-node cluster. The problem then arises that all nodes say that the value of `manchester-manager` is `David Moyes` except for one. In the errant node, the value of the `manchester-manager` key is still `Alex Ferguson`, which is outdated. An eventually consistent system is one in which a get request will most likely return `David Moyes` but could return the outdated value `Alex Ferguson`.

In a strongly consistent system, conversely, any successful read on `manchester-manager` will return `David Moyes` and never `Alex Ferguson`. Reads will return `David Moyes` every single time until Man U gets a new manager and someone performs a successful write to `manchester-manager` and changes its value.

It might also be useful to imagine it a bit more abstractly. The following causal sequence would characterize a strongly consistent system:

* A write is made that sets the value of the key `k` to `v`
* All reads on `k` return `v` without fail
* The value of `k` is changed to `v2`
* Thereafter, all reads on `k` return `v2` without fail
* The value is changed to `v3`, at which point the above cycle simply repeats

At no point in time does this system return an out-of-date value.

The following sequence could characterize an eventually consistent system:

* A write is made that sets the value of the key `k` to `v`
* Nearly all reads to `k` return `v`, but a small percentage return `not found`
* A write to `k` changes the value to `v2`
* Nearly all reads to `k` now return `v2`, but a small number return the outdated `v` because the newer value hasn't yet been replicated to all nodes

## Making the Strong vs. Eventual Decision

The first system described above may sound like the undisputed champion, and the second system undesirable. But there are two things need to be borne in mind before that judgment is made:

1. Reads and writes on the first system will often be slower---if only by a few milliseconds---because the system needs to utilize some kind of internal system to ensure that reads return only the most up-to-date value. If performance is of primary concern, the first system might not be worth the sacrifice.
2. For many use cases, a slightly outdated value might be both "good enough" and very much worth the performance gains. It might be better to very quickly find out that Alex Ferguson is the manager of Manchester United than to have to wait a bit longer to find out that David Moyes is really the manager.

So when deciding whether to use strong consistency in Riak, the following question needs to be asked: _for the specific use case at hand, is it better for get requests to fail than to return a potentially out-of-date value?_ If the answer is yes, then you should seriously consider using Riak in a strongly consistent way for the data that demands it, while bearing in mind that other data can still be stored in Riak in an eventually consistent way.

## Riak's Mixed Approach

Riak has always been a key/value store, but it is different from others in that it has always required that key/value pairs be stored in namespaces called [[buckets|The Basics#Bucket-Properties-and-Operations]].

The advantage of providing multiple namespaces for keys---as many namespaces as you wish---is that it enables users to fine-tune the availability/consistency trade-off on a bucket-by-bucket basis. Users can set some buckets to accept sloppy quorums, others with `w` and/or `r` equal to `n_val`, and so on, allowing for a mix-and-match approach to data within a Riak cluster.

This mixed approach is still possible, except that strong consistency has been introduced as yet another available bucket-level configuration. As of Riak 2.0, buckets have a property called `consistent`, which, if set to `true`, makes data in that bucket conform to strong consistency requirements. Implementation details can be found in the [[Using Strong Consistency]] tutorial.

## How Riak Implements Strong Consistency

Strong consistency in Riak is handled by a subsystem called [`riak_ensemble`](https://github.com/basho/riak_ensemble). When this subsystem is enabled---more on that in the [[Using Strong Consistency|Using Strong Consistency#Enabling-Strong-Consistency]] doc---all operations performed on strongly consistent are placed on a special code path that is separate from the path that handles eventually consistent data.

There are many conceivable ways of implementing strong consistency. Riak's approach involves the use of **[dotted version vectors](http://paginas.fe.up.pt/~prodei/dsie12/papers/paper_19.pdf)** (DVVs), which are attached as metadata to every object stored in strongly consistent buckets. DVVs are similar to [[vclocks]] in that they record logical rather than chronological causality (which makes them very different from timestamps). DVVs are used by Riak to ensure **recency**, i.e. that any read will see the most recent successful write.

This can be illustrated using the following hypothetical scenario:

* A client attempts a write to store the value `new_val` in the key `k` in a strongly consistent bucket (the write is simple because the client doesn't pass along any state to Riak).
* If `k` is empty at the time of the write, the write proceeds normally.
* If, however, there is already a value `old_val` stored in the key `k`, the write will fail if it is a simple write that doesn't inform Riak which version of the value the client has seen.
* In order for that write to succeed, the client needs to pass a DVV to Riak that tells Riak, in effect, "this is the most recent value that I've seen." If the client has not seen most recent successful write, the write will simply fail.
 
This is the basic mechanism used to solve the problem of concurrent writes. If a client hasn't yet seen the most recent write, then that client will be essentially locked out of writes to that key. In eventually consistent Riak, the system is always open to writes; with strong consistency, this locking out leads to a temporary loss of write availability.

The most important thing to note is that simple writee---i.e. writes that don't pass a DVV to Riak---will only ever succeed if a key does not already exist. If a key already exists, Riak will have to see a DVV. This makes Riak's strong consistency implementation fundamentally **state based**: DVVs are the system "state" used by Riak to make determinate judgments about which values are most recent. 

An important thing to note here is that only successful writes are guaranteed to be seen, not just any writes. If a simple write is attempted on a KV pair that does not yet exist, it will succeed. However, if the write is attempted on KV pair that already exists, it will fail. Instead, a write must engage in a get/modify/put operation that increments the object's DVV and marks it as having been changed.

So what happens when values conflict, whether due to concurrent or partial writes or a node coming back into the network or some other reason? We can illustrate this with another hypothetical scenario:

* A client reads a key from a strongly consistent bucket. That key stores conlicting values in different nodes, A in one node and B in the other.
* On the basis of DVVs, either value A or B will be deemed logically "older" than the other. If A is selected as being logically older, it will "win" over B.
* All other nodes will be notified that A is the correct value.

Conflicts are thus sorted out at read time rather than write time. Now, every time a client reads the key in the example above, Riak will return A, and B is essentially discarded as a possible correct value.

For some use cases, the rollback to older values in cases of "doubt" and the occasional refusal of writes can be detrimental, especially for those use cases in which it's important to accept all writes unconditionally.

But for other use cases, a slightly outdated value is typically "good enough" and much worth the performance and availability boost that can be achieved using eventual consistency. Depending on your business needs, it may be better to run the risk that an application will occasionally report a slightly out-of-date value than to not be able to provide any value at all.

And so if data non-ambiguity is a primary concern, to the extent that it's worth occasionally (a) discarding logically newer values in favor of logically older ones and (b) losing write availability, then you might want to consider strong consistency for that portion of your data that truly requires it.

## Important Caveats

There are a few things that we should note about strong consistency as it is implemented in Riak. First of all, the following Riak features are not available in strongly consistent buckets:

* [[Secondary indexes]]
* [[Active Anti-Entropy]] syncing
* Riak [[Datatypes]]

Second of all, strong consistency can be guaranteed only at the single-key level. There is currently no support for strong consistency across keys or for multi-key operations.
