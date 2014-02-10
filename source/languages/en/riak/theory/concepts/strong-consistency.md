---
title: Strong Consistency
project: riak
version: 2.0.0+
document: appendix
audience: intermediate
keyword: [appendix, concepts]
---

Riak was originally designed as an [[eventually consistent|Eventual Consistency]] system, fundamentally geared toward providing partition (i.e. fault) tolerance and high availability---a prioritization that inevitably comes at the expense of data consistency. In the language of the [CAP theorem](http://en.wikipedia.org/wiki/CAP_theorem), Riak began as an AP---highly available, partition-tolerant---system, and it continues to be an AP system by default.

In spite of the limitations imposed by CAP, Riak has always enabled users to sacrifice availability in favor of stronger consistency if they wish by adjusting [[various read and write parameters|Eventual Consistency#Replication-properties-and-request-tuning]] to find a point on the continuum between AP and CP that fit particular use cases. Yet one option that was not available in versions prior to 2.0 was the option of using Riak as a full-on CP system.

That has now changed. _In versions 2.0 or greater, Riak can be used as a_ strongly _rather than_ eventually _consistent system_.

## Strong vs. Eventual Consistency

A data storage system guarantees strong consistency when it ensures that an object hasn't changed since you last read it. In other words, if you write a key, the next successful read of that key is _guaranteed_ to show that write. Writes that meet this criterium are known as **atomic updates**.

In an eventually consistent system, on the other hand, a get request on a key could return the value of the most recent successful put _or_ potentially an out-of-date value. Strong consistency means that you simply _never_ see out-of-date values (although the drawback is that some writes may fail).

To borrow the example used in our treatment of [[eventual consistency]], imagine that you are querying Riak to find out who currently manages Manchester United. In that example, the value associated with the key `manchester-manager` was originally `Alex Ferguson` (i.e. that was the first successful put request on that key). But then `David Moyes` became Man U's manager, and somebody ran a put request on the `manchester-manager` key to change its value to be more up to date.

Now imagine that this put request was only successful on one node in the cluster. The problem then arises that all nodes say that the value of `manchester-manager` is `David Moyes` except for one that says `Alex Ferguson`. An eventually consistent system is one in which a get request will _probably_ return `David Moyes`, but _could_ return the outdated value of `Alex Ferguson`.

In a strongly consistent system, conversely, any successful get request on `manchester-manager` will return `David Moyes` and never `Alex Ferguson`. And this will remain the case until Man U gets a new manager and there is a successful write to the `manchester-manager` key that changes the value.

It might be useful to imagine it more abstractly, in terms of causal sequences. The following sequence would characterize a strongly consistent system:

* A put request is made that sets the value of the key `k` to `v`
* All get requests on `k` return `v` until the value is changed to `v2`
* Thereafter, _all_ get requests on `k` return `v2` until the value is changed to `v3`, at which point the cycle repeats
* At no point in time in this system does a read return an out-of-date value

The following sequence, however, would characterize an _eventually_ consistent system:

* A put request is made that sets the value of the key `k` to `v`
* Nearly all get requests to `k` return `v`, but a small percentage return `not found`
* The value associated with the key `k` is updated to `v2`
* Nearly all get requests to `k` return `v2`, but a small number return the outdated `v` (and a tiny number return `not found`)

## Making the Strong vs. Eventual Decision

The first system may _sound_ like the undisputed champion, and the second system undesirable. But there are two things need to be borne in mind before that judgment is made:

* Reads and writes on the first system will often be slower---if only by a few milliseconds---because the system needs to utilize some kind of internal system to ensure, either at read time or write time, that reads return only the most up-to-date value. If performance is of major import, the first system might not be worth the sacrifice.
* For many use cases, a slightly outdated value might be both "good enough" and very much worth the performance gains. It might be better to very quickly find out that Alex Ferguson is the manager of Manchester United than to have to wait a bit longer to find out that David Moyes is really the manager.

So when deciding whether to use strong consistency in Riak, the following question needs to be asked: _for the specific use case at hand, is it better for get requests to fail than to return a potentially out-of-date value?_ If the answer is yes, then you should seriously consider using Riak in a strongly consistent way _for the data that demands it_, while bearing in mind that other data can still be stored in Riak in a non-strongly-consistent way.

## Riak's Mixed Approach

Riak has always been a key/value store, but it is different from others in that it has always required that key/value pairs be stored in namespaces called **buckets**.

The advantage of providing multiple namespaces---as many as you wish---is that it enables users to fine-tune the availability/consistency trade-off on a bucket-by-bucket basis. Users can set some buckets to accept sloppy quorums, others with `w` and/or `r` equal to `n_val`, and so on, allowing for a mix-and-match approach to data within a Riak cluster.

This mixed approach is still possible, of course, except that now strong consistency has been introduced as yet another available bucket-level configuration. As of Riak 2.0, buckets have a property called `consistent`, which, if set to `true`, makes data in that bucket conform to strong consistency requirements. Implementation details can be found in the [[Using Strong Consistency]] tutorial.

## How Riak Implements Strong Consistency

Strong consistency in Riak is handled by a subsystem called `[riak_ensemble](https://github.com/basho/riak_ensemble)`. When this subsystem is enabled---more on that in the [[Using Strong Consistency|Using Strong Consistency#Enabling-Strong-Consistency]] doc---all operations performed on buckets with the property `consistent` set to `true` are placed on a special code path that is separate from the path that handles eventually consistent data.

There are many conceivable ways of implementing strong consistency. Riak's approach involves the use of **[dotted version vectors](http://paginas.fe.up.pt/~prodei/dsie12/papers/paper_19.pdf)** (DVVs), which are attached as metadata to every object stored in strongly consistent buckets. DVVs are similar to [[vclocks]] in that they record _logical_ rather than chronological causality (which makes them quite different from timestamps). DVVs are used by Riak to ensure **recency**, i.e. that any read will see the most recent successful write.

This can be illustrated using the following hypothetical scenario:

* A client attempts a simple write the value `new_val` to the key `k` in a strongly consistent bucket (the write is simple because the client doesn't pass along any state to Riak).
* If there is currently no value associated with `k`, the write proceeds normally.
* If, however, there is already a value `old_val` stored in the key `k`, the write will fail _if it is a simple write_, i.e. the equivalent of a `PUT` request. In order for that write to succeed, it needs to pass a DVV to Riak that says to Riak "this is the most recent value that I've seen." If the client has _not_ seen most recent successful write, the write will simply fail.
 
This is the basic mechanism used to solve the problem of concurrent writes. If a client hasn't yet seen the most recent write, then the client will be essentially locked out of writing to the key. In eventually consistent Riak, Riak is always open to writes; with strong consistency, this locking out leads to a temporary loss of write availability.

The most important thing to note is that simple writes will only _ever_ succeed if a key does not already exist. If a key already exists, Riak will have to see a DVV. This makes Riak's strong consistency implementation fundamentally **state based**: DVVs are the system "state" used by Riak to make determinate judgments about which values are most recent. 

An important thing to note here is that not all writes are guaranteed to be seen, because writes can fail, as when a client hasn't seen the most recent version of a key.

An important thing to note here is that _successful_ writes are guaranteed to be seen, not just any writes. If a simple write is attempted on a KV pair that does not yet exist, it will succeed. However, if the write is attempted on KV pair that already exists, it will fail. Instead, a write must engage in a get/modify/put operation that increments the object's DVV and marks it as having been changed.

So what happens when values conflict, whether due to concurrent or partial writes or a node coming back into the network or some other reason? We can illustrate this with another hypothetical scenario:

* A client reads a key from a strongly consistent bucket. That key stores conlicting values in different nodes, A in one node and B in the other.
* On the basis of DVVs, either value A or B will be deemed logically "older" than the other. If A is selected as being logically older, it will "win" over B.
* All other nodes will be notified that A is the correct value.

Conflicts are thus sorted out at _read time_ rather than write time. Now, every time a client reads the key in the example above, Riak will return A, and B is essentially discarded as a possible true value.

For some use cases, the rollback to older values in case of "doubt" and the occasional refusal of writes can be detrimental, especially for use cases in which it's important to accept all writes. But if non-ambiguity is a primary concern, to the extent that it's worth (a) occasionally discarding logically newer values in favor of logically older ones and (b) occasionally losing write availability, then you might want to consider strong consistency for at least that portion of your data that truly requires it.

## Important Caveats

There are a few things that we should note about strong consistency as it is implemented in Riak. First of all, the following Riak features are not available in strongly consistent buckets:

* [[Secondary indexes]]
* [[Active Anti-Entropy]] syncing
* Riak [[Datatypes]]

Second of all, it needs to be noted strong consistency can be guaranteed only at the single-key level. There is currently no support for consistency across keys or with regard to multi-key operations.
