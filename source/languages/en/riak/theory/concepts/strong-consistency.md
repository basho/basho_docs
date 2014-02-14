---
title: Strong Consistency
project: riak
version: 2.0.0+
document: appendix
audience: intermediate
keyword: [appendix, concepts]
---

Riak was originally designed as an [[eventually consistent|Eventual Consistency]] system, fundamentally geared toward providing partition (i.e. fault) tolerance and high availability---a prioritization that inevitably comes at the expense of data consistency. In the language of the [CAP theorem](http://en.wikipedia.org/wiki/CAP_theorem), Riak began as an AP---highly available, partition-tolerant---system, and it continues to be an AP system by default.

Riak has always enabled users to trade some availability in favor of consistency by adjusting [[various read and write parameters|Eventual Consistency#Replication-properties-and-request-tuning]]. _In versions 2.0 or greater, Riak can be used as a_ strongly _rather than_ eventually _consistent system_.

## Strong vs. Eventual Consistency

A data storage system guarantees strong consistency when it ensures that an object, once written, won't change _from the client's perspective_ until another write. In other words, if you write a key, the next successful read of that key is _guaranteed_ to show that write. 

In an eventually consistent system, on the other hand, a read request for a key could return the value from any recent write to that key. Strong consistency means that you _never_ see out-of-date values.

To borrow the example used in our treatment of [[eventual consistency]], imagine that you are querying Riak to find out who currently manages Manchester United. In that example, the value associated with the key `manchester-manager` was originally `Alex Ferguson` (i.e. that was the first successful put request on that key). But then `David Moyes` became Man U's manager, and someone correspondingly updated the `manchester-manager` key.

Now imagine that this update was only successful on one node in the cluster. Clients can then receive either manager's name via a read request. An eventually consistent system is one in which a get request will _probably_ return `David Moyes`, but _could_ return the outdated value of `Alex Ferguson`.

In a strongly consistent system, conversely, any update must be successful on a majority of servers responsible for the data or the request will fail; similarly, read requests must reflect the majority consensus for the data. Thus a write to only one server would be a failure, and clients would continue to read `Alex Furguson` until a successful update, at which point they would be guaranteed to read `David Moyes`.

It might be useful to imagine it more abstractly, in terms of causal sequences. The following sequence would characterize a strongly consistent system:

* A put request is made that sets the value of the key `k` to `v1`
* All get requests on `k` return `v1` until the value is changed to `v2`
* Thereafter, _all_ get requests on `k` return `v2` until the value is changed to `v3`, at which point the cycle repeats
* At no point in time in this system does a read return an out-of-date value

The following sequence, however, would characterize an _eventually_ consistent system suffering server failures or network partitions:

* A put request is made that sets the value of the key `k` to `v1`
* Nearly all get requests to `k` return `v1`, but a small percentage return `not found`
* The value associated with the key `k` is updated to `v2`
* Nearly all get requests to `k` return `v2`, but a small number return the outdated `v` (and a tiny number return `not found`)

## Making the Strong vs. Eventual Decision

The first system may _sound_ like the undisputed champion, and the second system undesirable. But keep in mind:

* Reads and writes on the first system will often be slower---if only by a few milliseconds---because the system needs to utilize some kind of internal system to ensure, either at read time or write time, that reads return only the most up-to-date value. If performance is of major import, the first system might not be worth the sacrifice.

* Eventually consistent Riak will do its best to accept all writes and report back a value on reads, no matter what state the cluster is in. Strongly consistent Riak will reject any writes for which a majority of the servers responsible for that key cannot be contacted, and may not be able to answer requests for data that it has.

For most use cases, a slightly outdated value is typically "good enough" and much worth the performance and availability boost from eventual consistency. Depending on your business needs, it may be better to run the risk that a web page will occasionally report a slightly out-of-date value than to not be able to provide any value at all.
    
So when deciding whether to use strong consistency in Riak, the following question needs to be asked: _for the specific use case at hand, is it better for get requests to fail than to return a potentially out-of-date value?_ If the answer is yes, then you should seriously consider using Riak in a strongly consistent way _for the data that demands it_, while bearing in mind that other data can still be stored in Riak in a non-strongly-consistent way.

## Riak's Mixed Approach

Riak has always been a key/value store, but it is different from others in that it has always required that key/value pairs be stored in namespaces called **buckets**.

The advantage of providing multiple namespaces---as many as necessary---is that it enables users to fine-tune the availability/consistency trade-off on a bucket-by-bucket basis. Users can set some buckets to accept sloppy quorums, others with `w` and/or `r` equal to `n_val`, and so on, allowing for a mix-and-match approach to data within a Riak cluster.

This mixed approach is still possible, of course, except that now strong consistency has been introduced as yet another available bucket-level configuration. As of Riak 2.0, buckets have a property called `consistent`, which, if set to `true`, makes data in that bucket conform to strong consistency requirements. Implementation details can be found in the [[Using Strong Consistency]] tutorial.
