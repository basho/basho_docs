---
title: Strong Consistency
project: riak
version: 2.0.0+
document: appendix
audience: intermediate
keyword: [appendix, concepts]
---

Riak was originally designed as an [[eventually consistent|Eventual Consistency]] system, fundamentally geared toward partition (i.e. fault) tolerance and high availability---a prioritization that _necessarily_ comes at the expense of data consistency. In the language of the CAP theorem, Riak began as an AP---highly available, partition-tolerant---system, and it continues to be an AP system by default.

Yet in spite of this, Riak has always enabled users to sacrifice availability in favor of stronger consistency if they wish by adjusting [[various read and write parameters|Eventual Consistency#Replication-properties-and-request-tuning]]. _In versions >= 2.0, Riak can be used as a strongly rather than eventually consistent system_.

## The Meaning of Strong Consistency

A data storage system guarantees strong consistency when it ensures that an object hasn't changed since you last read it. In other words, if you write a key, the next successful read of that key is _guaranteed_ to show that write. 

In an eventually consistent system, on the other hand, a get request could see the value for the most recent successful put _or_ potentially an older, out-of-date value. Strong consistency means that you simply _never_ see out-of-date values. If there is any question as to which value is up to date, the read will simply fail.

To borrow the example used in our treatment of [[eventual consistency]], imagine that you are querying Riak to find out who currently manages Manchester United. In this example, the value associated with the key `manchester-manager` was originally `Alex Ferguson` (i.e. that was the first put request for that value). But then `David Moyes` became Man U's manager, and somebody ran a put request on the `manchester-manager` key to change its value to be more up to date.

Now imagine that this put request was only successful on one node in the cluster. The problem then arises that one node says that the value of `manchester-manager` is `David Moyes` while all of the others say `Alex Ferguson`. In an eventually consistent system, it is possible that you will want to know the correct value `David Moyes` and yet still get `Alex Ferguson`, _even though that get request is deemed successful_.

In a strongly consistent system, conversely, any successful get request on `manchester-manager` will return `David Moyes` after the key has been updated. Once that change has been made, a successful operation to fetch it will always return the same value. If a node is queried that still has `Alex Ferguson` as the value, it will not return that value. And so the trade-off with strong consistency is that get requests can _fail_ because a key holds an out-of-date value.

It might be useful to imagine it more abstractly, in terms of causal sequences. The following sequence would characterize a strongly consistent system:

* A put request is made that sets the value of the key `k` to `v`
* All get requests on `k` return `v` until the value is changed to `v2` (and this value is deemed the "correct" one)
* Thereafter, _all_ get requests on `k` either return `v2` or fail, until the value is changed to `v3`, at which point the cycle repeats

At no point in time in this system does a get request return an out-of-date value.

The following sequence, however, could characterize an _eventually_ consistent system:

* A put request is made that sets the value of the key `k` to `v`
* Nearly all get requests to `k` return `v`, but a small percentage return `not found`
* The value associated with the key `k` is updated to `v2`
* Nearly all get requests to `k` return `v2`, but a small number return the outdated `v` (and a tiny number return `not found`)

The first system may _sound_ like the undisputed champion, and the second system undesirable. But there are two things need to be borne in mind:

* Get requests on the first system will usually be slower, if only a few milliseconds slower.
* For many use cases, a slightly outdated value might be both "good enough" and strongly preferred to `not found`. It might be better to find out (inaccurately) that Alex Ferguson is the manager of Manchester United than to get a `not found` and be left to wonder how that result should be interpreted.

So when deciding whether to use strong consistency in Riak, the following question needs to be asked: _for the specific use case at hand, is it better for get requests to fail than to return a potentially out-of-date value?_ If the answer is yes, then you should seriously consider using Riak in a strongly consistent way _for the data that demands it_, while bearing in mind that other data can still be stored in Riak in a non-strongly-consistent way.

## How to Use Riak in a Strongly Consistent Way

Riak has always required that key/value pairs be stored in buckets, and users have always been able to fine-tune the availability/consistency trade-off on a bucket-by-bucket basis. Users can set some buckets to accept sloppy quorums, others with `w` and/or `r` equal to `n_val`, and so on, allowing for a mix-and-match approach to data within a Riak cluster.

This mixed approach remains possible, but strong consistency has been introduced as yet another possible bucket-level configuration. Buckets now have a property called `consistent`, which, if set to `true`, makes that bucket strongly consistent. Implementation details can be found in the [[Using Strong Consistency]] tutorial.

## How Strong Consistency is Guaranteed in Riak

Under the hood, Riak guarantees strong consistency (where enabled) by essentially 

Strong consistency guarantees are beneficial when dealing with critical data and no sloppy quorums are allowed; eventual consistency is great and all, but in the long run, we're all dead (the problem of recency); any `GET` will see the most recent successful `PUT` (does this mean you have to lock out some `PUT`s?)

Consistent operations are conditional, single-key atomic updates. 

It has been argued that 
R + W > N is not true strong consistency because concurrent requests still generate non-deterministic results. Node failures and network partitions can lead to partial write failures that provide no guarantees on value consistency.

Strong consistency means atomic updates; it means that the write set and the read set always overlap

Riak resolves write ambiguity at read time rather than write time. Writes 

Partial writes need to be resolved at read time; you need to be sure that nothing changes behind your back

Riak resolves partial write ambiguity at read time; if you partially write to a replica that then goes offline (or becomes otherwise partitioned), the old value wins; the partitioned, failed-to-write value is rolled back, so to speak. 

One way of ensuring strong consistency is to do so at the application level; a better way for a lot of uses cases is to let the database do that for you; siblings mean that if a write fails, the next read defines consistency

The partial consistency approach involves choosing Rs and Ws along a continuum between 0 and N; the problem is that you always incur a performance hit as R and/or W increase because Riak has to communicate with and wait for results on more partitions

Ensembles: consensus groups; 64-partition ring means 64 ensembles; each ensemble elects a leader, established an epoch, and supports get/put operations

CRDTs don't have recency; 

Conditional, single-key, atomic updates; no siblings

## Limitations

It is important to always bear in mind that there are some Riak features that cannot be used in conjunction with strong consistency requirements. Riak's [[CRDT]]-inspired dataypes---sets, counters, and maps---are by definition _convergent_ rather than strongly consistent. When using Riak datatypes, there can be no guarantee


## Further Reading

## Scratchpad

https://github.com/basho/riak_kv/pull/710