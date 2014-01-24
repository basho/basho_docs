---
title: Strong Consistency
project: riak
version: 2.0.0+
document: appendix
audience: intermediate
keyword: [appendix, concepts]
---

Riak was originally designed as an [[eventually consistent|Eventual Consistency]] system, geared fundamentally toward partition (i.e. fault) tolerance and high availability---a prioritization that necessarily comes at the expense of data consistency. In the language of the CAP theorem, Riak began as an AP system, and it continues to be an AP system by default.

Yet in spite of this, Riak has always enabled users to sacrifice availability in favor of stronger consistency if they wish by adjusting [[various read and write parameters|Eventual Consistency#Replication-properties-and-request-tuning]]. Beginning with version 2.0, Riak even allows users to use it as a strongly rather than eventually consistent system.

## The Meaning of Strong Consistency

To put it simply, a system guarantees strong consistency when any get request will see the most recent successful put request. In an eventually consistent system, a get request could see the most recent successful put _or_ an older, out-of-date put.

To borrow the example employed in our treatment of [[eventual consistency]], imagine that you are querying Riak to find out who currently manages Manchester United. Originally, the value associated with the key `manchester-manager` was `Alex Ferguson`. But then `David Moyes` became manager, and somebody ran a put request on the `manchester-manager` key to change its value to be more up to date.

The problem: let's say that this put request was only successful on one node in the cluster, and so one node says that the value of `manchester-manager` is `David Moyes` while all of the others say `Alex Ferguson`. In an eventually consistent system, it is possible that you will need to know the correct value `David Moyes` and yet get `Alex Ferguson`, _and that get is deemed successful_.

In a strongly consistent system, however, any successful get request on `manchester-manager` will return `David Moyes` after the key has been updated. Once you make that change, a successful operation to fetch it will always return the same value. But this comes at a cost. The trade-off is that you could run get requests that _fail_ because a key holds out-of-date information.

Or imagine it more abstractly in terms of causal sequences. This sequence would characterize a strongly consistent system:

* A put request is made that sets the value of the key `k` to `v`
* All get requests on `k` return `v` until the value is changed to `v2` (and this value is deemed the "correct" one)
* Thereafter, all get requests on `k` return `v2`, until the value is changed to `v3`, and the cycle repeats.

The following sequence, however, is possible in an eventually consistent system:

* A put request is made that sets the value of the key `k` to `v`
* Most get requests to `k` return `v`, but a small percentage return `not found`
* The value associated with the key `k` is changed to `v2` (and this value is the one that you would want your application to get)
* Most get requests to `k` indeed return `v2`, but a small number return the outdated `v` (and a tiny number return `not found`)

The first system may _sound_ like a knock-down, drag-out winner, but it needs to be borne in mind that:

* Get requests on that system will be slower (though often only on the order of milliseconds).
* In a lot of cases, a slightly outdated value might be "good enough," and strongly preferred to `not found`. It might be better to find out (inaccurately) that Alex Ferguson is the manager of Manchester United than to get a `not found` and be left to wonder what that means. Does the team currently have no manager? Does the team no longer exist?

So this is the question that needs to be asked: for a particular use case, is it better for get requests to fail than to return a potentially out-of-date value? If the answer is yes, then you should seriously consider using Riak in a strongly consistent way _for that data_, while bearing in mind that you can still store other data in Riak in a non-strongly-consistent way.

## How to Use Riak in a Strongly Consistent Way

Riak has always required that key/value pairs be stored in buckets, and users have always been able to fine-tune the availability/consistency trade-off on a bucket-by-bucket basis. You can set some buckets to accept sloppy quorums, others with `w` and/or `r` equal to `n_val`, and so on, allowing for a mix-and-match approach to data within a Riak cluster.

This remains the case, except that now strong consistency is yet another possible bucket configuration. Buckets now have a new property called `consistent`, which, if set to `true`, makes that bucket strongly consistent.




Strong consistency guarantees are beneficial when dealing with critical data and no sloppy quorums are allowed; eventual consistency is great and all, but in the long run, we're all dead (the problem of recency); any `GET` will see the most recent successful `PUT` (does this mean you have to lock out some `PUT`s?)

R + W > N is not true strong consistency because concurrent requests still generate non-deterministic results. Node failures and network partitions can lead to partial write failures that provide no guarantees on value consistency.

Strong consistency means atomic updates; it means that the write set and the read set always overlap





The scenario to be avoided is one in which `v1` is the absolutely authoritative value, and yet a get request on `k` returns `v`

One way of ensuring strong consistency is to do so at the application level; a better way for a lot of uses cases is to let the database do that for you; siblings mean that if a write fails, the next read defines consistency

The partial consistency approach involves choosing Rs and Ws along a continuum between 0 and N; the problem is that you always incur a performance hit as R and/or W increase because Riak has to communicate with and wait for results on more partitions

Best practice: N = 5 with strong consistency turned on

Two approaches within AP:

* strict quorum --- divergent updates are okay
* sloppy quorum --- Riak default

Partial writes need to be resolved at read time; you need to be sure that nothing changes behind your back

Ensembles: consensus groups; 64-partition ring means 64 ensembles; each ensemble elects a leader, established an epoch, and supports get/put operations

CRDTs don't have recency; 

Conditional, single-key, atomic updates; no siblings

## At the Bucket Level

If `consistent` is set to `true` for a bucket, objects written to that bucket are consistent objects

Consistent operations:

* If you put a key, the next successful read is _guaranteed_ to show that write; 
* If you get/put an object, Riak ensures that the object hasn't changed since you read it; the request will fail if a concurrent write occurred and changed the object; 
* Riak resolves partial write ambiguity at read time; if you partially write to one replica that then goes offline (or becomes otherwise partitioned); the old value wins; the partitioned, failed-to-write value is rolled back

Limitations:

* No 2i
* No support for object mutators (hence no CRDTs)
* No firing of stats-related events

## Further Reading
