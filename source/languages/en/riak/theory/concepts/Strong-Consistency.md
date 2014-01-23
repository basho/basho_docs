---
title: Strong Consistency
project: riak
version: 2.0.0+
document: appendix
audience: intermediate
keyword: [appendix, concepts]
---

Riak was originally designed as an [[eventually consistent|Eventual Consistency]] system, strongly favoring the A and P in CAP over the C.

Strong consistency guarantees are beneficial when dealing with critical data and no sloppy quorums are allowed; eventual consistency is great and all, but in the long run, we're all dead (the problem of recency); any `GET` will see the most recent successful `PUT` (does this mean you have to lock out some `PUT`s?)

In versions of Riak >= 2.0, you have the option of implementing strong consistency guarantees on a bucket-by-bucket basis. This means that you can mix and match data approaches in a Riak cluster. Some buckets can favor availability while others favor consistency.

R + W > N is not true strong consistency because concurrent requests still generate non-deterministic results. Node failures and network partitions can lead to partial write failures that provide no guarantees on value consistency.

Strong consistency means atomic updates; it means that the write set and the read set always overlap

Sequence of events:

* `PUT` request of value `v` on key `k`
* `GET` request on `k` returns `v`

* `PUT` request of value `v` on key `k`
* `PUT` request of value `v1` on key `k`
* `GET` request on `k` returns `v1` and never `v`

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
