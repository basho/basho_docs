---
title: Strong Consistency
project: riak
version: 2.0.0+
document: appendix
audience: intermediate
keyword: [appendix, concepts]
---

Riak was originally designed as an [[eventually consistent|Eventual Consistency]] system, fundamentally geared toward providing partition (i.e. fault) tolerance and high availability---a prioritization that inevitably comes at the expense of data consistency. In the language of the [CAP theorem](http://en.wikipedia.org/wiki/CAP_theorem), Riak began as an AP---highly available, partition-tolerant---system, and it continues to be an AP system by default.

Yet in spite of this, Riak has always enabled users to sacrifice availability in favor of stronger consistency if they wish by adjusting [[various read and write parameters|Eventual Consistency#Replication-properties-and-request-tuning]]. _In versions 2.0 or greater, Riak can be used as a_ strongly _rather than_ eventually _consistent system_.

## Strong vs. Eventual Consistency

A data storage system guarantees strong consistency when it ensures that an object hasn't changed since you last read it. In other words, if you write a key, the next successful read of that key is _guaranteed_ to show that write. 

In an eventually consistent system, on the other hand, a get request on a key could return the value of the most recent successful put _or_ potentially an out-of-date value. Strong consistency means that you simply _never_ see out-of-date values.

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

Riak has always been a key/value store, but it is different from others in that it has always required that key/value pairs be stored in namespaces called [[**buckets**|The Basics#Bucket-Properties-and-Operations]].

The advantage of providing multiple namespaces---as many as you wish---is that it lets you fine-tune the availability/consistency trade-off on a bucket-by-bucket basis. Using bucket types, you can make some buckets accept sloppy quorums, others with `w` and/or `r` equal to `n_val`, and so on, allowing for a mix-and-match approach to data within a Riak cluster.

This mixed approach is still possible, of course, except that now strong consistency has been introduced as yet another possible bucket-level configuration. As of Riak 2.0, buckets have a property called `consistent`, which, if set to `true`, makes data in that bucket conform to strong consistency requirements. Implementation details can be found in the [[Using Strong Consistency]] tutorial.
