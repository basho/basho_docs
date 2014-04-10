---
title: Eventual Consistency
project: riak
version: 0.10.0+
document: appendix
toc: true
audience: intermediate
keywords: [appendix, concepts]
moved: {
  '1.4.0-': '/references/appendices/concepts/Eventual-Consistency'
}
---

In a distributed and fault-tolerant environment like Riak, server and network failures are expected. Riak is designed to respond to requests even when servers are offline or the cluster is experiencing a network partition.

Riak handles this problem by enabling conflicting copies of data stored in the same location, as specified by [[bucket type|Using Bucket Types]], bucket, and key, to exist at the same time in the cluster. This gives rise to the problem of **data inconsistency**.

## Data Inconsistency

Conflicts between replicas of an object are inevitable in highly-available, [[clustered|Clusters]] systems like Riak because there is nothing in those system do not allow for so-called [ACID transactions](http://en.wikipedia.org/wiki/ACID) that ensure that no conflicts are allowed to originate. Instead, these systems need to rely on some form of conflict-resolution mechanism.

One of the things that makes Riak's eventual consistency model powerful is that Riak does not dictate how data resolution takes place. While Riak does ship with a set of defaults regarding how data is [[replicated|Eventual Consistency#replication-properties-and-request-tuning]] and how [[conflicts are resolved|Conflict Resolution]], you can override these defaults if you want to employ a different strategy.

Among those strategies, you can enable Riak to resolve object conflicts automatically, whether via internal [[vector clocks]], timestamps, or special eventually consistent [[data types]], or you can resolve those conflicts on the application side by employing a use case-specific logic of your choosing. More information on this can be found in our guide to [[conflict resolution]].

This variety of options enables you to manage Riak's eventually consistent behavior in accordance with your application's [[data model or models|Use Cases]].

## Replication Properties and Request Tuning

In addition to providing you different means of resolving conflicts, Riak also enables you to fine-tune **replication properties**, which determine things like the number of nodes on which data should be stored and the number of nodes that are required to respond to read, write, and other requests.

An in-depth discussion of these behaviors and how they can be implemented on the application side can be found in our guides to [[replication properties]] and [[conflict resolution]].

In addition to our official documentation, we also recommend checking out the [Understanding Riak's Configurable Behaviors](http://basho.com/understanding-riaks-configurable-behaviors-part-1/) series from [our blog](http://basho.com/blog/).

## A Simple Example of Eventual Consistency

Let's assume for the moment that a sports news application is storing all of its data in Riak. One thing that the application always needs to be able to report to users is the identity of the current manager of Manchester United, which is stored in the key `manchester-manager` in the bucket `premier-league-managers`. This bucket has `allow_mult` set to `false`, which means that Riak will resolve all conflicts by itself.

Now let's stay that a server in this cluster has recently recovered from failure and has an old copy of the key `manchester-manager` stored in it, with the value `Alex Ferguson`. The problem is that Sir Ferguson stepped down in 2013 and is no longer the manager. Fortunately, the other servers in the cluster hold the value `David Moyes`, which is correct.

Shortly after the recovered server comes back online, other cluster members recognize that it is available. Then, a read request for `manchester-manager` arrives from the application. Regardless of which order the responses arrive to the server that is coordinating this request, `David Moyes` will be returned as the value to the client, because `Alex Ferguson` is recognized as an older value.

Why is this? How does Riak make this decision? Behind the scenes, after `David Moyes` is sent to the client, a [[read repair|Riak Glossary#read-repair]] mechanism will occur on the cluster to fix the older value on the server that just came back online. Because Riak tags all objects with versioning information, it can make these kinds of decisions on its own, if you wish.

### R=1

Let's say that you keep the above scenario the same, except you tweak the request and set R to 1, perhaps because you want faster responses to the client. In this case, it _is_ possible that the client will receive the outdated value `Alex Ferguson` because it is only waiting for a response from one server. If the recently recovered server is the first to reply

Relationship between replication properties and conflict resolution

If we keep the above scenario the same but tweak the request slightly with `R=1`, perhaps to allow for a faster response to the client, it _is_ possible that the client will be fed `Alex Ferguson` as the response, if the recently recovered server is the first to reply.

However, the read repair mechanism will kick in and fix the value, so the
next time someone asks for the value of `manchester-manager`, `David Moyes` will indeed be the answer.

### R=1, sloppy quorum

Let's take the scenario back in time to the point at which our unlucky server originally failed. At that point, all 3 servers had `Alex Ferguson` as the value for `manchester-manager`.

When a server fails, Riak's *sloppy quorum* feature kicks in and another server takes responsibility for serving its requests.

The first time we issue a read request after the failure, if `R` is set to 1, we run a significant risk of receiving a `not found` response from Riak. The server that has assumed responsibility for that data won't have a copy of `manchester-manager` yet, and it's much faster to verify a missing key than to pull a copy of the value from disk, so that server will likely respond fastest.

If `R` is left to its default value of 2, there wouldn't be a problem because 1 of the servers that still had a copy of `Alex Ferguson` would also respond before the client got its result. In either case, read repair will step in after the request has been completed and make certain that the value is propagated to all the servers that need it.

### PR, PW, sloppy quorum

Thus far, we've discussed settings that permit sloppy quorums in the interest of allowing Riak to maintain as high a level of availability as possible in the presence of server or network failure.

It is possible to configure requests to ignore sloppy quorums in order to limit the possibility of older data being returned to a client. The tradeoff, of course, is that there is an increased risk of request failures if failover servers are not permitted to serve requests.

In the scenario we've been discussing, for example, the possibility of a server for the `manchester-manager` key having failed, but to be more precise, we've been talking about a *primary* server, one that when the cluster is perfectly healthy would bear responsibility for that key.

When that server failed, using `R=2` as we've discussed or even `R=3` for a read request would still work properly: a failover server (sloppy quorum again) would be tasks to take responsibility for that key, and when it receives a request for it, it would reply that it doesn't have any such key, but the two surviving primary servers still know who the `manchester-manager` is.

However, if the PR (primary read) value is specified, only the two surviving primary servers are considered valid sources for that data.

So, setting PR to 2 works fine, because there are still 2 such servers, but a read request with PR=3 would fail because the 3rd primary server is offline, and no failover server can take its place *as a primary*.

The same is true of writes: W=2 or W=3 will work fine with the primary server offline, as will PW=2 (primary write), but PW=3 will result in an error.

<div class="note">
<div class="title">Errors and Failures</div>
It is important to understand the difference between an error and a failure.

The <tt>PW=3</tt> request in this scenario will result in an error, <strong>but the value will still be written to the two surviving primary servers</strong>.

By specifying <tt>PW=3</tt> the client indicated that 3 primary servers must respond for the operation to be considered successful, which it wasn't, but there's no way to tell without performing another read whether the operation truly failed.
</div>

## Further Reading

* [Understanding Riak's Configurable Behaviors blog series](http://basho.com/understanding-riaks-configurable-behaviors-part-1/)
* Werner Vogels, et. al.: [Eventually Consistent - Revisited](http://www.allthingsdistributed.com/2008/12/eventually_consistent.html)
