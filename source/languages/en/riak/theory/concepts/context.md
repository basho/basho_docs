---
title: Causal Context Objects
project: riak
version: 2.0.0+
document: appendix
audience: intermediate
keywords: [appendix, concepts]
---

Because Riak is an [[eventually consistent|Eventual Consistency]],
[[clustered|Clusters]] system, [[conflicts|Conflict Resolution]] between
object replicas stored on different nodes are inevitable, particularly
in cases when multiple connecting clients update an object at the same
time.

## The Problem of Conflict Values

To illustrate this problem, imagine that you're building a
[CRM](http://en.wikipedia.org/wiki/Customer_relationship_management)
application and storing customer information in Riak. Now imagine that
information about a user is being stored in the [[key|Keys and Objects]]
`mariejohnston` in the [[bucket|Buckets]] `customers`. What happens if
Marie has two browser windows open and changes her phone number to
555-1337 in one window and saves it, and also changes it to 555-1212 in
another window?

This means that two different values end up being stored in Riak. So
what happens at that point? There are essentially three possible
outcomes:

1. The two different values end up being stored in Riak, but Riak is
able to discern that one object is more causally recent than the other
(in this case 555-1212) and chooses that value as the "correct"/most
recent value

2. The two different values end up being stored in Riak, but the two
operations happen at roughly the same time, i.e. two **concurrent
updates** have been attempted, and Riak is unable to determine which
value "wins." In this scenario, one of two things can happen:

    a. Riak creates sibling values, aka **siblings**, for the object

    b. Riak chooses a value for you on the basis of timestamps or some
       other mechanism.

In the case of outcome 1 above, Riak uses **causal context objects** to
make that decision. 

The choice between **a** and **b** above is yours to make.

One means of achieving this is to use a mechanism like
[timestamps](http://en.wikipedia.org/wiki/Timestamp) to determine which
object is more **chronologically** recent. While Riak [[provides you
this option|Conflict
Resolution#Client-and-Server-side-Conflict-Resolution]], timestamps are
a notoriously unreliable means of determining which object value is most
current in distributed systems. A much more reliable means is to track
the **causal** history of object updates.

Riak current offers two mechanisms for tracking causality: [[vector
clocks|Causal Context Objects#Vector-Clocks]] and [[dotted version
vectors|Causal Context Objects#Dotted-Version-Vectors]]. You can find an
explanation of both below, including usage information.

## Vector Clocks

One of Riak's central goals is high availability. It was built as a
multi-node system in which any node is capable of receiving requests
without requiring that each node participate in each request. In a
system like this, it's important to be able to keep track of which
version of a value is the most current. This is where vector clocks
come in.

## Vector Clocks and Relationships Between Objects

All Riak objects are stored in a location defined by the object's
[[bucket|Buckets]] and [[key|Keys and Objects]], as well as by the
[[bucket type|Using Bucket Types]] defining the bucket's properties. It
is possible to [[configure Riak|Conflict Resolution]] to ensure that
only one copy of an object ever exists in a specific location. This will
ensure that _at most_ one object is returned when a read is performed on
a bucket type/bucket/key location (and no objects if Riak returns `not
found`).

If Riak is configured this way, Riak may still make use of vector clocks
behind the scenes to make intelligent decisions about which replica of
an object should be deemed the most recent, but in that case vector
clocks will be a non-issue for clients connecting to Riak.

## Siblings

It is also possible to configure Riak to store multiple objects in a
single key, i.e. for an object to have different values on different
nodes. Objects stored this way are called **siblings**. You can instruct
Riak to allow for sibling creation by setting the the `allow_mult`
bucket property to `false` for a specific bucket, preferably [[using
bucket types]].

This is where vector clocks come in. Vector clocks are metadata attached
to all Riak objects that enable Riak to determine the causal
relationship between two two objects. Vector clocks are non-human-
readable and look something like this:

```
a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=
```

A number of important aspects of the relationship between object
replicas can be determined using vector clocks:

 * Whether one object is a direct descendant of the other
 * Whether the objects are direct descendants of a common parent
 * Whether the objects are unrelated in recent heritage

Behind the scenes, Riak uses vector clocks as an essential element of
its [[active anti-entropy]] subsystem and of its automatic [[read
repair|Active Anti-Entropy#read-Repair-vs.-active-anti-entropy]]
capabilities.

From the standpoint of application development, the difficulty with
siblings is that they _by definition_ conflict with one another. When an
application attempts to read an object that has siblings, multiple
replicas will be stored in the location where the application is
looking.  This means that the application will need to develop a
strategy for [[conflict resolution]].

## More Information

Additional information on vector clocks:

* [[Conflict Resolution]] in Riak
* [[Vector Clocks on Wikipedia|http://en.wikipedia.org/wiki/Vector_clock]]
* [[Why Vector Clocks are Easy|http://blog.basho.com/2010/01/29/why-vector-clocks-are-easy/]]
* [[Why Vector Clocks are Hard|http://blog.basho.com/2010/04/05/why-vector-clocks-are-hard/]]
* The vector clocks used in Riak are based on the [[work of Leslie Lamport|http://portal.acm.org/citation.cfm?id=359563]].

## Dotted Version Vectors

In versions of Riak prior to 2.0, all causality-based conflict resolution, whether on the
client side or in Riak, was achieved using [[vector clocks|Causal
Context Objects#Vector-Clocks]]. In version 2.0, Riak added the option
of using **dotted version vectors** (DVVs) instead.

Like vector clocks, dotted version vectors are a mechanism for tracking
object update causality in terms of **logical time** rather than
chronological time (as with timestamps), enabling Riak to make decisions
about which objects are more current than others in cases of conflict.

## DVVs Versus Vector Clocks

The role that DVVs play in Riak is directly analogous to that of
[[vector clocks|Causal Context Objects#Vector-Clocks]], as both are used to resolve object conflicts, whether
during background operations like [[active anti-entropy]] or [[read
repair|Riak Glossary#read-repair]], or when applications engage in
client-side [[conflict resolution]]. The crucial difference between
them, however, lies in the way that they handle concurrent updates.

Vector clocks can detect concurrent updates to the same object but do
not identify which value was associated with each update. If an object
stored in the bucket `frequent_updates` with the key `update_me` is
updated by five different clients concurrently and tagged with the same
vector clock, then five values should be created as siblings.  However,
depending on the order of delivery of those updates to the different
replicas, sibling values may be duplicated, which can in turn lead to
[[sibling explosion|Vector Clocks#sibling-explosion]] and thus undue
[[latency|Latency Reduction Checklist]].

DVVs, on the other hand, identify each value with the update that
created it. If five clients concurrently update the object above (in the
bucket `frequent_updates`, with the key `update_me`), each of these
updates will be marked with a _dot_ (a minimal [[vector clock|vector
clocks]]) that indicates the specific event that introduced it. This
means that duplicate values can always be identified and removed,
reducing the likelihood of [[sibling explosion|Vector
Clocks#sibling-explosion]]. Rather than being potentially unbounded, the
number of sibling values will be proportional to the number of
concurrent updates.

In terms of performance, the difference between vector clocks and DVVs
should be minimal in most cases. Because DVVs de-duplicate updates, they
should generally be smaller than objects that use vector clocks.

## Usage

From an application's perspective, vector clocks and DVVs function in
exactly the same fashion. Object updates using DVVs involve the same
sequence in interacting with Riak:

* fetch an object from Riak,
* fetch the object's metadata, which will contain an opaque context
  object (e.g. `a85hYGBgzGDKBVIcWu/1S4Pjin9lMCWy5bEycN1/cYYvCwA=`) for
  the vector clock or DVV attached to that version of the object, and
  finally
* pass that opaque context object back to Riak when you update the
  object.

There's a good chance that you will not need to modify your application
code when switching from vector clocks to DVVs, even if you choose to
switch all Riak objects in your cluster to DVVs. You should make sure,
however, that the right bucket types and buckets are being targeted by
your application after the `dvv_enabled` parameter has been changed.

For compatibility's sake, DVVs contained in Riak objects' metadata are
still labeled `X-Riak-Vclock` if you're using the [[HTTP API]] and
`vclock` if using the [[Protocol Buffers interface|PBC Fetch Object]].

More on using vector clocks and DVVs on the application side can be
found in our documentation on [[conflict resolution]].

<div class="note">
<div class="title">Note on DVVs and bucket types</div>
The choice between vector clocks and DVVs can be made at the bucket
level, [[using bucket types]]. This enables you to employ a mixed
conflict resolution strategy in your Riak cluster, using DVVs in some
buckets and vector clocks in others if you wish. DVVs can be enabled by
setting the <code>dvv_enabled</code> bucket property to
<code>true</code> for one or more bucket types.

Vector clocks remain the default if you are not using bucket types.
However, any bucket type that you create and activate will have
<code>dvv_enabled</code> set to <code>true</code>. And so if you wish to
create a bucket type that uses traditional vector clocks, you will need
to explicitly set <code>dvv_enabled</code> to <code>false</code> for
that bucket type.
</div>

## Resources

* [Evaluating Dotted Version Vectors in Riak](http://asc.di.fct.unl.pt/~nmp/pubs/inforum-2011-2.pdf)
* [Improving Logical Clocks in Riak with Dotted Version Vectors: A Case Study](http://paginas.fe.up.pt/~prodei/dsie12/papers/paper_19.pdf)
* [Dotted Version Vector Sets](https://github.com/ricardobcl/Dotted-Version-Vectors)
