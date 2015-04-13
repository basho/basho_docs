---
title: Causal Context
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

## The Problem of Conflicting Values

To illustrate this problem, imagine that you're building a
[CRM](http://en.wikipedia.org/wiki/Customer_relationship_management)
application and storing customer information in Riak. Now imagine that
information about a particular  user is being stored in the [[key|Keys
and Objects]] `mariejohnston` in the [[bucket|Buckets]] `customers`.
What happens if Marie has two browser windows open and changes her phone
number to 555-1337 in one window and saves it, and then also changes it
to 555-1212 in another window and saves it?

This means that two different values are written to Riak. So what
happens at that point? There are essentially three possible outcomes:

1. The two different values end up being stored in Riak, but Riak is
able to discern that one object is more causally recent than the other
(in this case 555-1212) and chooses that value as the "correct"/most
recent value.
2. The two different values end up being stored in Riak, but the two
operations happen at roughly the same time, i.e. two **concurrent
updates** have been completed, and Riak is unable to determine which
value "wins." In this scenario, one of two things can happen:

    a. Riak creates sibling values, aka **siblings**, for the object

    b. Riak chooses a single value for you on the basis of timestamps

In the case of outcome 1 above, Riak uses **causal context** metadata to
make that decision. This metadata is attached to every object in Riak.
Causal context comes in two forms in Riak: **vector clocks** and
**dotted version vectors**. More information in both can be found in the
sections below.

In the case of outcome 2, the choice between **a** and **b** is yours to
to make. If you set the `allow_mult` parameter to `true` for a bucket,
[[using bucket types]], all writes to that bucket will create siblings
in the case of concurrent writes (and occasionally under other
scenarios, e.g. healed network partitions).

If, however, `allow_mult` is set to `false`, then Riak will not generate
siblings, instead relying on internal mechanisms to decide which value
"wins." In general, we recommend _always_ setting `allow_mult` to
`true`. A more complete discussion can be found in our documentation on
[[conflict resolution|Conflict
Resolution#Client-and-Server-side-Conflict-Resolution]].

## Vector Clocks

In versions of Riak prior to 1.4, Riak used vector clocks as the sole
means of tracking the history of object updates. In Riak versions 2.0
and later, we recommend using [[dotted version vectors|Causal
Context#Dotted-Version-Vectors]] instead, for reasons that are explained
in that section.

Like dotted version vectors, vector clocks are a means of tracking
events in distributed systems. Unlike normal clocks, vector clocks have
no sense of chronological time, i.e. they don't care if something
happened at 6 pm today or back in 1972. They care only about sequences
of events. More specifically, they keep track of who---i.e. which actor
in the system---has modified an object and how many times they've done
so.

In a distributed system like Riak, multiple replicas of each object are
active in the cluster all the time. Because it's inevitable that objects
will have conflicting values due to events like concurrent updates and
healed network partitions, Riak needs a mechanism to keep track of which
replica of an object is more current than another. In versions of Riak
prior to 2.0, vector clocks were the means employed by Riak to do
precisely that.

A number of important aspects of the relationship between object
replicas can be determined using vector clocks:

 * Whether one object is a direct descendant of the other
 * Whether the objects are direct descendants of a common parent
 * Whether the objects are unrelated in recent heritage

Behind the scenes, Riak uses vector clocks as an essential element of
its [[active anti-entropy]] subsystem and of its automatic [[read
repair|Active Anti-Entropy#read-Repair-vs.-active-anti-entropy]]
capabilities.


Vector clocks are non-human-readable metadata attached to all Riak
objects. They look something like this:

```
a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=
```

While vector clocks quite often resolve object conflicts without
trouble, there are times when they can't, i.e. when it's unclear which
value of an object is most current. When that happens, Riak, if
configured to do so, will create **siblings**.

## Siblings

It is possible, though not recommendable, to [[configure Riak|Conflict
Resolution]] to ensure that only one copy of an object ever exists in a
specific location. This will ensure that _at most_ one value is returned
when a read is performed on a bucket type/bucket/key location (and no
value if Riak returns `not found`).

It's also possible, however, to configure Riak to store multiple objects
in a single key if necessary, i.e. for an object to have different
values on different nodes. Objects stored this way have what are called
sibling values. You can instruct Riak to allow for sibling creation by
setting the the `allow_mult` bucket property to `true` for a specific
bucket, preferably [[using bucket types]].

From the standpoint of application development, the difficulty with
siblings is that they _by definition_ conflict with one another. When an
application attempts to read an object that has siblings, multiple
replicas will be stored in the location where the application is
looking. This means that the application will need to develop a
strategy for [[conflict resolution]], i.e. the application will need to
decide which value is more correct depending on the use case.

## More Information on Vector Clocks

Additional information on vector clocks:

* [[Conflict Resolution]] in Riak
* [[Vector Clocks on Wikipedia|http://en.wikipedia.org/wiki/Vector_clock]]
* [[Why Vector Clocks are Easy|http://blog.basho.com/2010/01/29/why-vector-clocks-are-easy/]]
* [[Why Vector Clocks are Hard|http://blog.basho.com/2010/04/05/why-vector-clocks-are-hard/]]
* The vector clocks used in Riak are based on the [[work of Leslie Lamport|http://portal.acm.org/citation.cfm?id=359563]].

## Dotted Version Vectors

In versions of Riak prior to 2.0, all causality-based conflict
resolution, whether on the client side or in Riak, was achieved using
[[vector clocks|Causal Context#Vector-Clocks]]. In version 2.0,
Riak added the option of using **dotted version vectors** (DVVs)
instead.

Like vector clocks, dotted version vectors are a mechanism for tracking
object update causality in terms of **logical time** rather than
chronological time (as with timestamps), enabling Riak to make decisions
about which objects are more current than others in cases of conflict.

<div class="note">
<div class="title">DVVs Recommended Over Vector Clocks</div>
If you are using Riak version 2.0 or later, we strongly recommend using
dotted version vectors instead of vector clocks, as DVVs are far better
at limiting the number of siblings produced in a cluster, which can
prevent a wide variety of potential issues.
</div>

## DVVs Versus Vector Clocks

The role that DVVs play in Riak is directly analogous to that of
[[vector clocks|Causal Context#Vector-Clocks]], as both are used
to resolve object conflicts, whether during background operations like
[[active anti-entropy]] or [[read repair|Riak Glossary#read-repair]], or
when applications engage in client-side [[conflict resolution]]. The
crucial difference between them, however, lies in the way that they
handle concurrent updates.

Vector clocks can detect concurrent updates to the same object but they
can't identify which value was associated with each update. If an object
stored in the bucket `frequent_updates` with the key `update_me` is
updated by five different clients concurrently and tagged with the same
vector clock, then five values should be created as siblings.  However,
depending on the order of delivery of those updates to the different
replicas, sibling values may be duplicated, which can in turn lead to
[[sibling explosion|Causal Context#Sibling-Explosion]] and thus undue
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

You will not need to modify your application code when switching from
vector clocks to DVVs, even if you choose to switch all Riak objects in
your cluster to DVVs. You should make sure, however, that the right
bucket types and buckets are being targeted by your application after
the `dvv_enabled` parameter has been changed.

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

## Sibling Explosion

Sibling explosion occurs when an object rapidly collects siblings that
are not reconciled. This can lead to a variety of problems, including
degraded performance, especially if many objects in a cluster suffer
from siblings explosion. At the extreme, having an enormous object in a
node can cause reads of that object to crash the entire node. Other
issues include [[undue latency|Latency Reduction Checklist]] and
out-of-memory errors.

To prevent sibling explosion, we recommend the following:

1. Use [[dotted version vectors|Causal Context#Dotted-Version-Vectors]]
instead of [[vector clocks|Causal Context#Vector-Clocks]] for causal
context.
2. Always update mutable objects within a read/modify/write cycle. More
information can be found in the [[Object Updates]] doc.

## Resources

* [Evaluating Dotted Version Vectors in Riak](http://asc.di.fct.unl.pt/~nmp/pubs/inforum-2011-2.pdf)
* [Improving Logical Clocks in Riak with Dotted Version Vectors: A Case Study](http://paginas.fe.up.pt/~prodei/dsie12/papers/paper_19.pdf)
* [Dotted Version Vector Sets](https://github.com/ricardobcl/Dotted-Version-Vectors)
* [A History of Time in Riak](https://www.youtube.com/watch?v=3SWSw3mKApM)
