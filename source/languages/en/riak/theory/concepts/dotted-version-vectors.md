---
title: Dotted Version Vectors
project: riak
version: 2.0.0+
document: appendix
audience: advanced
---

When using Riak as an [[eventually consistent|Eventual Consistency]]---
as opposed to [[strongly consistent|Strong Consistency]]---system, you
will need to select a means of dealing with [[object replica
conflicts|Conflict Resolution]], which are inevitable in a multi-server,
[[clustered|Clusters]] system like Riak.

In versions of Riak prior to 2.0, conflict resolution, whether on the
client side or in Riak, takes place using [[vector clocks]]. Vector
clocks are a mechanism for tracking object update causality in terms of
**logical time** rather than chronological time, enabling Riak to make
decisions about which objects are to be deemed correct in cases of
conflict.

In Riak 2.0 and later, vector clocks can still be used for conflict
resolution, but there is now the added option of using dotted version
vectors (DVVs) instead.

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

## Versus Vector Clocks

The role that DVVs play in Riak is directly analogous to that of
[[vector clocks]], as both are used to resolve object conflicts, whether
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

## Resources

* [DVV Paper: "Scalable and Accurate Causality Tracking for Eventually Consistent Stores"](http://haslab.uminho.pt/tome/files/dvvset-dais.pdf)
* [DVV Github](https://github.com/ricardobcl/Dotted-Version-Vectors)
* [Improving Logical Clocks in Riak with Dotted Version Vectors: A Case Study](http://paginas.fe.up.pt/~prodei/dsie12/papers/paper_19.pdf)
