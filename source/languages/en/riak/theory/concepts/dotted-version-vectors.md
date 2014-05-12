---
title: Dotted Version Vectors
project: riak
version: 2.0.0+
document: appendix
audience: advanced
---

When storing data in [[eventually consistent|Eventual Consistency]]---as
opposed to [[strongly consistent|Strong Consistency]]---systems,
applications need to select a means of dealing with [[object replica conflicts|Conflict Resolution]],
which are always a possibility due to the very nature of multi-node,
[[clustered|Clusters]] systems.

In versions of Riak prior to 2.0, conflict resolution, whether on the
client side or in Riak, took place using [[vector clocks]]. Vector
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
level, [[using bucket types]], which means that you can employ a mixed
conflict resolution strategy in your Riak cluster if you wish. DVVs can
be enabled by setting the <tt>dvv_enabled</tt> bucket property to
<tt>true</tt>.

Vector clocks remain the default if you are not using bucket types.
However, any bucket type that you create and activate will have
<tt>dvv_enabled</tt> set to <tt>true</tt>. And so if you wish to create
a bucket type that uses traditional vector clocks, you will need to
explicitly set <tt>dvv_enabled</tt> to <tt>false</tt> for that bucket
type.
</div>

## Versus Vector Clocks

The role that DVVs play in Riak is directly analogous to that of
[[vector clocks]], as both are used to resolve object conflicts, whether
during background operations like [[active anti-entropy]] or
[[read repair|Riak Glossary#read-repair]], or when applications engage
in client-side [[conflict resolution]].

There are, however, a few core differences:

* DVVs are better than vector clocks at preventing [[sibling explosion|Vector Clocks#sibling-explosion]] because they contain more granular information. The difference is that in the case of causally concurrent values, all values are kept.
* DVVs are more scalable than vector clocks because they are both more lightweight in terms of raw byte length and also in terms of how they grow in size. In contrast to vector clocks, the size of DVVs remains proportional to their objects' replication degree.

## Resources

* [Evaluating Dotted Version Vectors in Riak](http://asc.di.fct.unl.pt/~nmp/pubs/inforum-2011-2.pdf)
* [Improving Logical Clocks in Riak with Dotted Version Vectors: A Case Study](http://paginas.fe.up.pt/~prodei/dsie12/papers/paper_19.pdf)
* [Dotted Version Vector Sets](https://github.com/ricardobcl/Dotted-Version-Vectors)
