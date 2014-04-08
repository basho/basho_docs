---
title: Vector Clocks
project: riak
version: 0.10.0+
document: appendix
toc: true
audience: intermediate
keywords: [appendix, concepts]
moved: {
  '1.4.0-': '/references/appendices/concepts/Vector-Clocks'
}
---

One of Riak's central goals is high availability. It was built as a multi-node system in which any node is capable of receiving requests without requiring that each node participate in each request. In a system like this, it's important to be able to keep track of which version of a value is the most current. This is where vector clocks come in.

## Vector Clocks and Relationships Between Objects

All Riak objects are stored in a location defined by the object's [[bucket|Buckets]] and [[key|Keys and Objects]], as well as the [[bucket type|Using Bucket Types]] defining the bucket's properties. It is possible to [[configure Riak|Conflict Resolution]] to ensure that only one copy of an object ever exists in a specific location. This will ensure that _at most_ one object is returned when a read is performed on a bucket type/bucket/key location (and no objects if Riak returns `not found`).

If Riak is configured this way, Riak may still make use of vector clocks behind the scenes to make intelligent decisions about which replica of an object should be deemed the most recent. vector clocks are a non-issue for clients connecting to Riak.

## Siblings

It is also possible, however, to configure Riak to store multiple objects in a single key. Objects stored this way are called **siblings**.


If you set the `allow_mult` bucket property to `false` for a specific bucket, preferably using [[bucket types|Using Bucket Types]]



to be created when more than one object is written to a location. The problem with siblings, though, is that they _by definition_ conflict with one another. When an application tries to read an object, multiple **replicas** of the object will be stored in the location where it's looking.

Vector clocks enable Riak to compare two objects stored in a specific location, as defined by the object's  , and determine the relationship between the two objects. A number of important aspects of that relationship can be determined using vector clocks:

 * Whether one object is a direct descendant of the other
 * Whether the objects are direct descendants of a common parent
 * Whether the objects are unrelated in recent heritage

Using this knowledge, Riak can auto-repair out-of-sync data when feasible or at least provide a client with an opportunity to reconcile divergent changesets in an application-specific manner.

Vector clocks are non-human-readable and look something like this:

```
a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=
```

## Vector Clock Tagging

When an object is stored in Riak, it is tagged with a vector clock, establishing the object's initial version. For objects that are stored in buckets that do not allow siblings, i.e. buckets with the `allow_mult` property set to `false`,


If the object is in a bucket that allows for siblings, i.e. if `allow_mult` is set to `true`, 

## More Information

Additional background information on vector clocks:

* [[Vector Clocks on Wikipedia|http://en.wikipedia.org/wiki/Vector_clock]]
* [[Why Vector Clocks are Easy|http://blog.basho.com/2010/01/29/why-vector-clocks-are-easy/]]
* [[Why Vector Clocks are Hard|http://blog.basho.com/2010/04/05/why-vector-clocks-are-hard/]]
* The vector clocks used in Riak are based on the [[work of Leslie Lamport|http://portal.acm.org/citation.cfm?id=359563]].
