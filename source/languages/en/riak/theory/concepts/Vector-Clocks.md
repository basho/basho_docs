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
