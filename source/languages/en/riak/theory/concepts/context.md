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
object replicas stored on different nodes is inevitable. In a system
like this, it's important to keep track of which version of a value is
the most current.

One means of achieving this is to use a mechanism like
[timestamps](http://en.wikipedia.org/wiki/Timestamp) to determine which
object is more **chronologically** recent. While Riak [[provides you
this option|Conflict
Resolution#Client-and-Server-side-Conflict-Resolution]], timestamps are
a notoriously unreliable means of determining which object value is most
current in distributed systems. A much more reliable means is to track
the **causal** history of object updates.

## Dotted Version Vectors

## Vector Clocks
