---
title: Object Deletion
project: riak
version: 2.0.0+
document: guide
audience: advanced
keywords: [operators, deletion, delete_mode, tombstones]
---

In single-server, non-clustered data storage systems, object deletion
is a trivial process. In an [[eventually consistent|Eventual Consistency]],
[[clustered|Clusters]] system like Riak, however, object deletion
is less trivial because objects live on multiple [[nodes|Riak Glossary#nodes]],
which means that a deletion process must be chosen to determine when an
object can be removed from the storage backend.

This problem can be illustrated more concretely using the following
example:

* An object is stored on nodes A, B, and C
* Node C suddenly goes offline
* A Riak client sends a delete request to node A, which forwards that request to node B
* Node C comes back online

At this point, a decision needs to be made about whether the object
should be removed from storage. In Riak, that decision can be
configured using [[replication properties]] and the `delete_mode`
parameter.

## Configuring Deletion


## Tombstones

Riak addresses the problem of deletion in distributed systems by marking
deleted objects with a so-called **tombstone**, i.e. a metadata object
`X-Riak-Deleted` with a value of `true` is added, and the value of the
object is set to an empty Erlang object (`<<>>`). When processes like
[[read repair|Active Anti-Entropy#read-repair]]

Most useful when deleting and then re-creating keys rapidly
Default is `keep`

The central problem: Riak needs to keep objects available during
failures by storing copies on multiple nodes; deletion is complex
because Riak must decide what to do when an object is deleted only on
some nodes and not others

## The Deletion Process

Deletion process:

1. A tombstone objects (`<<>>`) is written to N nodes
2. If all N nodes store the tombstone, the object is removed
3. If fallback nodes are in use, the object will not be removed

## Configuring Object Deletion


The `delete_mode` setting provides control over how that process
functions; it dictates what happens in the time window between (a) the 
GET FSM deciding that an object can be removed and (b) that removal
taking place.

Settings:

* `keep` --- Disables tombstone removal; protects against an edge case in which an object is deleted and recreated on the owning nodes while a fallback is either down or awaiting handoff
* `immediate` --- The tombstone is removed as soon as the request is received. 
* interval --- How long to wait until the tombstone is removed

MDC => the problem is that deleted objects can be resurrected when
synchronizing between multiple DCs, especially when connectivity is an
issue; this problem can be avoided using `delete_mode`

Fetching the vclock for a deleted key => setting `deletedvclock` to
`true` via PBC [[PBC Fetch Object]]

## Client Library Examples



```java
FetchValue fetch = new FetchValue.Builder(location)
		.withOption(Option.DELETED_VCLOCK, true)
		.build();
```

```ruby
object.delete
deleted_object = bucket.get('test', 'test', deletedvclock: true)
deleted_object.vclock
```

Fetching a deleted object's vector clock prior to deletion

## Resources

http://lists.basho.com/pipermail/riak-users_lists.basho.com/2011-October/006048.html