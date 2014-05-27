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
is far less trivial because objects live on multiple [[nodes|Riak Glossary#nodes]],
which means that a deletion process must be chosen to determine when an
object can be removed from the storage backend.

## Object Deletion Example Scenario

The problem of object deletion in Riak can be illustrated more
concretely using the following example:

* An object is stored on nodes A, B, and C
* Node C suddenly goes offline
* A Riak client sends a delete request to node A, which forwards that request to node B
* On nodes A and B, the object is marked as deleted with a [[tombstone|Object Deletion#tombstones]]
* Node C comes back online
* The object has been marked as deleted on nodes A and B, but it still lives on node C
* A client attempts to read the object, Riak senses that there are divergent replicas and initiates a repair process (either [[read repair|Active Anti-Entropy#read-repair]] or [[active anti-entropy]], depending on configuration)

At this point, Riak needs to make a decision about what to do. Should
node C be instructed to delete the object as well? Should nodes A and B
be instructed to reinstate the object so that it lives on all three
nodes again?

What happens in this scenario depends on how you have configured Riak to
handle deletion. More on configuration can be found in the
[[section below|Object Deletion#configuring-object-deletion]].

## Tombstones

Riak addresses the problem of deletion in distributed systems by marking
deleted objects with a so-called **tombstone**, i.e. an `X-Riak-Deleted`
metadata key is added to the object and given the value `true` while the
object itself is set to an empty Erlang object, `<<>>`.

The causal process behind deletion goes something like this:

1. A tombstone object (`<<>>`) is written to N nodes (with N defined by `[[n_val|Replication Properties#n-value-and-replication]]`)
2. If all N nodes store the tombstone, the object is removed
3. If fallback nodes are in use, the object will not be removed

Step 3 in this process is where 

Most useful when deleting and then re-creating keys rapidly
Default is `keep`

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