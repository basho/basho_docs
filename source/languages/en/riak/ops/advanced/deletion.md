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
deleted objects with a so-called **tombstone**. This means that an
`X-Riak-Deleted` metadata key is added to the object and given the value 
`true`, while the object itself is set to an empty Erlang object,
i.e. `<<>>`.

When a delete request is sent to Riak, the following process is set in
motion:

1. A tombstone object (`<<>>`) is written to N [[vnodes|Riak Glossary#vnode]] (with N defined by `[[n_val|Replication Properties#n-value-and-replication]]`)
2. If all N vnodes store the tombstone, the object is removed
3. If fallback vnodes are in use, the object will not be immediately removed

What this means is that there is always the possibility that a client
can send a delete request to Riak 

## Configuring Object Deletion

If step 3 in the process explained above is reached, the `delete_mode`
setting in your [[configuration files|Configuration Files#advanced-configuration]] will determine what happens
next. This setting determines how long Riak will wait after identifying
an object for deletion and actually removing the object from the storage
backend.

There are three possible settings:

* `keep` --- Disables tombstone removal; protects against an edge case in which an object is deleted and recreated on the owning [[vnodes|Riak Glossary#vnode]] while a fallback is either down or awaiting handoff
* `immediate` --- The tombstone is removed as soon as the request is received. 
* Custom time interval --- How long to wait until the tombstone is removed (the default is to `3s`, i.e. to wait 3 seconds)

In general, we recommend setting the `delete_mode` parameter to `keep`
if you plan to delete and recreate objects under the same key
frequently. This is the default setting.

Setting `delete_mode` to `immediate` can be useful in situations in
which an aggressive space reclamation process is necessary, but we do
not recommend this in general.

Setting `delete_mode` to a specific
time duration can be useful in certain edge cases involving
[[Multi-Datacenter Replication|Multi Data Center Replication v3 Architecture]],
e.g. when network connectivity is an issue. In general, however, we 
recommend keeping `delete_mode` set to the default of `keep`.

## Client Library Examples

If you are updating an object that has been deleted---or if you suspect
that an update might target a deleted object---it is recommended that
you first fetch the [[vector clock]] of the object prior to updating. This
can be done by setting `deletedvclock` parameter to `true` as part of
the [[fetch operation|PBC Fetch Object]]. This can also be done with the
official Riak clients for Ruby, Java, and Erlang, as in the example
below:


```ruby
object.delete
deleted_object = bucket.get('test', 'test', deletedvclock: true)
deleted_object.vclock
```

```python
# It is not currently possible to fetch the vector clock for a deleted
# key in the Python client
```

```java
Location loc = new Location("<bucket>")
		.setBucketType("<bucket_type>")
		.setKey("<key>");
FetchValue fetch = new FetchValue.Builder(loc)
		.withOption(Option.DELETED_VCLOCK, true)
		.build();
FetchValue.Response response = client.execute(fetch);
System.out.println(response.getVclock().asString());
```

```erlang
{ok, Obj} = riakc_pb_socket:get(Pid,
	                            {<<"bucket_type">>, <<"bucket">>},
	                            <<"key">>,
	                            [{deleted_vclock}]).

%% In the Erlang client, the vector clock is accessible using the Obj
%% object obtained above.
```

## Resources

* [Discussion on the Riak mailing list](http://lists.basho.com/pipermail/riak-users_lists.basho.com/2011-October/006048.html)
