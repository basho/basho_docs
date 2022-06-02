---
title: "Object Deletion Reference"
description: ""
project: "riak_kv"
project_version: "2.0.0"
menu:
  riak_kv-2.0.0:
    name: "Object Deletion"
    identifier: "managing_ref_object_deletion"
    weight: 103
    parent: "managing_ref"
toc: true
aliases:
  - /riak/2.0.0/ops/advanced/deletion
  - /riak/kv/2.0.0/ops/advanced/deletion
---

[concept clusters]: ../../../learn/concepts/clusters
[glossary vnode]: ../../../learn/glossary/#vnode
[usage delete objects]: ../../../developing/usage/deleting-objects

In single-server, non-clustered data storage systems, object deletion
is a trivial process. In an [eventually consistent](../../../learn/concepts/eventual-consistency), [clustered][concept clusters] system like Riak, however,
object deletion is far less trivial because objects live on multiple
[nodes](../../../learn/glossary/#nodes), which means that a deletion process must be chosen to determine when an object can be removed from the storage backend.

## Object Deletion Example Scenario

The problem of object deletion in Riak can be illustrated more
concretely using the following example:

* An object is stored on nodes A, B, and C
* Node C suddenly goes offline
* A Riak client sends a delete request to node A, which forwards that
  request to node B
* On nodes A and B, the object is marked as deleted with a
  [tombstone](#tombstones)
* Node C comes back online
* The object has been marked as deleted on nodes A and B, but it still
  lives on node C
* A client attempts to read the object, Riak senses that there are
  divergent replicas and initiates a repair process (either [read repair](../../../learn/concepts/active-anti-entropy/#read-repair-vs-active-anti-entropy) or [active anti-entropy](../../../learn/concepts/active-anti-entropy/),
  depending on configuration)

At this point, Riak needs to make a decision about what to do. Should
node C be instructed to delete the object as well? Should nodes A and B
be instructed to reinstate the object so that it lives on all three
nodes again?

What happens in this scenario depends on how you have configured Riak to
handle deletion. More on configuration can be found in the
[section below](#configuring-object-deletion).

## Tombstones

Riak addresses the problem of deletion in distributed systems by marking
deleted objects with a so-called **tombstone**. This means that an
`X-Riak-Deleted` metadata key is added to the object and given the value 
`true`, while the object itself is set to an empty Erlang object,
i.e. `<<>>`.

When a delete request is sent to Riak, the following process is set in
motion:

1. A tombstone object (`<<>>`) is written to N [vnodes][glossary vnode], with N
   defined by [`n_val`](../../../developing/app-guide/replication-properties#n-value-and-replication)
2. If all N vnodes store the tombstone, the object is removed
3. If fallback vnodes are in use, the object will not be immediately
   removed

## Configuring Object Deletion

If step 3 in the [process explained above](#tombstones) is reached, the `delete_mode` setting in your [configuration files](../../../configuring/reference/#advanced-configuration) will determine what happens next. This
setting determines how long Riak will wait after identifying an object
for deletion and actually removing the object from the storage backend.

There are three possible settings:

* `keep` --- Disables tombstone removal; protects against an edge case
  in which an object is deleted and recreated on the owning
  [vnodes][glossary vnode] while a fallback is either down or awaiting handoff
* `immediate` --- The tombstone is removed as soon as the request is
  received
* Custom time interval --- How long to wait until the tombstone is
  removed, expressed in milliseconds. The default is `3000`, i.e. to
  wait 3 seconds

In general, we recommend setting the `delete_mode` parameter to `keep`
if you plan to delete and recreate objects under the same key
frequently.

Setting `delete_mode` to `immediate` can be useful in situations in
which an aggressive space reclamation process is necessary, such as
when running [MapReduce jobs](../../../developing/usage/mapreduce/), but we do not recommend
this in general.

Setting `delete_mode` to a longer time duration than the default can be
useful in certain edge cases involving [Multi-Datacenter Replication](../../cluster-operations/v3-multi-datacenter), e.g. when
network connectivity is an issue.

Please note that there is an edge case where tombstones will remain
stored in the backend, even if the time interval-based `delete_mode` is
used. This occurs if the node is stopped after a tombstone has been
written but before it has been removed from the backend. In this case,
the tombstone will show up in keylisting and MapReduce operations and
will not disappear until you read the key, which has the effect of
making Riak aware of the tombstone. In practice, if `delete_mode`is set
to 10000, all keys that have been deleted during the last 10 seconds
before a node is stopped will remain in the backend.

## Client Library Examples

Check out [Deleting Objects][usage delete objects] in the Developing section for examples of deleting objects client-side.

## Resources

* [Discussion on the Riak mailing list](http://lists.basho.com/pipermail/riak-users_lists.basho.com/2011-October/006048.html)
