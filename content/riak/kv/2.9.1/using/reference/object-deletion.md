---
title: "Object Deletion Reference"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "Object Deletion"
    identifier: "managing_ref_object_deletion"
    weight: 103
    parent: "managing_ref"
toc: true
aliases:
  - /riak/2.9.1/ops/advanced/deletion
---

[concept eventual consistency]: ../../../learn/concepts/eventual-consistency
[concept clusters]: ../../../learn/concepts/clusters
[glossary vnode]: ../../../learn/glossary/#vnode
[usage delete objects]: ../../../developing/usage/deleting-objects
[developing keylist]: ../../../developing/api/http/list-keys
[developing mapreduce]: ../../../developing/usage/mapreduce
[cluster mdc]: ../../cluster-operations/v3-multi-datacenter
[config advanced]: ../../../configuring/reference/#advanced-configuration
[glossary sloppy quorum]: ../../../learn/glossary/#sloppy-quorum
[bitcask merging]: ../../../setup/planning/backend/bitcask/#disk-usage-and-merging-settings
[leveldb compaction]: ../../../setup/planning/backend/leveldb/#compaction

In single-server, non-clustered data storage systems, object deletion
is a trivial process. In an [eventually consistent][concept eventual consistency], [clustered][concept clusters] system like Riak, however,
object deletion is far less trivial because objects live on multiple
[nodes](../../../learn/glossary/#nodes), which means that a deletion process must be chosen to determine when an object can be removed from the storage backend.

## Object Deletion Example

The problem of object deletion in distributed systems can be illustrated more concretely using the following example:

* An object is stored on nodes A, B, and C
* Node C suddenly goes offline due to a network failure
* A client sends a delete request to node A, which forwards that
  request to node B, but it cannot reach node C
* On nodes A and B, the object is deleted
* Node C comes back online
* A client attempts to read the object, and the request hits node C
* Node C asks nodes A and B for the object, but they return `not_found`. Node C, on the other hand, still has the object.

The question here is: how should node C respond to the client? Given only the above information, it isn't possible to determine which of two possible scenarios actually occurred:

1. the object was deleted on A & B but not on C
2. the object was created on C but not on A & B

To get around this problem, Riak uses *Tombstones*.

## Tombstones

Riak addresses the problem of deletion in distributed systems by replacing the deleted object with a special object called a **tombstone** rather than just removing it.

This allows Riak to understand the difference between an object that has been deleted, and one that was never written in the first place. A tombstone specifically has `X-Riak-Deleted` = `true` in the metadata and a value of `<<>>` (the Erlang empty binary) in its contents, whereas an unwritten object has *no entry at all*.

The problem with tombstones is that they take up space, albeit not very much. For this reason, Riak can be configured to automatically remove tombstones after a set period of time. This process is called **reaping**.

After being reaped, a tombstone is completely removed, and the object entry ceases to exist entirely (as if it had never been written to).

## Configuring Object Deletion

The `delete_mode` setting in a cluster's [configuration files][config advanced] will determine how long a tombstone will remain before being reaped.

There are three possible settings:

* `keep` --- Disables tombstone removal
* `immediate` --- The tombstone is removed as soon as the request is
  received
* Custom time interval --- How long to wait until the tombstone is
  removed, expressed in milliseconds. The default is `3000`, i.e. to
  wait 3 seconds

In general, we recommend setting the `delete_mode` parameter to `keep`
if you plan to delete and recreate objects under the same key. This protects against failure scenario cases in which a deleted object may be resurrected.

Setting `delete_mode` to `immediate` can be useful in situations in
which an aggressive space reclamation process is necessary, such as
when running [MapReduce jobs][developing mapreduce], but we do not recommend
this in general.

Setting `delete_mode` to a longer time duration than the default can be
useful in certain cases involving [Multi-Datacenter Replication][cluster mdc], e.g. when
network connectivity is an issue.

## Deletion from Backends

When attempting to reclaim disk space, deleting data may seem like the obvious first step. However, in Riak this is not necessarily the best thing to do if the disk is nearly full. This is because Riak's disk-based backends don't remove data immediately. This is true both for the initial deletion when a Riak tombstone is created, and later when that tombstone is reaped.

In the case of Bitcask, a new entry is written in the log with either the Riak tombstone or, after reaping, a Bitcask tombstone. The in-memory key-pointer is then updated to point to this new value.

In LevelDB, a newly written value obscures the earlier value. Again, this is either the Riak tombstone or, after reaping, a LevelDB tombstone.

Some time later, the backends will perform their regular garbage collection procedures. For Bitcask this is [merging][bitcask merging], for LevelDB it is [compaction][leveldb compaction]. At this time, stale entries containing the original objects will be purged from disk, along with any Bitcask or LevelDB tombstones. Riak tombstones will *not* be purged, because the backends treat them like regular objects.

Thus, reclaiming disk space is not immediate with respect to delete operations, nor even with respect to reaping, and prior to garbage collection delete operations will actually cause disk space usage to rise slightly.

## Tombstones & Reporting

When designing applications and operating a live Riak cluster, it is important to know how to interpret Riak's responses to requests. With respect to deletion and tombstones, please note the following:

* A delete request is considered a special case of an update. It will fail if the `W` and `PW` values are not satisfied. However, due to [Sloppy Quorum][glossary sloppy quorum], deletes will usually succeed. This does not mean that tombstones have been written over *all* copies of the object, but rather that tombstones have been written on at least `W` nodes, of which at least `PW` are primaries.
* Successful delete requests do not guarantee successful reaping. If a node fails before its reap timer expires, the reap timer will not automatically recommence upon restart. Rather, the tombstone will remain upon the node until a further request finds it. At this time, a new reap timer will be initiated.
* A GET request that sees a quorum of Riak tombstones will return a `not_found` response to the client, even though internally Riak knows there used to be an object there.
* A GET request will never see backend tombstones, because the backends report these as `not_found`. To RiakKV, such answers will appear as if the object has never been written. A `not_found` will be sent up to the client in this case, too.
* A [Keylist][developing keylist] or [MapReduce][developing mapreduce] operation *will* return Riak tombstones, but *not* backend tombstones. This is because these operations fold over the backends directly, and make no attempt to filter Riak tombstones out of the fold by default.

## Client Library Examples

Check out [Deleting Objects][usage delete objects] in the Developing section for examples of deleting objects client-side.

## Resources

* [Discussion on the Riak mailing list](http://lists.basho.com/pipermail/riak-users_lists.basho.com/2011-October/006048.html)
