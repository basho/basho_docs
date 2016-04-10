---
title: "Object Deletion Reference"
description: ""
project: "riak_kv"
project_version: "2.0.2"
menu:
  riak_kv-2.0.2:
    name: "Object Deletion"
    identifier: "managing_ref_object_deletion"
    weight: 103
    parent: "managing_ref"
toc: true
aliases:
  - /riak/2.1.3/ops/advanced/deletion
canonical_link: "docs.basho.com/riak/kv/latest/using/reference/object-deletion"
---

[concept clusters]: /riak/kv/2.0.2/learn/concepts/clusters
[glossary vnode]: /riak/kv/2.0.2/learn/glossary/#vnode
[usage delete objects]: /riak/kv/2.0.2/developing/usage/deleting-objects

In single-server, non-clustered data storage systems, object deletion
is a trivial process. In an [eventually consistent](/riak/kv/2.0.2/learn/concepts/eventual-consistency), [clustered][concept clusters] system like Riak, however,
object deletion is far less trivial because objects live on multiple
[nodes](/riak/kv/2.0.2/learn/glossary/#nodes), which means that a deletion process must be chosen to determine when an object can be removed from the storage backend.

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
  divergent replicas and initiates a repair process (either [read repair](/riak/kv/2.0.2/learn/concepts/active-anti-entropy/#read-repair) or [active anti-entropy](/riak/kv/2.0.2/learn/concepts/active-anti-entropy/),
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
   defined by [`n_val`](/riak/kv/2.0.2/developing/app-guide/replication-properties#n-value-and-replication)
2. If all N vnodes store the tombstone, the object is removed
3. If fallback vnodes are in use, the object will not be immediately
   removed

## Client Library Examples

Check out [Deleting Objects][usage delete objects] in the Developing section for examples of deleting objects client-side.

## Resources

* [Discussion on the Riak mailing list](http://lists.basho.com/pipermail/riak-users_lists.basho.com/2011-October/006048.html)
