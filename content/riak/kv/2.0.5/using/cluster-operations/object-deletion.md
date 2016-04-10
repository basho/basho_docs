---
title: "Object Deletion"
description: ""
project: "riak_kv"
project_version: "2.0.5"
menu:
  riak_kv-2.0.5:
    name: "Object Deletion"
    identifier: "cluster_operations_object_deletion"
    weight: 108
    parent: "managing_cluster_operations"
toc: true
aliases:
  - /riak/2.1.3/ops/advanced/deletion
canonical_link: "docs.basho.com/riak/kv/latest/using/cluster-operations/object-deletion.md"
---

[glossary vnode]: /riak/kv/2.0.5/learn/glossary/#vnode

## Configuring Object Deletion

If step 3 in the process explained above is reached, the `delete_mode`
setting in your [configuration files](/riak/kv/2.0.5/configuring/reference/#advanced-configuration) will determine what happens next. This
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
when running [MapReduce jobs](/riak/kv/2.0.5/developing/usage/mapreduce/), but we do not recommend
this in general.

Setting `delete_mode` to a longer time duration than the default can be
useful in certain edge cases involving [Multi-Datacenter Replication](/riak/kv/2.0.5/using/cluster-operations/v3-multi-datacenter), e.g. when
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
