---
title: "Repairing Secondary Indexes"
description: ""
project: "riak_kv"
project_version: "3.0.10"
lastmod: 2022-05-30T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-3.0.10:
    name: "Repair Secondary Indexes"
    identifier: "repair_recover_2i"
    weight: 105
    parent: "managing_repair_recover"
toc: true
aliases:
  - /riak/3.0.10/ops/running/recovery/repairing-indexes
  - /riak/kv/3.0.10/ops/running/recovery/repairing-indexes
---

The `riak admin repair-2i` command can be used to repair any stale or missing secondary indexes.  This command scans and repairs any mismatches between the secondary index data used for querying and the secondary index data stored in the Riak objects. It can be run on all partitions of a node or on a subset of them.  We recommend scheduling these repairs outside of peak load time.

### Running a Repair

The secondary indexes of a single partition can be repaired by executing:

```bash
riak admin repair-2i <Partition_ID>
```

The secondary indexes of every partition can be repaired by executing the same command, without a partition ID:

```bash
riak admin repair-2i
```

### Monitoring a Repair

Repairs can be monitored using the below command:

```bash
riak admin repair-2i status
```

### Killing a Repair

In the event the secondary index repair operation needs to be halted, all repairs can be killed with:

```bash
riak admin repair-2i kill
```

----

### Monitoring a Repair

The above Repair command can be slow, so if you reattach to the console, you can run the repair_status function. You can use the `Partitions` variable defined above to get the status of every partition.

```erlang
[{P, riak_search_vnode:repair_status(P)} || P <- Partitions].
```

When you're done, press `Ctrl-D` to disconnect the console.

### Killing a Repair

Currently there is no easy way to kill an individual repair.  The only
option is to kill all repairs targeting a given node.  This is done by
running `riak_core_vnode_manager:kill_repairs(Reason)` on the node
undergoing repair.  This means you'll either have to be attached to
that node's console or you can use the `rpc` module to make a remote
call.  Here is an example of killing all repairs targeting partitions
on the local node.

```erlang
riak_core_vnode_manager:kill_repairs(killed_by_user).
```

Log entries will reflect that repairs were killed manually, something akin to this:

```
2012-08-10 10:14:50.529 [warning] <0.154.0>@riak_core_vnode_manager:handle_cast:395 Killing all repairs: killed_by_user
```

Here is an example of executing the call remotely.

```erlang
rpc:call('dev1@127.0.0.1', riak_core_vnode_manager, kill_repairs, [killed_by_user]).
```

When you're done, press `Ctrl-D` to disconnect the console.

Repairs are not allowed to occur during ownership changes.  Since
ownership entails the moving of partition data it is safest to make
them mutually exclusive events.  If you join or remove a node all
repairs across the entire cluster will be killed.

