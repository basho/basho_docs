---
title: Repairing Partitions
project: riak
version: 1.2.0+
document: tutorial
toc: true
audience: advanced
keywords: [kv, troubleshooting]
moved: {
    '1.4.0-': '/cookbooks/Repairing-KV-Indexes'
}
---

If you have experienced a loss of object replicas in your cluster, you
may need to perform a repair operation on one or more of your data
[[partitions|Clusters#The-Ring]]. Repairs of Riak KV data are typically
run in situations where partitions or whole nodes are lost due to
corruption or hardware failure. In these cases, nodes or partitions are
brought back online without any data, which means that the need to
repair data will depend mainly on your use case and on whether [[active
anti-entropy]] is enabled.

You will need to run a repair if the following are both true:

* Active anti-entropy is [[disabled|Managing Active
  Anti-Entropy#Disabling-Active-Anti-Entropy]]
* You have both non-expiring data and keys that are not accessed
  frequently (which means that they are not likely to be subject to
  [[read repair|Active Anti-Entropy#Read-Repair-vs-Active-Anti-Entropy]])

You will most likely not need to run a repair operation if _any_ of the
following is true:

* Active anti-entropy is [[enabled|Active
  Anti-Entropy#Enabling-Active-Anti-Entropy]]
* Your entire key set is accessed frequently, allowing passive read
  repair to repair the partitions
* Your data expires frequently

In most cases, we recommend either using active anti-entropy or, if
necessary and only when necessary, running a repair operation using the
instructions below.

## Running a Repair

The Riak KV repair operation will repair objects from a node's adjacent
partitions on the ring, consequently fixing the index. This is done as
efficiently as possible by generating a hash range for all the buckets
and thus avoiding a preflist calculation for each key. Only a hash of
each key is done, its range determined from a bucket->range map, and
then the hash is checked against the range.

Repairs are not allowed to occur during ownership changes. Since
ownership entails the moving of partition data it is safest to make them
mutually exclusive events. If you join or remove a node all repairs
across the entire cluster will be killed.

### Repairing a Single Partition

In the case of data loss in a single partition, only that partition can
be repaired.

1. From any node in the cluster, attach to Riak's Erlang shell:

    ```bash
    riak attach
    ```

    You may have to hit **Enter** again to get a console prompt.

2. Execute the repair for a single partition using the below command:

    ```erlang
    riak_kv_vnode:repair(<Partition_ID>).
    ```

    where `<Partition_ID>` is replaced by the ID of the partition to
    repair. For example:

    ```erlang
    riak_kv_vnode:repair(251195593916248939066258330623111144003363405824).
    ```

3. Once the command has been executed, detach from Riak using
`Control-C`.

### Repairing All Partitions on a Node

If a node is lost, all partitions currently owned by that node can be
repaired.

1. From any node in the cluster, attach to Riak's Erlang shell:

    ```bash
    riak attach
    ```

2. Get a copy of the current Ring:

    ```erlang
    {ok, Ring} = riak_core_ring_manager:get_my_ring().
    ```

    You will get a lot of output with ring record information.
    You can safely ignore it.

3. Get a list of partitions owned by the node that needs to be repaired.
Replace `dev1@127.0.0.1` with the name of the node to be repaired.  The
name can be found in each node's `vm.args` file, specified as the
`-name` parameter, if you are using the older configuration system; if
you are using the newer, `riak-conf`-based system, the name is given by
the `nodename` parameter.

    ```erlang
    Partitions = [P || {P, 'dev1@127.0.0.1'} <- riak_core_ring:all_owners(Ring)].
    ```

    **Note**: The above is an [Erlang list
    comprehension](http://www.erlang.org/doc/programming_examples/list_comprehensions.html)
    that loops over each `{Partition, Node}` tuple in the ring and
    extracts only the partitions that match the given node name, as a
    list.


4. Execute the repair on all the partitions. Executing the repairs all
at once will cause a lot of `{shutdown, max_concurrency}` messages in
the logs. These can be safely ingored, as it is just the transfers
mechanism enforcing an upper limit on the number of concurrent
transfers.

    ```erlang
    [riak_kv_vnode:repair(P) || P <- Partitions].
    ```
5. Once the command has been executed, detach from Riak using
`Control-C`.

## Monitoring Repairs

The above repair commands can be monitored via the `riak-admin
transfers` command.

## Killing a Repair

Currently there is no easy way to kill an individual repair. The only
option is to kill all repairs targeting a given node. This is done by
running `riak_core_vnode_manager:kill_repairs(Reason)` on the node
undergoing repair.  This command can be executed from a `riak attach`
session like below:

```erlang
riak_core_vnode_manager:kill_repairs(killed_by_user).
```

Log entries will reflect that repairs were killed manually, and will
look similar to:

```
2012-08-10 10:14:50.529 [warning] <0.154.0>@riak_core_vnode_manager:handle_cast:395 Killing all repairs: killed_by_user
```

Repairs on a node can also be killed remotely from another node in the
cluster. From a `riak attach` session the below command can be used:

```erlang
rpc:call('dev1@127.0.0.1', riak_core_vnode_manager, kill_repairs, [killed_by_user]).
```
