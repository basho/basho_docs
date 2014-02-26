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

If a loss of replicas occurs, you may need to run a Riak KV repair on one or more partitions.  Typically, Riak KV repairs are run in situations where partitions or nodes are lost due to corruption or hardware failure.  In these cases, when the partition or node are brought back online, they are done so without any data.  The need to run this command will depend mainly on use case and whether Active Anti-Entropy is enabled or not.

**You need to run a repair if both of the below are true:**

- Active Anti-Entropy is disabled.
- You have non-expiring data, and have keys that are not accessed frequently.

**You may not need to run a repair if any of the below are true:**

- Active Anti-Entropy is enabled.
- Your entire key set is accessed frequently, allowing passive read-repair to repair the partitions.
- Your data expires frequently.

In most cases, using Active Anti-Entropy or running a Riak KV repair with the below instructions is suggested.

## Running a Repair

The Riak KV repair operation will repair objects from a node's adjacent partitions on the ring, consequently fixing the index.  This is done as efficiently as possible by generating a hash range for all the buckets and thus avoiding a preflist calculation for each key. Only a hash of each key is done, its range determined from a bucket->range map, and then the hash is checked against the range.

Repairs are not allowed to occur during ownership changes.  Since ownership entails the moving of partition data it is safest to make them mutually exclusive events.  If you join or remove a node all repairs across the entire cluster will be killed.

### Repairing a Single Partition

In the case of data loss in a single partition, just that partition can be repaired.

1. From any node in the cluster, attach to Riak:

    ```bash
    riak attach
    ```

    You may have to hit enter again to get a console prompt.

2. Execute the repair for a single partition using the below command:

    ```erlang
    riak_kv_vnode:repair(<Partition_ID>).
    ```

    where `<Partition_ID>` is replaced by the ID of the partition to repair.  For example:

    ```erlang
    riak_kv_vnode:repair(251195593916248939066258330623111144003363405824).
    ```

3.  Once the command has been executed, detach from Riak using {{#1.3.2-}}`Control-D`{{/1.3.2-}}{{#1.4.0+}}`Control-C`{{/1.4.0+}}.

### Repairing All Partitions on a Node

If a node is lost, all partitions that node currently owns can be repaired.

1. From any node in the cluster, attach to Riak:

    ```bash
    riak attach
    ```

2. Get a copy of the current Ring:

    ```erlang
    {ok, Ring} = riak_core_ring_manager:get_my_ring().
    ```

    You will get a lot of output with Ring record information. You can safely ignore it.

3. Get a list of partitions owned by the node that needs to be repaired. Replace 'dev1@127.0.0.1' with the name of the node to be repaired.  The name can be found in the vm.args, specified as the `-name` parameter.

    ```erlang
    Partitions = [P || {P, 'dev1@127.0.0.1'} <- riak_core_ring:all_owners(Ring)].
    ```

    _Note: The above is an [Erlang list comprehension](http://www.erlang.org/doc/programming_examples/list_comprehensions.html), that loops over each `{Partition, Node}` tuple in the Ring, and extracts only the partitions that match the given node name, as a list._

4. Execute the repair on all the partitions. Executing the repairs all at once will cause a lot of `{shutdown, max_concurrency}` messages in the logs. These can be safely ingored, as it is just the transfers mechanism enforcing an upper limit on the number of concurrent transfers.

    ```erlang
    [riak_kv_vnode:repair(P) || P <- Partitions].
    ```
5. Once the command has been executed, detach from Riak using {{#1.3.2-}}`Control-D`{{/1.3.2-}}{{#1.4.0+}}`Control-C`{{/1.4.0+}}.

## Monitoring Repairs

The above repair commands can be monitored via the `riak-admin transfers` command.

## Killing a Repair

Currently there is no easy way to kill an individual repair.  The only option is to kill all repairs targeting a given node.  This is done by running `riak_core_vnode_manager:kill_repairs(Reason)` on the node undergoing repair.  This command can be executed from a `riak attach` session like below:

```erlang
riak_core_vnode_manager:kill_repairs(killed_by_user).
```

Log entries will reflect that repairs were killed manually, and will look similar to:

```
2012-08-10 10:14:50.529 [warning] <0.154.0>@riak_core_vnode_manager:handle_cast:395 Killing all repairs: killed_by_user
```

Repairs on a node can also be killed remotely from another node in the cluster.  From a `riak attach` session the below command can be used:

```erlang
rpc:call('dev1@127.0.0.1', riak_core_vnode_manager, kill_repairs, [killed_by_user]).
```




