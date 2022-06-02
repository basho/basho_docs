---
title: "Repairs"
description: ""
project: "riak_kv"
project_version: "2.0.4"
menu:
  riak_kv-2.0.4:
    name: "Repairs"
    identifier: "repair_recover_repairs"
    weight: 102
    parent: "managing_repair_recover"
toc: true
aliases:
  - /riak/2.0.4/ops/running/recovery/repairing-indexes
  - /riak/kv/2.0.4/ops/running/recovery/repairing-indexes
  - /riak/2.0.4/ops/running/recovery/failed-node
  - /riak/kv/2.0.4/ops/running/recovery/failed-node
  - /riak/2.0.4/ops/running/recovery/repairing-leveldb
  - /riak/kv/2.0.4/ops/running/recovery/repairing-leveldb
  - /riak/2.0.4/ops/running/recovery/repairing-partitions
  - /riak/kv/2.0.4/ops/running/recovery/repairing-partitions
---

## Repairing Secondary Indexes

The `riak-admin repair-2i` command can be used to repair any stale or missing secondary indexes.  This command scans and repairs any mismatches between the secondary index data used for querying and the secondary index data stored in the Riak objects. It can be run on all partitions of a node or on a subset of them.  We recommend scheduling these repairs outside of peak load time.

### Running a Repair

The secondary indexes of a single partition can be repaired by executing:

```bash
riak-admin repair-2i <Partition_ID>
```

The secondary indexes of every partition can be repaired by executing the same command, without a partition ID:

```bash
riak-admin repair-2i
```

### Monitoring a Repair

Repairs can be monitored using the below command:

```bash
riak-admin repair-2i status
```

### Killing a Repair

In the event the secondary index repair operation needs to be halted, all repairs can be killed with:

```bash
riak-admin repair-2i kill
```

## Repairing Search Indexes

Riak Search indexes currently have no form of anti-entropy (such as read-repair). Furthermore, for performance and load balancing reasons, Search reads from one random node. This means that when a replica loss has occurred, inconsistent results may be returned.

### Running a Repair

If a replica loss has occurred, you need to run the repair command. This command repairs objects from a node's adjacent partitions on the ring, consequently fixing the search index.

This is done as efficiently as possible by generating a hash range for all the buckets and thus avoiding a preflist calculation for each key. Only a hash of each key is done, its range determined from a bucket&rarr;range map, and then the hash is checked against the range.

This code will force all keys in each partition on a node to be reread, thus rebuilding the search index properly.

1. From a cluster node with Riak installed, attach to the Riak console:

    ```bash
    riak attach
    ```

    You may have to hit enter again to get a console prompt.

2. Get a list of partitions owned by the node that needs repair:

    ```erlang
    {ok, Ring} = riak_core_ring_manager:get_my_ring().
    ```

    You will get a lot of output with Ring record information. You can safely ignore it.

3. Then run the following code to get a list of partitions. Replace 'dev1@127.0.0.1' with the name of the node you need to repair.

    ```erlang
    Partitions = [P || {P, 'dev1@127.0.0.1'} <- riak_core_ring:all_owners(Ring)].
    ```

    _Note: The above is an [Erlang list comprehension](http://www.erlang.org/doc/programming_examples/list_comprehensions.html), that loops over each `{Partition, Node}` tuple in the Ring, and extracts only the partitions that match the given node name, as a list._

4. Execute repair on all the partitions. Executing them all at once like this will cause a lot of `{shutdown,max_concurrency}` spam but it's not anything to worry about. That is just the transfers mechanism enforcing an upper limit on the number of concurrent transactions.

    ```erlang
    [riak_search_vnode:repair(P) || P <- Partitions].
    ```

5. When you're done, press `Ctrl-D` to disconnect the console. DO NOT RUN q() which will cause the running Riak node to quit. Note that `Ctrl-D` merely disconnects the console from the service, it does not stop the code from running.


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

## Repairing LevelDB

In the event of major hardware or filesystem problems, LevelDB can become corrupted. These failures are uncommon, but they could happen, as heavy loads can push I/O limits.

### Checking for Compaction Errors

Any time there is a compaction error, it will be noted in the LevelDB logs. Those logs are located in a `LOG` file in each instance of LevelDB in a Riak node, specifically in `#(platform_data_dir)/leveldb/<vnode>/LOG`. The `platform_data_dir` can be specified in the [`riak.conf`]({{<baseurl>}}riak/kv/2.0.4/configuring/reference/) configuration file. The default is `./data`.

Compaction error messages take the following form:

```
<timestamp> Compaction Error: Corruption: corrupted compressed block contents
```

To check whether your node has experienced such errors, you will need to run a script that searches for `Compaction Error` in each `LOG` file. Here is an example script:

```bash
find . -name "LOG" -exec grep -l 'Compaction error' {} \;
```

If there are compaction errors in any of your vnodes, those will be listed in the console. If any vnode has experienced such errors, you would see output like this:

```
./442446784738847563128068650529343492278651453440/LOG 
```

{{% note title="Note" %}}
While corruption on one vnode is not uncommon, corruption in several vnodes
very likely means that there is a deeper problem that needs to be address,
perhaps on the OS or hardware level.
{{% /note %}}

## Healing Corrupted LevelDBs

The first step in properly addressing this problem is to stop the node.

```bash
riak stop
```

Repairing the corrupted LevelDB can be done through the [Erlang shell](http://learnyousomeerlang.com/starting-out). Do not start Riak at this point; use the shell only.

You can fire up the shell by running the `erl` command. To ensure that you start up the shell using the same version of Erlang that's embedded with Riak, you should run the `erl` command as an absolute path. Here's an example:

```bash
/opt/local/riak/erts-5.8.5/bin/erl
```

Once you're in the shell, run the following command:

```erlang
[application:set_env(eleveldb, Var, Val) || {Var, Val} <- 
    [{max_open_files, 2000}, 
     {block_size, 1048576}, 
     {cache_size, 20*1024*1024*1024}, 
     {sync, false}, 
     {data_root, ""}]].
```

For each corrupted LevelDB that you found using the `find` command (as demonstrated above), run the following `repair` command, substituting the path to your LevelDB vnodes and the appropriate vnode number:

```erlang
eleveldb:repair("/path-to-vnode/<vnode_number>", []).
```

This process will likely take several minutes. When it has completed successfully, you can restart the node and continue as usual.

```bash
riak start
```

## Repairing Partitions

If you have experienced a loss of object replicas in your cluster, you
may need to perform a repair operation on one or more of your data
[partitions]({{<baseurl>}}riak/kv/2.0.4/learn/concepts/clusters/#the-ring). Repairs of Riak KV data are typically
run in situations where partitions or whole nodes are lost due to
corruption or hardware failure. In these cases, nodes or partitions are
brought back online without any data, which means that the need to
repair data will depend mainly on your use case and on whether [active anti-entropy]({{<baseurl>}}riak/kv/2.0.4/learn/concepts/active-anti-entropy/) is enabled.

You will need to run a repair if the following are both true:

* Active anti-entropy is [disabled]({{<baseurl>}}riak/kv/2.0.4/learn/concepts/active-anti-entropy/#disabling-active-anti-entropy)
* You have both non-expiring data and keys that are not accessed
  frequently (which means that they are not likely to be subject to
  [read repair]({{<baseurl>}}riak/kv/2.0.4/learn/concepts/active-anti-entropy/#read-repair-vs-active-anti-entropy))

You will most likely not need to run a repair operation if _any_ of the
following is true:

* Active anti-entropy is [enabled]({{<baseurl>}}riak/kv/2.0.4/learn/concepts/active-anti-entropy/#enabling-active-anti-entropy)
* Your entire key set is accessed frequently, allowing passive read
  repair to repair the partitions
* Your data expires frequently

In most cases, we recommend either using active anti-entropy or, if
necessary and only when necessary, running a repair operation using the
instructions below.

### Running a Repair

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

### Monitoring Repairs

The above repair commands can be monitored via the `riak-admin
transfers` command.

### Killing a Repair

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
