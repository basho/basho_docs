---
title: "Repairs"
description: ""
project: "riak_kv"
project_version: 2.9.4
menu:
  riak_kv-2.9.4:
    name: "Repairs"
    identifier: "repair_recover_repairs"
    weight: 102
    parent: "managing_repair_recover"
toc: true
aliases:
  - /riak/2.9.4/ops/running/recovery/repairing-indexes
  - /riak/kv/2.9.4/ops/running/recovery/repairing-indexes
  - /riak/2.9.4/ops/running/recovery/failed-node
  - /riak/kv/2.9.4/ops/running/recovery/failed-node
  - /riak/2.9.4/ops/running/recovery/repairing-leveldb
  - /riak/kv/2.9.4/ops/running/recovery/repairing-leveldb
  - /riak/2.9.4/ops/running/recovery/repairing-partitions
  - /riak/kv/2.9.4/ops/running/recovery/repairing-partitions
---

[cluster ops aae]: {{<baseurl>}}riak/kv/2.9.4/using/cluster-operations/active-anti-entropy/
[config ref]: {{<baseurl>}}riak/kv/2.9.4/configuring/reference/
[Erlang shell]: http://learnyousomeerlang.com/starting-out
[glossary AAE]: {{<baseurl>}}riak/kv/2.9.4/learn/glossary/#active-anti-entropy-aae
[glossary readrep]: {{<baseurl>}}riak/kv/2.9.4/learn/glossary/#read-repair
[search config]: {{<baseurl>}}riak/kv/2.9.4/configuring/search/#search-config-settings
[tiered storage]: {{<baseurl>}}riak/kv/2.9.4/setup/planning/backend/leveldb/#tiered-storage



## Repairing Search Indexes

Riak search indexes are repaired whenever objects are corrected by [read repair][glossary readrep].

[Active anti-entropy (AAE)][glossary AAE] is provided for Riak search.

Riak KV's [configuration for AAE][cluster ops aae] will be used for Riak search's AAE hashtrees by default.

Riak search can be provided its own AAE settings in the [search config settings][search config].

## Repairing Secondary Indexes

The `riak-admin repair-2i` command can be used to repair any stale or missing secondary indexes.  This command scans and repairs any mismatches between the secondary index data used for querying and the secondary index data stored in the Riak objects. It can be run on all partitions of a node or on a subset of them.  We recommend scheduling these repairs outside of peak load time.

### Running a Repair

The secondary indexes of a single partition can be repaired by executing:

```bash
riak-admin repair-2i »Partition ID«
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

## Repairing LevelDB

In the event of major hardware or filesystem problems, LevelDB can become corrupted. These failures are uncommon, but they could happen, as heavy loads can push I/O limits.

### Checking for Compaction Errors

Any time there is a compaction error, it will be noted in the LevelDB logs. Those logs are located in a `LOG` file in each instance of LevelDB in a Riak node, specifically in `#(platform_data_dir)/leveldb/<vnode>/LOG`. The `platform_data_dir` can be specified in the [`riak.conf`][config ref] configuration file. The default is `./data`.

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


{{% note %}}
While corruption on one vnode is not uncommon, corruption in several vnodes very likely means that there is a deeper problem that needs to be address, perhaps on the OS or hardware level.
{{% /note %}}


## Healing Corrupted LevelDBs

When you have discovered corruption in your LevelDB backend, the steps you take to resolve it will depend on whether you are using [tiered storage] or not.

Choose your setup below: 

1. [Just LevelDB](#leveldb)
2. [LevelDB with tiered storage](#leveldb-with-tiered-storage)


### LevelDB

Follow the steps below to heal your corrupted LevelDB.

1\. Stop the node:

```bash
riak stop
```

2\. To repair the corrupted LevelDB through the [Erlang shell],  you will run the the `riak ertspath` command to output the path to Riak's internal Erlang runtime, and the `erl` command to start the Erlang shell. You can run them in a single command: 

```bash
`riak ertspath`/erl
```

{{% note title="Erlang version" %}}
Note, you must start up the Erlang shell using the same version of Erlang packaged with Riak. The above command will make sure you do so. If you choose not to use the above command please pay close attention to the version and location you use with the `erl` command.
{{% /note %}}

3\. Once in the shell, run the following command:

```erlang
application:set_env(eleveldb, data_root, "").
```

4\. Then set `Options` equal to an empty list:

```erlang
Options = [].
```

5\. Set some supportive variables for the repair process.  These will be custom to your environment and specific repair needs.
VNodeList should be a list of each corrupted LevelDB that you found using the [`find` command above](#checking-for-compaction-errors).

```erlang
DataRoot = "»path to your data root«".
VNodeList = ["»vnode id you want to repair«", ...].
```

6\. Run the following commands, which will parse the information you provided and run eleveldb:repair over all of the VNode IDs that you listed in VNodeList.

```erlang
RepairPath = fun(DataRoot, VNodeNumber) -> Path = lists:flatten(DataRoot ++ "/" ++ VNodeNumber), io:format("Repairing ~s.~n",[Path]), Path end.
[eleveldb:repair(RepairPath(DataRoot, VNodeList), Options) || VNodeNumber <- VNodeList].
```

7\. This process may take several minutes. When it has completed successfully, you can restart the node and continue as usual.

```bash
riak start
```

### LevelDB with Tiered Storage

Follow the steps below to heal your corrupted LevelDB.

1\. Stop the node:

```bash
riak stop
```

2\. Check your riak.conf file and make note of the following values:

* leveldb.tiered (integer)
* leveldb.tiered.path.fast
* leveldb.tiered.path.slow

3\. To repair the corrupted LevelDB through the [Erlang shell],  you will run the the `riak ertspath` command to output the path to Riak's internal Erlang runtime, and the `erl` command to start the Erlang shell. You can run them in a single command: 

```bash
`riak ertspath`/erl
```

{{% note title="Erlang version" %}}
Note, you must start up the Erlang shell using the same version of Erlang packaged with Riak. The above command will make sure you do so. If you choose not to use the above command please pay close attention to the version and location you use with the `erl` command.
{{% /note %}}

4\. Once in the shell, run the following command:

```erlang
application:set_env(eleveldb, data_root, "").
```

5\. Then supply the information you noted in Step 2:

```erlang
Options = [
  {tiered_slow_level, »leveldb.tiered value«},    
  {tiered_fast_prefix, "»leveldb.tiered.path.fast value«"},
  {tiered_slow_prefix, "»leveldb.tiered.path.slow value«"}
].
```

6\. Set some supportive variables for the repair process.  These will be custom to your environment and specific repair needs.
VNodeList should be a list of each corrupted LevelDB partitions that you found using the [`find` command above](#checking-for-compaction-errors) provided in double quotes.

```erlang
DataRoot = "»path to your data root«".
VNodeList = ["»vnode id you want to repair«", ...].
```

7\. Run the following commands, which will parse the information you provided and run eleveldb:repair over all of the VNode IDs that you listed in VNodeList.

```erlang
RepairPath = fun(DataRoot, VNodeNumber) -> Path = lists:flatten(DataRoot ++ "/" ++ VNodeNumber), io:format("Repairing ~s.~n",[Path]), Path end.
[eleveldb:repair(RepairPath(DataRoot, VNodeList), Options) || VNodeNumber <- VNodeList].
```
8\. This process may take several minutes. When it has completed successfully, you can restart the node and continue as usual.

```bash
riak start
```


## Repairing Partitions

If you have experienced a loss of object replicas in your cluster, you
may need to perform a repair operation on one or more of your data
[partitions]({{<baseurl>}}riak/kv/2.9.4/learn/concepts/clusters/#the-ring). Repairs of Riak KV data are typically
run in situations where partitions or whole nodes are lost due to
corruption or hardware failure. In these cases, nodes or partitions are
brought back online without any data, which means that the need to
repair data will depend mainly on your use case and on whether [active anti-entropy]({{<baseurl>}}riak/kv/2.9.4/learn/concepts/active-anti-entropy/) is enabled.

You will need to run a repair if the following are both true:

* Active anti-entropy is [disabled]({{<baseurl>}}riak/kv/2.9.4/learn/concepts/active-anti-entropy/#disabling-active-anti-entropy)
* You have both non-expiring data and keys that are not accessed
  frequently (which means that they are not likely to be subject to
  [read repair]({{<baseurl>}}riak/kv/2.9.4/learn/concepts/active-anti-entropy/#read-repair-vs-active-anti-entropy))

You will most likely not need to run a repair operation if _any_ of the
following is true:

* Active anti-entropy is [enabled]({{<baseurl>}}riak/kv/2.9.4/learn/concepts/active-anti-entropy/#enabling-active-anti-entropy)
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
    riak_kv_vnode:repair(»Partition ID«).
    ```

    where `»Partition_ID«` is replaced by the ID of the partition to
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

