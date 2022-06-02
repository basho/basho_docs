---
title: "Cluster Administration Commands"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "Cluster Admin Commands"
    identifier: "cluster_admin_commands"
    weight: 100
    parent: "managing_cluster_admin"
toc: true
aliases:
  - /riak/2.9.1/ops/running/cluster-admin
  - /riak/kv/2.9.1/ops/running/cluster-admin
---

[use admin riak-admin#cluster]: {{<baseurl>}}riak/kv/2.9.1/using/admin/riak-admin/#cluster
[concept clusters]: {{<baseurl>}}riak/kv/2.9.1/learn/concepts/clusters
[cluster ops add remove node]: {{<baseurl>}}riak/kv/2.9.1/using/cluster-operations/adding-removing-nodes
[use admin riak-admin#cluster-plan]: {{<baseurl>}}riak/kv/2.9.1/using/admin/riak-admin/#cluster-plan
[use admin riak-admin#cluster-commit]: {{<baseurl>}}riak/kv/2.9.1/using/admin/riak-admin/#cluster-commit


This document explains usage of the [`riak-admin cluster`][use admin riak-admin#cluster] interface, which enables you to perform a wide
variety of cluster-level actions.

## How Cluster Administration Works

Riak provides a multi-phased approach to cluster administration that
enables you to stage and review cluster-level changes prior to
committing them. This allows you to group multiple changes together,
such as adding multiple nodes at once, adding some nodes and removing
others, etc.

Enacting cluster-level changes typically follows this set of steps:

1. Choose an action or set of actions, such as adding a node, removing
multiple nodes, etc. These actions will be **staged** rather than
executed immediately.
1. **Plan** the changes using the [`cluster plan`](#plan) command. This will return a list of staged
commands that you can review.
1. **Commit** the changes using the [`cluster commit`](#commit) command. This will execute the changes that
have been staged and reviewed.

> **Note on command names**
>
> Many of the commands available through the `riak-admin cluster`
interface are also available as self-standing commands. The `riak-admin
member-status` command is now the `riak-admin cluster status` command,
`riak-admin join` is now `riak-admin cluster join`, etc.
>
> We recommend using the `riak-admin cluster` interface over the older,
deprecated commands. You will receive a deprecation warning if you use
the older commands.

## status

Displays a variety of information about the cluster.

```bash
riak-admin cluster status
```

This will return output like the following in a 3-node cluster:

```
---- Cluster Status ----
Ring ready: true

+--------------------+------+-------+-----+-------+
|        node        |status| avail |ring |pending|
+--------------------+------+-------+-----+-------+
| (C) dev1@127.0.0.1 |valid |  up   | 34.4|  --   |
|     dev2@127.0.0.1 |valid |  up   | 32.8|  --   |
|     dev3@127.0.0.1 |valid |  up   | 32.8|  --   |
+--------------------+------+-------+-----+-------+
```

In the above output, `Ring ready` denotes whether or not the cluster
agrees on [the ring][concept clusters], i.e. whether the cluster is
ready to begin taking requests.

The following information is then displayed for each node, by nodename
(in this case `dev1@127.0.0.1`, etc.):

* `status` --- There are five possible values for status:
  * `valid` --- The node has begun participating in cluster operations
  * `leaving` --- The node is is currently unloading ownership of its
    [data partitions][concept clusters] to other nodes
  * `exiting` --- The node's ownership transfers are complete and it is
    currently shutting down
  * `joining` --- The node is in the process of joining the cluster but
    but has not yet completed the join process
  * `down` --- The node is not currently responding
* `avail` --- There are two possible values: `up` if the node is
    available and taking requests and `down!` if the node is unavailable
* `ring` --- What percentage of the Riak [ring][concept clusters] the
  node is responsible for
* `pending` --- The number of pending transfers to or from the node

In addition, the cluster's [claimant node][cluster ops add remove node] node will have a `(C)` next
to it.

## join

Joins the current node to another node in the cluster.

```bash
riak-admin cluster join <node>
```

You _must_ specify a node to join to by nodename. You can join to any
node in the cluster. The following would join the current node to
`riak1@127.0.0.1`:

```bash
riak-admin cluster join riak1@127.0.0.1
```

Once a node joins, all of the operations necessary to establish
communication with all other nodes proceeds automatically.

> **Note**: As with all cluster-level actions, the changes made when you
run the `cluster join` command will take effect only after you have both
planned the changes by running [`riak-admin cluster plan`][use admin riak-admin#cluster-plan] and committed the changes by running
[`riak-admin cluster commit`][use admin riak-admin#cluster-commit].
You can stage multiple joins before planning/committing.

## leave

Instructs the current node to hand off its
[data partitions][concept clusters], leave the cluster, and shut down.

```bash
riak-admin cluster leave
```

You can also instruct another node (by nodename) to leave the cluster:

```bash
riak-admin cluster leave <node>
```

> **Note**: As with all cluster-level actions, the changes made when you
run the `cluster leave` command will take effect only after you have
both planned the changes by running [`riak-admin cluster plan`][use admin riak-admin#cluster-plan] and committed the changes
by running [`riak-admin cluster commit`][use admin riak-admin#cluster-commit].
You can stage multiple leave command before planning/committing.

## force-remove

Removes another node from the cluster (by nodename) _without_ first
handing off its [data partitions][concept clusters]. This command is
designed for crashed, unrecoverable nodes and should be used with
caution.

```bash
riak-admin cluster force-remove <node>
```

> **Note**: As with all cluster-level actions, the changes made when you
run the `cluster force-remove` command will take effect only after you have
both planned the changes by running [`riak-admin cluster plan`][use admin riak-admin#cluster-plan] and committed the changes
by running [`riak-admin cluster commit`][use admin riak-admin#cluster-commit]. You can stage multiple force-remove actions
before planning/committing.

## replace

Instructs a node to transfer all of its [data partitions][concept clusters] to another node and then to leave the
cluster and shut down.

```bash
riak-admin cluster replace <node1> <node2>
```

> **Note**: As with all cluster-level actions, the changes made when you
run the `cluster replace` command will take effect only after you have
both planned the changes by running [`riak-admin cluster plan`][use admin riak-admin#cluster-plan] and committed the changes
by running [`riak-admin cluster commit`][use admin riak-admin#cluster-commit]. You can stage multiple replace actions before
planning/committing.

## force-replace

Reassigns all [data partitions][concept clusters] owned by one node to
another node _without_ first handing off data.

```bash
riak-admin force-replace <node_being_replaced> <replacement_node>
```

Once the data partitions have been reassigned, the node that is being
replaced will be removed from the cluster.

> **Note**: As with all cluster-level actions, the changes made when you
run the `cluster force-replace` command will take effect only after you have
both planned the changes by running [`riak-admin cluster plan`][use admin riak-admin#cluster-plan] and committed the changes
by running [`riak-admin cluster commit`][use admin riak-admin#cluster-commit]. You can stage multiple force-replace actions
before planning/committing.

## plan

Displays the currently staged cluster changes.

```bash
riak-admin cluster plan
```

`riak-admin cluster plan` is complex, depending on the staged changes. 

* If a `leave` operation has been staged, `riak-admin cluster plan` will undo the staged change and no node will be stopped. 
* If a `join` operation has been staged, the joining node will be shut down after its ring has been cleared. When this node restarts, it will behave like a fresh unjoined node and can be joined again. 
* If a `cluster clear` operation is staged on a node that remains in the cluster, running `riak-admin cluster plan` will leave the node unaffected.

If there is no current cluster plan, the output will be `There are no
staged changes`. 

If there is a staged change (or changes), however, you
will see a detailed listing of what will take place upon commit, what
the cluster will look like afterward, etc. 

For example, if a `cluster leave` operation is staged in a 3-node cluster the output will look something like this:

```
=============================== Staged Changes ================================
Action         Details(s)
-------------------------------------------------------------------------------
leave          'dev2@127.0.0.1'
-------------------------------------------------------------------------------


NOTE: Applying these changes will result in 2 cluster transitions

###############################################################################
                         After cluster transition 1/2
###############################################################################

================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
leaving    32.8%      0.0%    'dev2@127.0.0.1'
valid      34.4%     50.0%    'dev1@127.0.0.1'
valid      32.8%     50.0%    'dev3@127.0.0.1'
-------------------------------------------------------------------------------
Valid:2 / Leaving:1 / Exiting:0 / Joining:0 / Down:0

WARNING: Not all replicas will be on distinct nodes

Transfers resulting from cluster changes: 38
  6 transfers from 'dev1@127.0.0.1' to 'dev3@127.0.0.1'
  11 transfers from 'dev3@127.0.0.1' to 'dev1@127.0.0.1'
  5 transfers from 'dev2@127.0.0.1' to 'dev1@127.0.0.1'
  16 transfers from 'dev2@127.0.0.1' to 'dev3@127.0.0.1'

###############################################################################
                        After cluster transition 2/2
###############################################################################

================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
valid      50.0%      --      'dev1@127.0.0.1'
valid      50.0%      --      'dev3@127.0.0.1'
-------------------------------------------------------------------------------
Valid:2 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

WARNING: Not all replicas will be on distinct nodes
```

Notice that there are distinct sections of the output for each of the
transitions that the cluster will undergo, including warnings, planned
data transfers, etc.

## commit

Commits the currently staged cluster changes. Staged cluster changes
must be reviewed using [`riak-admin cluster plan`][use admin riak-admin#cluster-plan] prior to being committed.

```bash
riak-admin cluster commit
```

## clear

Clears the currently staged cluster changes.

```bash
riak-admin cluster clear
```

## partitions

Prints primary, secondary, and stopped partition indices and IDs either
for the current node or for another, specified node. The following
prints that information for the current node:

```bash
riak-admin cluster partitions
```

This would print the partition information for a different node in the
cluster:

```bash
riak-admin cluster partitions --node=<node>
```

Partition information is contained in a table like this:

```
Partitions owned by 'dev1@127.0.0.1':
+---------+-------------------------------------------------+--+
|  type   |                      index                      |id|
+---------+-------------------------------------------------+--+
| primary |                        0                        |0 |
| primary | 91343852333181432387730302044767688728495783936 |4 |
| primary |182687704666362864775460604089535377456991567872 |8 |
|   ...   |                      ....                       |..|
| primary |1438665674247607560106752257205091097473808596992|63|
|secondary|                       --                        |--|
| stopped |                       --                        |--|
+---------+-------------------------------------------------+--+
```

## partition-count

Displays the current partition count either for the whole cluster or for
a particular node. This would display the partition count for the
cluster:

```bash
riak-admin cluster partition-count
```

This would display the count for a node:

```bash
riak-admin cluster partition-count --node=<node>
```

When retrieving the partition count for a node, you'll see a table like
this:

```
+--------------+----------+-----+
|     node     |partitions| pct |
+--------------+----------+-----+
|dev1@127.0.0.1|    22    | 34.4|
+--------------+----------+-----+
```

The `partitions` column displays the number of partitions claimed by the
node, while the `pct` column displays the percentage of the ring claimed.

## partition

The `cluster partition` command enables you to convert partition IDs to
indexes and vice versa using the `partition id` and `partition index`
commands, respectively. Let's say that you run the `riak-admin cluster
partitions` command and see that you have a variety of partitions, one
of which has an index of
`1004782375664995756265033322492444576013453623296`. You can convert
that index to an ID like this:

```bash
riak-admin cluster partition index=1004782375664995756265033322492444576013453623296
```

Conversely, if you have a partition with an ID of 20, you can retrieve
the corresponding index:

```bash
riak-admin cluster partition id=20
```
