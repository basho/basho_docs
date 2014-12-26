---
title: Cluster Administration
project: riak
version: 2.0.4+
---

This document explains usage of the `[[riak-admin cluster|riak-admin
Command Line#cluster]]` interface, which enables you to perform a wide
variety of cluster-level actions.

<div class="note">
<div class="title">Note on command names</div>
Many of the commands available through the `riak-admin cluster`
interface are also available as self-standing commands. The `riak-admin
member-status` command is now the `riak-admin cluster status` command,
`riak-admin join` is now `riak-admin cluster join`, etc.

We recommend using the `riak-admin cluster` interface over the older,
deprecated commands. You will receive a deprecation warning if you use
the older commands.
</div>

## status

Displays a variety of information about the cluster.

```bash
riak-admin cluster status
```

This will return output like the following in a three-node cluster:

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
agrees on [[the ring|Clusters#The-Ring]], i.e. whether the cluster is
ready to begin taking requests.

The following information is then displayed for each node, by nodename
(in this case `dev1@127.0.0.1`, etc.):

* `status` --- There are five possible values for status:
  * `valid` ---
  * `leaving` ---
  * `exiting` ---
  * `joining` ---
  * `down` ---
* `avail` --- There are two possible values: `up` if the node is
    available and taking requests and `down!` if the node is unavailable
* `ring` --- What percentage of the Riak [[ring|Clusters#The-Ring]] the
  node is responsible for
* `pending` --- The number of pending transfers to or from the node

In addition, the cluster's [[claimant node|Adding and Removing
Nodes#How-Cluster-Membership-Changes-Work]] node will have a `(C)` next
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

**Note**: As with all cluster-level actions, the changes made when you
run the `cluster join` command will take effect only after you have both
planned the changes by running `[[riak-admin cluster plan|riak-admin
Command Line#cluster-plan]]` and committed the changes by running
`[[riak-admin cluster commit|riak-admin Command Line#cluster-commit]]`.
You can stage multiple joins before planning/committing.

## leave

Instructs the current node to hand off its [[data
partitions|Clusters#The-Ring]], leave the cluster, and shut down.

```bash
riak-admin cluster leave
```

You can also instruct another node (by nodemane) to leave the cluster:

```bash
riak-admin cluster leave <node>
```

**Note**: As with all cluster-level actions, the changes made when you
run the `cluster leave` command will take effect only after you have
both planned the changes by running `[[riak-admin cluster
plan|riak-admin Command Line#cluster-plan]]` and committed the changes
by running `[[riak-admin cluster commit|riak-admin Command
Line#cluster-commit]]`. You can stage multiple leave command before
planning/committing.

## force-remove

Removes another node from the cluster (by nodename) _without_ first
handing off its [[data partitions|Clusters#The-Ring]]. This command is
designed for crashed, unrecoverable nodes and should be used with
caution.

```bash
riak-admin cluster force-remove <node>
```

**Note**: As with all cluster-level actions, the changes made when you
run the `cluster force-remove` command will take effect only after you
have both planned the changes by running `[[riak-admin cluster
plan|riak-admin Command Line#cluster-plan]]` and committed the changes
by running `[[riak-admin cluster commit|riak-admin Command
Line#cluster-commit]]`. You can stage multiple force-remove actions
before planning/committing.

## replace

Instructs a node to transfer all of its [[data
partitions|Clusters#The-Ring]] to another node and then to leave the
cluster and shut down.

```bash
riak-admin cluster replace <node1> <node2>
```

**Note**: As with all cluster-level actions, the changes made when you
run the `cluster replace` command will take effect only after you have
both planned the changes by running `[[riak-admin cluster
plan|riak-admin Command Line#cluster-plan]]` and committed the changes
by running `[[riak-admin cluster commit|riak-admin Command
Line#cluster-commit]]`. You can stage multiple replace actions before
planning/committing.

## plan

Displays the currently staged cluster changes.

```bash
riak-admin cluster plan
```

If there is no current cluster plan, the output will be `There are no
staged changes`. If there is a staged change (or changes), however, you
will see a detailed listing of what will take place upon commit, what
the cluster will look like afterward, etc. If a `cluster leave`
operation is staged in a three-node cluster, for example, the output
will look something like this:

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

Notice that there are distinc sections of the output for each of the
transitions that the cluster will undergo, including warnings, planned
data transfers, etc.

## partition



## partitions


