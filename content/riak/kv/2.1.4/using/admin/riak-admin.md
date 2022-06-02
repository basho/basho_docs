---
title: "riak-admin Command Line Interface"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "riak-admin CLI"
    identifier: "cluster_admin_cli"
    weight: 101
    parent: "managing_cluster_admin"
toc: true
aliases:
  - /riak/2.1.4/ops/running/tools/riak-admin
  - /riak/kv/2.1.4/ops/running/tools/riak-admin
---

[config reference]: {{<baseurl>}}riak/kv/2.1.4/configuring/reference
[use admin commands]: {{<baseurl>}}riak/kv/2.1.4/using/admin/commands
[use admin commands#join]: {{<baseurl>}}riak/kv/2.1.4/using/admin/commands/#join
[use admin commands#leave]: {{<baseurl>}}riak/kv/2.1.4/using/admin/commands/#leave
[cluster ops backup]: {{<baseurl>}}riak/kv/2.1.4/using/cluster-operations/backing-up
[config reference#node-metadata]: {{<baseurl>}}riak/kv/2.1.4/configuring/reference/#node-metadata
[cluster ops change info]: {{<baseurl>}}riak/kv/2.1.4/using/cluster-operations/changing-cluster-info
[usage mapreduce]: {{<baseurl>}}riak/kv/2.1.4/developing/usage/mapreduce
[usage commit hooks]: {{<baseurl>}}riak/kv/2.1.4/developing/usage/commit-hooks
[config reference#ring]: {{<baseurl>}}riak/kv/2.1.4/configuring/reference/#ring
[cluster ops inspect node]: {{<baseurl>}}riak/kv/2.1.4/using/cluster-operations/inspecting-node
[use ref monitoring]: {{<baseurl>}}riak/kv/2.1.4/using/reference/statistics-monitoring
[downgrade]: {{<baseurl>}}riak/kv/2.1.4/setup/downgrade
[security index]: {{<baseurl>}}riak/kv/2.1.4/using/security/
[security managing]: {{<baseurl>}}riak/kv/2.1.4/using/security/managing-sources
[cluster ops bucket types]: {{<baseurl>}}riak/kv/2.1.4/using/cluster-operations/bucket-types
[cluster ops 2i]: {{<baseurl>}}riak/kv/2.1.4/using/reference/secondary-indexes
[repair recover index]: {{<baseurl>}}riak/kv/2.1.4/using/repair-recovery
[cluster ops strong consistency]: {{<baseurl>}}riak/kv/2.1.4/using/cluster-operations/strong-consistency
[cluster ops handoff]: {{<baseurl>}}riak/kv/2.1.4/using/cluster-operations/handoff
[use admin riak-admin#stats]: {{<baseurl>}}riak/kv/2.1.4/using/admin/riak-admin/#stats

## riak-admin

This script performs operations unrelated to node liveness, including
node membership, backup, and basic status reporting. The node must be
running for most of these commands to work. Running the `riak-admin`
command by itself will output a list of available commands:

```
Usage: riak-admin { cluster | join | leave | backup | restore | test |
                    reip | js-reload | erl-reload | wait-for-service |
                    ringready | transfers | force-remove | down |
                    cluster-info | member-status | ring-status | vnode-status |
                    aae-status | diag | stat | status | transfer-limit | reformat-indexes |
                    top [-interval N] [-sort reductions|memory|msg_q] [-lines N] |
                    downgrade-objects | security | bucket-type | repair-2i |
                    search | services | ensemble-status | handoff | set |
                    show | describe }
```

## Node Naming

An important thing to bear in mind is that all Riak nodes have unique
names within the cluster that are used for a wide variety of operations.
The name for each node can be set and changed in each node's
[configuration files][config reference]. The examples below set the name of a node to
`riak_node_1@199.99.99.01` in the `riak.conf` file if you are using the
newer configuration system and in `vm.args` if you are using the older
system:

```riakconf
nodename = riak_node_1@199.99.99.01
```

```vmargs
-name riak_node_1@199.99.99.01
```

The name prior to the `@` symbol can be whatever you'd like, e.g.
`riak1`, `dev`, `cluster1_node1`, or `spaghetti`. After the `@` you must
use a resolvable IP address or hostname. In general, we recommend using
hostnames over IP addresses when possible because this enables the node
to potentially live on different machines over the course of its
existence.

## cluster

Documentation for the `riak-admin cluster` command interface can be
found in [Cluster Administration][use admin commands].

## join

> **Deprecation Notice**
>
>As of Riak version 1.2, the `riak-admin join` command has
been deprecated in favor of the [`riak-admin cluster join`][use admin commands#join] command. However, this command can still be
used by providing a `-f` option (which forces the command).

Joins the running node to another running node so that they participate
in the same cluster. `<node>` is the other node to connect to.

```bash
riak-admin join -f <node>
```

## leave

> **Deprecation Notice**
>
> As of Riak version 1.2, the `riak-admin leave` command has
been deprecated in favor of the new [`riak-admin cluster leave`][use admin commands#leave] command. However, this
command can still be used by providing a `-f` option (which
forces the command).

Causes the node to leave the cluster in which it participates. After
this is run, the node in question will hand-off all its replicas to
other nodes in the cluster before it completely exits.

```bash
riak-admin leave -f
```

## backup

> **Deprecation notice**
The `riak-admin backup` command has been deprecated. We recommend using
backend-specific backup procedures instead. Documentation can be found
in [Backing up Riak KV][cluster ops backup].

Backs up the data from the node or entire cluster into a file.

```bash
riak-admin backup <node> <cookie> <filename> [node|all]
```

* `<node>` is the node from which to perform the backup.
* `<cookie>` is the Erlang cookie/shared secret used to connect to the
  node. This is `riak` in the [default configuration][config reference#node-metadata].
* `<filename>` is the file where the backup will be stored. _This should
  be the full path to the file_.
* `[node|all]` specifies whether the data on this node or the entire

## restore

> **Deprecation notice**
>
> The `riak-admin restore` command has been deprecated. It was originally
intended to be used in conjunction with backups performed using the
`riak-admin backup` command, which is also deprecated. We recommend
using the backup and restore methods described in [Backing up Riak KV][cluster ops backup].

Restores data to the node or cluster from a previous backup.

```bash
riak-admin restore <node> <cookie> <filename>
```

* `<node>` is the node which will perform the restore.
* `<cookie>` is the Erlang cookie/shared secret used to connect to the
  node. This is `riak` in the [default configuration][config reference#node-metadata].
* `<filename>` is the file where the backup is stored. _This should be
  the full path to the file_.

## test

Runs a test of a few standard Riak operations against the running node.

```bash
riak-admin test
```

If the test is successful, you should see output like the following:

```
Successfully completed 1 read/write cycle to 'dev1@127.0.0.1'
```

## reip

Renames a node. This process backs up and edits the Riak ring, and
**must** be run while the node is stopped. Reip should only be run in
cases where `riak-admin cluster force-replace` cannot be used to
rename the nodes of a cluster. For more information, visit the
[Changing Cluster Information][cluster ops change info] document.

```bash
riak-admin reip <old nodename> <new nodename>
```

{{% note title="Note about reip prior to Riak 2.0" %}}
Several bugs have been fixed related to reip in Riak 2.0. We recommend against
using reip prior to 2.0, if possible.
{{% /note %}}


## js-reload

Forces the embedded Javascript virtual machines to be restarted. This is
useful when deploying custom built-in [MapReduce][usage mapreduce]
functions.

**Note**: This needs to be run on _all nodes_ in the cluster.

```bash
riak-admin js-reload
```

## erl-reload

Reloads the Erlang `.beam` files used for [MapReduce][usage mapreduce]
jobs, [pre- and post-commit hooks][usage commit hooks], and other
purposes.

> **Note**: This needs to be run on _all nodes_ in the cluster.

```bash
riak-admin erl-reload
```

## wait-for-service

Waits on a specific watchable service to be available (typically
`riak_kv`). This is useful when (re-)starting a node while the cluster
is under load. Use `riak-admin services` to see which services are
available on a running node.

```bash
riak-admin wait-for-service <service> <nodename>
```

## ringready

Checks whether all nodes in the cluster agree on the ring state.
Prints `FALSE` if the nodes do not agree. This is useful after changing
cluster membership to make sure that the ring state has settled.

```bash
riak-admin ringready
```

## transfers

Identifies nodes that are awaiting transfer of one or more partitions.
This usually occurs when partition ownership has changed (after adding
or removing a node) or after node recovery.

```bash
riak-admin transfers
```

## transfer-limit

Change the `handoff_concurrency` limit. The value set by running this
command will only persist while the node is running. If the node is
restarted, the `transfer-limit` will return to the default of `2` or the
value specified in the [`transfer_limit`][config reference#ring] setting in the `riak.conf` configuration file.

Running this command with no arguments will display the current
transfer-limit for each node in the cluster.

```bash
riak-admin transfer-limit <node> <limit>
```

## down

Marks a node as down so that ring transitions can be performed before
the node is brought back online.

```bash
riak-admin down <node>
```

## cluster-info

Output system information from a Riak cluster. This command will collect
information from all nodes or a subset of nodes and output the data to a
single text file.

```bash
riak-admin cluster-info <output file> [<node list>]
```

The following information is collected:

 * Current time and date
 * VM statistics
 * `erlang:memory()` summary
 * Top 50 process memory hogs
 * Registered process names
 * Registered process name via `regs()`
 * Non-zero mailbox sizes
 * Ports
 * Applications
 * Timer status
 * ETS summary
 * Nodes summary
 * `net_kernel` summary
 * `inet_db` summary
 * Alarm summary
 * Global summary
 * `erlang:system_info()` summary
 * Loaded modules
 * Riak Core config files
 * Riak Core vnode modules
 * Riak Core ring
 * Riak Core latest ring file
 * Riak Core active partitions
 * Riak KV status
 * Riak KV ringready
 * Riak KV transfers

#### Examples

Output information from all nodes to `/tmp/cluster_info.txt`:

```bash
riak-admin cluster_info /tmp/cluster_info.txt
```

Output information from the current nodeL

```bash
riak-admin cluster_info /tmp/cluster_info.txt local
```

Output information from a subset of nodes:

```bash
riak-admin cluster_info /tmp/cluster_info.txt riak@192.168.1.10
riak@192.168.1.11
```

## member-status

Prints the current status of all cluster members.

```bash
riak-admin member-status
```

## ring-status

Outputs the current claimant, its status, ringready, pending ownership
handoffs, and a list of unreachable nodes.

```bash
riak-admin ring-status
```

## vnode-status

Outputs the status of all vnodes the are running on the local node.

```bash
riak-admin vnode-status
```

{{% note title="Frequent use of `riak-admin vnode-status`" %}}
The `riak-admin vnode-status` command should not be used more frequently than every 5 minutes. Running it more often will result in handoffs being stalled.
{{% /note %}}

## aae-status

This command provides insight into operation of Riak's Active
Anti-Entropy (AAE) feature.

```bash
riak-admin aae-status
```

The output contains information on AAE key/value partition exchanges,
entropy tree building, and key repairs which were triggered by AAE.

* **Exchanges**
 * The *Last* column lists when the most recent exchange between a
   partition and one of its sibling replicas was performed.
 * The *All* column shows how long it has been since a partition
   exchanged with all of its sibling replicas.

* **Entropy Trees**
 * The *Built* column shows when the hash trees for a given partition
   were created.

* **Keys Repaired**
 * The *Last* column shows the number of keys repaired during the most
   recent key exchange.
 * The *Mean* column shows the mean number of keys repaired during all
   key exchanges since the last node restart.
 * The *Max* column shows the maximum number of keys repaired during all
   key exchanges since the last node restart.

{{% note title="Note in AAE status information" %}}
All AAE status information is in-memory and is reset across a node restart.
Only tree build times are persistent (since trees themselves are persistent)
{{% /note %}}

More details on the `aae-status` command are available in the [Riak
version 1.3 release notes](https://github.com/basho/riak/blob/1.3/RELEASE-NOTES.md#active-anti-entropy).

## diag

The `diag` command invokes the Riaknostic
diagnostic system.

```bash
riak-admin diag
```

This command allows you to specify which diagnostic checks you would
like to run, which types of diagnostic messages you wish to see, and so
on. More comprehensive information can be found in the documentation on
[inspecting a node][cluster ops inspect node].

## stat

Provides an interface for interacting with a variety of cluster-level
metrics and information.

```bash
riak-admin stat
```

Full documentation of this command can be found in [Statistics and Monitoring][use ref monitoring].

## status

Prints status information, including performance statistics, system
health information, and version numbers. Further information about the
output is available in the documentation on [inspecting a node][cluster ops inspect node].

```bash
riak-admin status
```

## reformat-indexes

This command reformats integer indexes in Secondary Index data for
versions of Riak prior to 1.3.1 so that range queries over the indexes
will return correct results.

```
riak-admin reformat-indexes [<concurrency>] [<batch size>] --downgrade
```

The `concurrency` option defaults to `2` and controls how many
partitions are concurrently reformatted.

The `batch size` option controls the number of simultaneous key
operations and defaults to `100`.

This command can be executed while the node is serving requests, and
default values are recommended for most cases. You should only change
the default values after testing impact on cluster performance.

Information is written to `console.log` upon completion of the process.

A `--downgrade` switch can be specified when downgrading a node to a version
of Riak prior to version 1.3.1.

Additional details are available in the [Riak 1.3.1 release
notes](https://github.com/basho/riak/blob/1.3/RELEASE-NOTES.md).

## top

Top uses Erlang's etop to provide information about what the Erlang
processes inside of Riak are doing. Top reports process reductions (an
indicator of CPU utilization), memory used, and message queue sizes.

```bash
riak-admin top [-interval N] [-sort reductions|memory|msg_q] [-lines N]
```

Options:

* `interval` specifies the number of seconds between each update of the
  top output and defaults to `5`
* `sort` determines on which category `riak-admin top` sorts and
  defaults to `reductions`
* `lines` specifies the number of processes to display in the top output
  and defaults to `10`

More information about Erlang's etop can be found in the [etop
documentation](http://www.erlang.org/doc/man/etop.html).

## downgrade-objects

This command is used when changing the format of Riak objects, usually
as part of a version downgrade.

```bash
riak-admin downgrade-objects <kill-handoffs> [<concurrency>]
```

More detailed information can be found in [Rolling Downgrades][downgrade].

## security

This command enables you to manage to manage Riak users, choose sources
of authentication, assign and revoke permissions to/from users and
groups, enable and disable Riak Security, and more.

```bash
riak-admin security <command>
```

More comprehensive information on user management and can be found in
the [Authentication and Authorization][security index] guide. Detailed information on authentication sources can be found in [Managing Security Sources][security managing].

## bucket-type

Bucket types are a means of managing bucket properties introduced in
Riak 2.0, as well as an additional namespace in Riak in addition to
buckets and keys. This command enables you to create and modify bucket
types, provide the status of currently available bucket types, and
activate created bucket types.

```bash
riak-admin bucket-type <command>
```

More on bucket types can be found in [Using Bucket Types][cluster ops bucket types].

## repair-2i

This command repairs [secondary indexes][cluster ops 2i] in a
specific partition or on a cluster-wide basis. Implementation details
can be found in [Repairing Indexes][repair recover index].

To repair secondary indexes throughout the entire cluster, run the
`repair-2i`command by itself, without a subcommand:

```bash
riak-admin repair-2i
```

This will initiate the repair process. When you run this command, you
should see something like the following (where `<ring_size>` is the
number of partitions in your Riak cluster):

```
Will repair 2i data on <ring_size> partitions
Watch the logs for 2i repair progress reports
```

To repair secondary indexes in a specific partition, provide the ID of
the partition along with the `repair-2i` command:

```bash
riak-admin repair-2i 593735040165679310520246963290989976735222595584
```

You can check on the status of the repair process at any time:

```bash
riak-admin repair-2i status
```

If the repair is already finished, the console will return `2i repair is
not running`. If the repair is still in progress, the console will
return a series of statistics like this:

```
2i repair status is running:
        Total partitions: 64
        Finished partitions: 44
        Speed: 100
        Total 2i items scanned: 0
        Total tree objects: 0
        Total objects fixed: 0
```

If you're concerned about the computational resources required to repair
secondary indexes, you can set the speed of the process to an integer
between 1 and 100 (with 100 being the fastest). This command would set
the speed to 90:

```bash
riak-admin repair-2i --speed 90
```

The repair process can be stopped at any moment using the `kill`
command:

```bash
riak-admin repair-2i kill
```

## search

The search command provides sub-commands for various administrative
work related to the new Riak Search.

```bash
riak-admin search <command>
```

### aae-status

```bash
riak-admin search aae-status
```

Output active anti-entropy (AAE) statistics for search. There are
three sections. Each section contains statistics for a specific aspect
of AAE for every partition owned by the local node.

The first section provides information on exchanges. Exchange is the
process of comparing hash trees to determine divergences between KV
data and search indexes. The `Index` column contains the partition
number. The `Last (ago)` column is the amount of time that has passed
since the last exchange. The `All (ago)` column is the amount of time
that has passed since all preflists for that partition have been
exchanged.

The second section lists how much time has passed since the hashtree
for that partition has been built from scratch. By default trees
expire after 1 week and are rebuilt from scratch.

The third section presents statistics on repair operations that have
occurred. Repair is performed when AAE notices that the KV and search
hashtree don't match for a particular key. The `Last` column is the
number of keys repaired during the last exchange. The `Mean` column is
the average number of keys repaired for all exchange rounds since the
node has started. The `Max` column is the maximum number of keys
repaired for a given exchange round since the node has started.

### switch-to-new-search

{{% note title="Only For Legacy Migration" %}}
This is only needed when migrating from legacy riak search to the new Search
(Yokozuna).
{{% /note %}}

```bash
riak-admin search switch-to-new-search
```

Switch handling of the HTTP `/solr/<index>/select` resource and
protocol buffer query messages from legacy Riak Search to new Search
(Yokozuna).

## services

Lists available services on the node (e.g. `riak_kv`).

```bash
riak-admin services
```

## ensemble-status

This command is used to provide insight into the current status of the
consensus subsystem undergirding Riak's [strong consistency][cluster ops strong consistency] feature.

```bash
riak-admin ensemble-status
```

This command can also be used to check on the status of a specific
consensus group in your cluster:

```bash
riak-admin ensemble-status <group id>
```

Complete documentation of this command can be found in [Managing Strong Consistency][cluster ops strong consistency].

## handoff

Documentation for the `handoff` command can be found in [Handoff][cluster ops handoff].

## set

Enables you to change the value of one of Riak's configuration
parameters on the fly, without needing to stop and restart the node.

```bash
riak-admin set <variable>=<value>
```

At the moment, the `set` command can only be used for the following
parameters, all three of which are related to Riak's [handoff][cluster ops handoff] subsystem:

* `transfer_limit`
* `handoff.outbound`
* `handoff.inbound`

## show

Whereas the [`riak-admin status`][use admin riak-admin#stats] command will display all currently available statistics for your Riak
cluster, the `show` command enables you to view only some of those
statistics.

```bash
riak-admin show <variable>
```

## describe

Provides a brief description of one of Riak's [configurable parameters][config reference].

```bash
riak-admin describe <variable>
```

If you want to know the meaning of the `nodename` parameter:

```bash
riak-admin describe nodename
```

That will produce the following output:

```
nodename:
  Name of the Erlang node
```
