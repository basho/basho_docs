---
title: riak-admin Command Line
project: riak
version: 1.2.0+
document: reference
toc: true
audience: beginner
keywords: [command-line, riak-admin]
moved: {
    '1.4.0-': '/references/Command-Line-Tools---riak-admin'
}
---

# riak-admin

This script performs operations unrelated to node-liveness, including node
membership, backup, and basic status reporting.  The node must be running for
most of these commands to work.


```
Usage: riak-admin { cluster | join | leave | backup | restore | test |
                    reip | js-reload | erl-reload | wait-for-service |
                    ringready | transfers | force-remove | down |
                    cluster-info | member-status | ring-status | vnode-status |
                    diag | status | transfer-limit | top }
```

## cluster

As of version 1.2, Riak provides a multi-phased approach to cluster administration that allows changes to be staged and reviewed before being committed.

This approach to cluster administration allows multiple changes to be grouped together, such as adding multiple nodes at once, or adding some nodes while removing others.

Details about how a set of staged changes will impact the cluster, listing the future ring ownership as well as the number of transfers necessary to implement the planned changes are provided by the new interface.

The following commands stage changes to cluster membership. These commands
do not take effect immediately. After staging a set of changes, the staged
plan must be committed using the staging commands to take effect:

## cluster join
Join this node to the cluster containing &lt;node&gt;.

```bash
riak-admin cluster join <node>
```

## cluster leave
Instruct this node to hand off its data partitions, leave the cluster and shutdown.

```bash
riak-admin cluster leave
```

Instruct &lt;node&gt; to hand off its data partitions, leave the cluster and shutdown.

```bash
riak-admin cluster leave <node>
```
## cluster force-remove
Remove &lt;node&gt; from the cluster without first handing off data partitions. This command is designed for crashed, unrecoverable nodes, and should be used with caution.

```bash
riak-admin cluster force-remove <node>
```
## cluster replace
Instruct &lt;node1&gt; to transfer all data partitions to &lt;node2&gt;, then leave the cluster and shutdown.

```bash
riak-admin cluster replace <node1> <node2>
```

## cluster force-replace
Reassign all partitions owned by &lt;node1&gt; to &lt;node2&gt; without first handing off data, and then remove &lt;node1&gt; from the cluster.

```bash
riak-admin cluster force-replace <node1> <node2>
```


### Staging Commands

The following commands are used to work with staged changes:

#### cluster plan
Display the currently staged cluster changes.

```bash
riak-admin cluster plan
```

#### cluster clear
Clear the currently staged cluster changes.

```bash
riak-admin cluster clear
```

#### cluster commit
Commit the currently staged cluster changes. Staged cluster changes must be reviewed with `riak-admin cluster plan` prior to being committed.

```bash
riak-admin cluster commit
```


## join

<div class="note"><div class="title">Deprecation Notice</title></div>
  <p>As of Riak version 1.2, the <tt>riak-admin join</tt> command has been deprecated in favor of the [[riak-admin cluster join|riak-admin Command Line#cluster-join]] command. The command can still be used by providing a <tt>-f</tt> option, however.</p>
</div>

Joins the running node to another running node so that they participate in the
same cluster.

* &lt;node&gt; is the other node to connect to.


```bash
riak-admin join -f <node>
```


## leave

<div class="note"><div class="title">Deprecation Notice</title></div>
  <p>As of Riak version 1.2, the <tt>riak-admin leave</tt> command has been deprecated in favor of the new [[riak-admin cluster leave|riak-admin Command Line#cluster-leave]] command. The command can still be used by providing a <tt>-f</tt> option, however.</p>
</div>


Causes the node to leave the cluster in which it participates. After this is run, the node in question will hand-off all its replicas to other nodes in the cluster before it completely exits.


```bash
riak-admin leave -f
```


## backup

Backs up the data from the node or entire cluster into a file.

* &lt;node&gt; is the node from which to perform the backup.
* &lt;cookie&gt; is the Erlang cookie/shared secret used to connect to the node.
This is "riak" in the [[default configuration|Configuration Files#\-setcookie]].
* &lt;filename&gt; is the file where the backup will be stored. _This should be
the full path to the file._
* [node|all] specifies whether the data on this node or the entire cluster will
be backed up, respectively.


```bash
riak-admin backup <node> <cookie> <filename> [node|all]
```


## restore

Restores data to the node or cluster from a previous backup.

* &lt;node&gt; is the node which will perform the restore.
* &lt;cookie&gt; is the Erlang cookie/shared secret used to connect to the node.
By default this is set to "riak" in the [[vm.args|Configuration Files#vm.args]] configuration file.
* &lt;filename&gt; is the file where the backup is stored. _This should be the
full path to the file._

```bash
riak-admin restore <node> <cookie> <filename>
```

## test

Runs a test of a few standard Riak operations against the running node.

```
riak-admin test
```

## reip

_This will likely be removed in future versions. Instead use `riak-admin cluster replace`._

Renames a node. The current ring state will be backed up in the process. **The
node must NOT be running for this to work.**

```bash
riak-admin reip <old nodename> <new nodename>
```

## js-reload

Forces the embedded Javascript virtual machines to be restarted. This is useful
when deploying new custom built-in [[MapReduce|Using MapReduce]] functions. (_This needs to be
run on all nodes in the cluster_.)

```bash
riak-admin js-reload
```

## services

Lists available services on the node (e.g. **riak_kv**)

```bash
riak-admin services
```

## wait-for-service

Waits on a specific watchable service to be available (typically _riak_kv_).
This is useful when (re-)starting a node while the cluster is under load. Use
"services" to see what services are available on a running node.

```
riak-admin wait-for-service <service> <nodename>
```

## ringready

Checks whether all nodes in the cluster agree on the ring state. Prints "FALSE"
if the nodes do not agree. This is useful after changing cluster membership to
make sure that ring state has settled.

```bash
riak-admin ringready
```

## transfers

Identifies nodes that are awaiting transfer of one or more partitions. This
usually occurs when partition ownership has changed (after adding or removing a
node) or after node recovery.

```bash
riak-admin transfers
```

## transfer-limit

Change the handoff_concurrency limit.

```bash
riak-admin transfer-limit <node> <limit>
```

## force-remove

<div class="note">
<div class="title">Deprecation Notice</title></div>
As of Riak version 1.2, the <tt>riak-admin force-remove</tt> command has been deprecated in favor of the new [[riak-admin cluster force-remove|riak-admin Command Line#cluster-force-remove]] command. The command can still be used by providing a <code>-f</code> option, however.
</div>

Immediately removes a node from the cluster without ensuring handoff of its replicas. This is a dangerous command, and is designed to only be used in cases were the normal, safe leave behavior cannot be used -- e.g. when the node you are removing had a major hardware failure and is unrecoverable. Using this command will result in a loss of all replicas living on the removed node which will then need to be recovered through other means such as [[read repair|Replication#Read-Repair]]. It's recommended that you use the [[riak-admin leave|riak-admin Command Line#leave]] command whenever possible.

```bash
riak-admin force-remove -f <node>
```

## down

Marks a node as down so that ring transitions can be performed before the node is brought back online.


```bash
riak-admin down <node>
```

## cluster-info

Output system information from a Riak cluster. This command will collect
information from all nodes or a subset of nodes and output the data to a single
text file.

The following information is collected:

 * Current time and date
 * VM statistics
 * erlang:memory() summary
 * Top 50 process memory hogs
 * Registered process names
 * Registered process name via regs()
 * Non-zero mailbox sizes
 * Ports
 * Applications
 * Timer status
 * ETS summary
 * Nodes summary
 * net_kernel summary
 * inet_db summary
 * Alarm summary
 * Global summary
 * erlang:system_info() summary
 * Loaded modules
 * Riak Core config files
 * Riak Core vnode modules
 * Riak Core ring
 * Riak Core latest ring file
 * Riak Core active partitions
 * Riak KV status
 * Riak KV ringready
 * Riak KV transfers

```bash
riak-admin cluster_info <output file> [<node list>]
```

Examples:

```bash
# Output information from all nodes to /tmp/cluster_info.txt
riak-admin cluster_info /tmp/cluster_info.txt
```

```
# Output information from the current node
riak-admin cluster_info /tmp/cluster_info.txt local
```

```bash
# Output information from a subset of nodes
riak-admin cluster_info /tmp/cluster_info.txt riak@192.168.1.10
riak@192.168.1.11
```

## member-status

Prints the current status of all cluster members.

```bash
riak-admin member-status
```

## ring-status

Outputs the current claimant, its status, ringready, pending ownership handoffs
and a list of unreachable nodes.

```bash
riak-admin ring-status
```

## vnode-status

Outputs the status of all vnodes the are running on the local node.

```bash
riak-admin vnode-status
```

{{#1.3.0+}}
## aae-status

This command provides insight into operation of Riak's Active Anti Entropy
(AAE) feature.

```
riak-admin aae-status
```

The output contains information on AAE key/value partition exchanges,
entropy tree building, and key repairs which were triggered by AAE.

* **Exchanges**
 * The *Last* column lists when the most recent exchange between a partition and one of its sibling replicas was performed.
 * The *All* column shows how long it has been since a partition exchanged with all of its sibling replicas.

* **Entropy Trees**
 * The *Built* column shows when the hash trees for a given partition were created.

* **Keys Repaired**
 * The *Last* column shows the number of keys repaired during the most
   recent key exchange.
 * The *Mean* column shows the mean number of keys repaired during all
   key exchanges since the last node restart.
 * The *Max* column shows the maximum number of keys repaired during all
   key exchanges since the last node restart.

<div class="info">All AAE status information is in-memory and is reset across a node restart. Only tree build times are persistent (since trees themselves are persistent).</div>

More details on the `aae-status` command are available in the
[Riak version 1.3 release notes](https://github.com/basho/riak/blob/1.3/RELEASE-NOTES.md#active-anti-entropy).
{{/1.3.0+}}

## diag

Run diagnostic checks against &lt;node&gt;. {{#<1.3.0}}[riaknostic](http://riaknostic.basho.com/) must be installed in order to run.{{/<1.3.0}}

```bash
riak-admin diag <check>
```

## status

Prints status information, including performance statistics, system health
information, and version numbers. The statistics-aggregator must be enabled in
the [[configuration|Configuration Files#riak_kv_stat]] for this to work. Further
information about the output is available [[here|Inspecting a Node]].


```bash
riak-admin status
```

{{#1.3.1+}}
## reformat-indexes

This command reformats integer indexes in Secondary Index data for versions
of Riak prior to 1.3.1 so that range queries over the indexes will return
correct results.

```
riak-admin reformat-indexes [<concurrency>] [<batch size>] --downgrade
```

The `concurrency` option defaults to *2* and controls how many partitions are
concurrently reformatted.

The `batch size` option controls the number of simultaneous key operations
and defaults to *100*.

The command can be executed while the node is serving requests, and default
values are recommended for most cases. You should only change the default
values after testing impact on cluster performance.

Information is written to `console.log` upon completion of the process.

A `--downgrade` switch can be specified when downgrading a node to a version
of Riak prior to version 1.3.1.

Additional details are available in the
[Riak 1.3.1 release notes](https://github.com/basho/riak/blob/1.3/RELEASE-NOTES.md).
{{/1.3.1+}}

## top

Top provides information about what the Erlang processes inside of Riak are doing. Top reports process reductions (an indicator of CPU utilization), memory used and message queue sizes

```bash
riak-admin top
```
