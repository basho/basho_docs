---
title: Handoff
project: riak
version: 2.0.4+
document: reference
audience: intermediate
keywords: [operator, handoff, admin]
---

Riak is a distributed system built with two essential goals in mind:

* **fault tolerance**, whereby a Riak cluster can withstand node
    failure, network partitions, and other events in a way that does not
    disrupt normal functioning, and
* **scalability**, whereby operators can gracefully add and remove nodes
    to/from a Riak cluster

Both of these goals demand that Riak is able to either temporarily or
permanently re-assign responsibility for portions of the keyspace. That
re-assigning is referred to as **intra-cluster handoff** (or simply
**handoff** in our documentation).

## Types of Handoff

Intra-cluster handoff typically takes one of two forms: **hinted
handoff** and **ownership transfer**.

Hinted handoff occurs when a [[vnode|Vnodes]] temporarily takes over
responsibility for some data and then returns that data to its original
"owner." Imagine a three-node cluster with nodes A, B, and C. If node C
goes offline, e.g. during a network partition, nodes A and B will pick
up the slack, so to speak, assuming responsibility for node C's
operations. When node C comes back online, responsibility will be handed
back to the original vnodes.

Ownership transfer is different because it is meant to be permanent.
It occurs when a [[vnode|Vnodes]] no longer belongs to
the node on which it's running. This typically happens when the very
makeup of a cluster changes, e.g. when nodes are added or removed from
the cluster. In this case, responsibility for portions of the keyspace
needs to be fundamentally re-assigned.

Both types of handoff are handled automatically by Riak. Operators do
have the option, however, of enabling and disabling handoff on
particular nodes or all nodes and of configuring key aspects of Riak's
handoff behavior. More information can be found below.

## Configuring Handoff

Config | Description | Default
:------|:------------|:-------


## The Handoff Command-line Interface

### enable

```bash
riak-admin handoff enable
```

You must specify two things when enabling handoff:

* the node(s) to be targeted by the command
* whether you'd like to enable inbound handoff, outbound handoff, or
    both

You can select a target node using either the `--node` or the `-n` flag.
You can select a direction by specifying `inbound`, `outbound`, or
`both`. The following equivalent commands would enable outbound handoff
on the node `riak3@100.0.0.1`:

```bash
riak-admin handoff enable outbound --node riak3@100.0.0.1
riak-admin handoff enable outbound -n riak3@100.0.0.1
```

These two equivalent commands would enable inbound handoff on the node
`riak5@100.0.0.1`:

```bash
riak-admin handoff enable inbound --node riak5@100.0.0.1
riak-admin handoff enable inbound -n riak5@127.0.0.1
```

Alternatively, you can enable handoff on all nodes at the same time
using either the `-a` or `--all` flag. This command would enable both
inbound and outbound handoff on all nodes:

```bash
riak-admin handoff enable both --all
```

### disable

As for enabling handoff, the `riak-admin disable` command requires that
you specify both both a node or nodes to be targeted by the command and
whether you'd like to disbale inbound handoff, outbound handoff, or
both. The `disable` command works just like `enable`. This command
would disable all forms of handoff on all nodes, to give just one
example:

```bash
riak-admin handoff disable both --all
```

### summary

The `summary` command provides high-level information about active
handoffs in a cluster.

```bash
riak-admin handoff summary
```

This will return a table that will provide the following information
about each node in your cluster;

Header | Description
:------|:-----------
`Node` | Nodename of the node
`Total` | Total number of active transfers throughout the entire cluster
`Ownership` | Total number of ownership exchanges
`Resize` | Total handoffs related to [[ring resizing]] operations
`Hinted` | [[Hinted handoff|Riak Glossary#Hinted-Handoff]] total
`Repair` |

### details

This command provides information only about active transfers.

```bash
riak-admin handoff details
```

If no transfers are currently underway, this command will output `No
ongoing transfers`. Otherwise, you will something like this:

```

```

### config

This command displays the values for all handoff-specific [[configurable
parameters|Configuration Files#Intra-Cluster-Handoff]] on each node in
the cluster. The table below lists and describes those parameters:

Config | Description | Default
:------|:------------|:-------
`transfer_limit` | The number of concurrent node-to-node transfers that are allowed | `2`
`handoff.outbound` | Whether outbound handoff is enabled on the node. Possible values are `on` or `off`. | `on`
`handoff.inbound` | Whether inbound handoff is enabled on the node | `on`
`handoff.port` | The port used by the node for handoff-related traffic |

