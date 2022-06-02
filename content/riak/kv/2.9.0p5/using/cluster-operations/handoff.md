---
title: "Enabling and Disabling Handoff"
description: ""
project: "riak_kv"
project_version: "2.9.0p5"
menu:
  riak_kv-2.9.0p5:
    name: "Handoff"
    identifier: "cluster_operations_handoff"
    weight: 107
    parent: "managing_cluster_operations"
toc: true
aliases:
  - /riak/2.9.0p5/ops/running/handoff
  - /riak/kv/2.9.0p5/ops/running/handoff
  - /riak/2.9.0p5/using/cluster-operations/handoff/
  - /riak/2.9.0/using/cluster-operations/handoff/
  - /riak/kv/2.9.0/using/cluster-operations/handoff/
  - /riak/kv/2.9.0p1/using/cluster-operations/handoff/
  - /riak/kv/2.9.0p2/using/cluster-operations/handoff/
  - /riak/kv/2.9.0p3/using/cluster-operations/handoff/
  - /riak/kv/2.9.0p4/using/cluster-operations/handoff/
---


Riak KV provides a command-line interface for enabling and disabling handoff on the fly, without needing to set your configuration and restart the node. To
enable handoff:

```bash
riak-admin handoff enable <inbound|outbound|both> <nodename>
```

You must specify two things when enabling handoff:

* whether you'd like to enable inbound handoff, outbound handoff, or
    both
* the node to be targeted by the command (or all nodes)

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

As for enabling handoff, the `riak-admin disable` command requires that
you specify both both a node or nodes to be targeted by the command and
whether you'd like to disable inbound handoff, outbound handoff, or
both. The `disable` command works just like `enable`. This command
would disable all forms of handoff on all nodes, to give just one
example:

```bash
riak-admin handoff disable both --all
```

## Other Command-line Tools

In addition to enabling and disabling handoff, the
[`riak-admin`]({{<baseurl>}}riak/kv/2.9.0p5/using/admin/riak-admin/) interface enables you to
retrieve a summary of handoff-related activity and other information.

### summary

The `summary` command provides high-level information about active
handoffs in a cluster.

```bash
riak-admin handoff summary
```

This will return a table that will provide the following information
about each node in your cluster:

Header | Description
:------|:-----------
`Node` | The name of the node
`Total` | Total number of active transfers throughout the entire cluster
`Ownership` | Total number of ownership exchanges
`Resize` | Total handoffs related to ring resizing operations (This should always be 0, as the Resize Ring feature has been deprecated)
`Hinted` | Total number of [hinted handoffs](../../reference/handoff#types-of-handoff)
`Repair` | Total repair-related handoffs. More information can be found [here](https://github.com/basho/riak_core/commit/036e409eb83903315dd43a37c7a93c9256863807).

### details

This command provides information only about active transfers.

```bash
riak-admin handoff details
```

If no transfers are currently underway, this command will output `No
ongoing transfers`. Otherwise, you will something like this:

### config

This command displays the values for handoff-specific [configurable parameters]({{<baseurl>}}riak/kv/2.9.0p5/configuring/reference/#intra-cluster-handoff) on each node in
the cluster, including:

* `transfer_limit`
* `handoff.outbound`
* `handoff.inbound`
* `handoff.port`

Descriptions of those parameters can be found in the sections above.
