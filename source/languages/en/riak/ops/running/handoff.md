---
title: Hinted Handoff
project: riak
version: 2.0.4+
document: reference
audience: intermediate
keywords: [operator, handoff, admin]
---

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
`Node` | The nodename of the node
`Total` | The total number of active transfers throughout the entire cluster
`Ownership` |
`Resize` |
`Hinted` |
`Repair` |

### details

This command provides information only about active transfers.
