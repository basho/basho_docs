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

* the node to be targeted by the command
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


(equivalent) commands would enable handoff on the node
`riak3@100.0.0.1`:

```bash
riak-admin handoff enable --node riak2@100.0.0.1
riak-admin handoff enable -n riak@100.0.0.1
```

Alternatively, you can enable handoff on all nodes at the same time
using either the `-a` or `--all` flag, as in this example:

```bash
riak-admin handoff enable --all
riak-admin handoff enable -a
```

### disable



### summary

### details
