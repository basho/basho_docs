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
deprecated commands. When using the older commands, you will receive a
deprecation warning.
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

* `status` ---
* `avail` ---
* `ring` --- What percentage of the Riak [[ring|Clusters#The-Ring]] the
  node is responsible for
* `pending` --- The number of pending transfers to or from the node

## members

## members status

## partition

## partitions


