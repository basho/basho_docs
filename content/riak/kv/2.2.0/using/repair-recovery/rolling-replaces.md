---
title: "Rolling Replaces"
description: ""
project: "riak_kv"
project_version: "2.2.0"
menu:
  riak_kv-2.2.0:
    name: "Rolling Replaces"
    identifier: "repair_recover_replace"
    weight: 106
    parent: "managing_repair_recover"
toc: true
---

[upgrade]: /riak/kv/2.2.0/setup/upgrading/cluster/
[rolling restarts]: /riak/kv/2.2.0/using/repair-recovery/rolling-restart/

Riak KV functions as a multi-node system, so cluster-level [version upgrades][upgrade] and [restarts][rolling restarts] can be performed on a node-by-node or "rolling" basis.

The following steps should be undertaken on each Riak KV node that you wish to replace:

1\. Create a free node either by:

  a\. Creating an additional node with similar specifications to the other nodes in the cluster.

  b\. Leave a node that is currently in the cluster:

  ```bash
  riak-admin cluster leave «nodename»
  ```

  Wait for all transfers to complete:

  ```bash
  riak-admin transfers
  ```

2\. Start by joining the free node to your cluster:

```bash
riak-admin cluster join «nodename_leaving_cluster»
```

3\. Next, set the free node to replace an existing node with:

```bash
riak-admin cluster replace «nodename_leaving_cluster» «nodename_joining_cluster»
```

4\. Then plan the cluster transition:

```bash
riak-admin cluster plan
```

5\. And commit the changes:

```bash
riak-admin cluster commit
```

6\. Wait for all transfers to complete:

```bash
riak-admin transfers
```

7\. Repeat steps 2-6 above until each node has been replaced.

8\. Join the final replaced node back into the cluster or decommission the additional node that was created.
