---
title: "Recovering a Failed Node"
description: ""
project: "riak_kv"
project_version: 2.9.7
menu:
  riak_kv-2.9.7:
    name: "Recover a Failed Node"
    identifier: "repair_recover_failed_node"
    weight: 104
    parent: "managing_repair_recover"
toc: true
aliases:
  - /riak/2.9.7/ops/running/recovery/failed-node
  - /riak/kv/2.9.7/ops/running/recovery/failed-node
---

## General Recovery Notes

A Riak node can fail for many reasons, but a handful of checks enable you to
uncover some of the most common problems that can lead to node failure, 
such as checking for RAID and filesystem consistency or faulty memory and 
ensuring that your network connections are fully functioning.

When a node fails and is then brought back into the cluster, make sure that it has the same node name that it did before it crashed. If the name has changed, the cluster will assume that the node is entirely new and that the crashed node is still part of the cluster.

During the recovery process, hinted handoff will kick in and update the data on
the recovered node with updates accepted from other nodes in the cluster. Your
cluster may temporarily return `not found` for objects that are currently
being handed off (see our page on [Eventual Consistency](../../../learn/concepts/eventual-consistency) for more details on
these scenarios, in particular how the system behaves while the failed node is
not part of the cluster).

## Node Name Changed

If you are recovering from a scenario in which node name changes are out of
your control, you'll want to notify the cluster of its *new* name using the
following steps:

1. Stop the node you wish to rename:

    ```bash
    riak stop
    ```


2. Mark the node down from another node in the cluster:

    ```bash
    riak-admin down <previous_node_name>
    ```

3. Update the node name in Riak's configuration files:

    ```riakconf
    nodename = <updated_node_name>
    ```

    ```vmargs
    -name <updated_node_name>
    ```

4. Delete the ring state directory (usually `/var/lib/riak/ring`).

5. Start the node again:

    ```bash
    riak start
    ```

6. Ensure that the node comes up as a single instance:

    ```bash
    riak-admin member-status
    ```

    The output should look something like this:

    ```
    ========================= Membership ==========================
Status     Ring    Pending    Node
---------------------------------------------------------------
valid     100.0%      --      'dev-rel@127.0.0.1'
---------------------------------------------------------------
Valid:1 / Leaving:0 / Exiting:0 / Joining:0 / Down:0
    ```

7. Join the node to the cluster:

    ```bash
    riak-admin cluster join <node_name_of_a_member_of_the_cluster>
    ```

8. Replace the old instance of the node with the new:

    ```bash
    riak-admin cluster force-replace <previous_node_name> <new_node_name>
    ```

9. Review the changes:

    ```bash
    riak-admin cluster plan
    ```

    Finally, commit those changes:

    ```bash
    riak-admin cluster commit
    ```




