---
title: Recovering a Failed Node
project: riak
version: 0.10.0+
document: cookbook
toc: true
audience: advanced
keywords: [troubleshooting]
moved: {
    '1.4.0-': '/cookbooks/Recovering-a-Failed-Node'
}
---

## General Recovery Notes

A Riak node can fail for many reasons, but a handful of checks can cover the
most glaring problems. Check for RAID and file system consistency, faulty
memory, fully functional network connections, etc.

When a failed node comes back up, ensure that it has the same node name as
before it crashed.  Changing the node name makes the cluster assume this is an
entirely new node, leaving the old one still as part of the ring.

During the recovery process hinted handoff will kick in and update the data on
the recovered node with updates accepted from other nodes in the cluster. Your
cluster may temporarily return "not found" for objects that are currently
being handed off (see our page on [[Eventual Consistency]] for more details on
these scenarios, in particular how the system behaves while the failed node is
not part of the cluster).

## Node Name Changed

If you are recovering from a scenario where node name changes are out of your
control, you'll want to notify the cluster of its *new* name using the
following steps:

1. Stop the node you wish to rename with `riak stop` or `service riak stop`
2. Mark the node down from another node in the cluster with `riak-admin down <PREVIOUS_NODE_NAME>`
3. Update the node name in Riak's configuration files
4. Delete the ring state directory (usually `/var/lib/riak/ring`)
5. Start the node with `riak start` or `service riak start`
6. Ensure that the node comes up as a single instance (verify this with `riak-admin member-status`)
7. Join the node to the cluster with `riak-admin cluster join <NODE_NAME_OF_A_MEMBER_OF_THE_CLUSTER>`
8. Replace the old instance of the node with the new with `riak-admin cluster force-replace <PREVIOUS_NODE_NAME> <NEW_NODE_NAME>`
9. Review the changes with: `riak-admin cluster plan` and commit them with `riak-admin cluster commit`
