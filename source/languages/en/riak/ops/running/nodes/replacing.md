---
title: Replacing a Node
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: advanced
keywords: [operator]
moved: {
    '1.4.0-': '/cookbooks/Replacing-a-Node'
}
---

At some point, for various reasons, you might need to replace a node in
your Riak cluster (which is different from [[recovering a failed
node|Recovering a failed node]]). Here is the recommended way to go
about replacing a node.

1. Back up your data directory on the node in question. In this example
scenario, we'll call the node `riak4`:

    ```bash
    sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
    ```

    If you have any unforeseen issues at any point in the node
    replacement process, you can restore the node's data from this
    backup.

2. Download and install Riak on the new node you wish to bring into the
cluster and have it replace the `riak4` node. We'll call the new node
`riak7` for the purpose of this example.

3. Start the new `riak7` node with `[[riak start|Command Line
Tools#start]]`:

    ```bash
    riak start
    ```

4. Plan the join of the new `riak7` node to an existing node already
participating in the cluster; for example `riak0` with the `[[riak-admin
cluster join|riak-admin Command Line#cluster]]` command executed on the
new `riak7` node:

    ```bash
    riak-admin cluster join riak0
    ```

5. Plan the replacement of the existing `riak4` node with the new
`riak7` node using the `[[riak-admin cluster replace|riak-admin
Command Line#cluster]]` command:

    ```bash
    riak-admin cluster replace riak4 riak7
    ```

    <div class=info>
    <div class=title>Single Nodes</div>
    If a node is started singly using default settings (as, for example,
    you might do when you are building your first test environment), you
    will need to remove the ring files from the data directory after you
    edit `/etc/vm.args`. `riak-admin cluster replace` will not work as
    the node has not been joined to a cluster.
    </div>

6. Examine the proposed cluster changes with the `[[riak-admin cluster
plan|riak-admin Command Line#cluster]]` command executed on the new
`riak7` node:

    ```bash
    riak-admin cluster plan
    ```

7. If the changes are correct, you can commit them with the
`[[riak-admin cluster commit|riak-admin Command Line#cluster]]` command:

    ```bash
    riak-admin cluster commit
    ```

    If you need to clear the proposed plan and start over, use `[[riak-admin cluster clear|riak-admin Command Line#cluster]]`:

    ```bash
    riak-admin cluster clear
    ```

Once you have successfully replaced the node, it should begin leaving
the cluster. You can check on ring readiness after replacing the node
with the `[[riak-admin ringready|riak-admin Command Line#ringready]]`
and `[[riak-admin member-status|riak-admin Command Line#member-status]]`
commands.

<div class="info">
<div class="title">Ring Settling</div>
You'll need to make sure that no other ring changes occur between the
time when you start the new node and the ring settles with the new IP
info.

The ring is considered settled when the new node reports `true` when you
run the `riak-admin ringready` command.
</div>
