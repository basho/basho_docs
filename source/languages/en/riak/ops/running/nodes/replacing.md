---
title: Replacing a Node
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: advanced
keywords: [operator]
---

At some point, for various reasons, you might need to replace a node in your Riak cluster (which is different from [[recovering a failed node|Recovering a failed node]]). Here is the recommended way to go about replacing a node.

  1. Back up your data directory on the node in question. In this example scenario,
    we'll call the node **riak4**:
    
    ```bash
    sudo tar -czf riak_backup.tar.gz /var/lib/riak /etc/riak
    ```

  2. Download and install Riak on the new node you wish to bring into the cluster and
    have replace the **riak4** node. We'll call this node **riak7** for the purpose of this example.

  3. Start the new **riak7** node with `[[riak start|Command Line Tools#start]]`:

    ```bash
    riak start
    ```

  4. Plan the join of the new **riak7** node to an existing node already participating in
    he cluster; for example **riak0** with the
    `[[riak-admin cluster join|Command-Line Tools - riak-admin#cluster]]` command executed
    on the the new **riak7** node:

    ```bash
    riak-admin cluster join riak0
    ```

  5. Plan the replacement of the existing **riak4** node with the new **riak7** node using
    the `[[riak-admin cluster replace|Command-Line Tools - riak-admin#cluster]]` command:

    ```bash
    riak-admin cluster replace riak4 riak7
    ```

  6. Examine the proposed cluster changes with the
    `[[riak-admin cluster plan|Command-Line Tools - riak-admin#cluster]]` command executed
    on the the new **riak7** node:

    ```bash
    riak-admin cluster plan
    ```

  7. If the changes are correct, you can commit them with the
    `[[riak-admin cluster commit|Command-Line Tools - riak-admin#cluster]]` command:

    ```bash
    riak-admin cluster commit
    ```

If you need to clear the proposed plan and start over, use `[[riak-admin cluster clear|Command-Line Tools - riak-admin#cluster]]`:

```bash
riak-admin cluster clear
```

Once you have successfully replaced the node, it should begin leaving the cluster. You can check on ring readiness after replacing the node with the `[[riak-admin ringready|Command-Line Tools - riak-admin#ringready]]` and `[[riak-admin member-status|Command-Line Tools - riak-admin#member-status]]` commands.

<div class="info">
<div class="title">Ring Settling</div>
You'll need to make sure that no other ring changes occur between the time when you start the new node and the ring settles with the new IP info.

The ring is considered settled when the new node reports <strong>true</strong> using the <code>riak-admin ringready</code> command.
</div>
