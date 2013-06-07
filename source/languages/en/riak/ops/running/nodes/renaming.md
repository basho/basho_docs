---
title: Renaming Nodes
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: advanced
keywords: [operator]
moved: {
    '1.4.0-': '/cookbooks/Renaming-Nodes/'
}
---

Previous to Riak version 1.2, a cluster node's IP address could be changed
the `[[riak-admin reip|Command-Line-Tools---riak-admin#reip]]` command,
which involves downtime for the entire cluster.

As of Riak version 1.2, that method has been superseded by
`[[riak-admin cluster force-replace|Command-Line-Tools---riak-admin#cluster-force-replace]]`
which is safer and does not require cluster wide downtime.

The following example describes reconfiguring node IP addresses with the
new `riak-admin cluster force-replace` method.

## Example Scenario

For this example scenario, Riak is operating in a cluster of 5 nodes with the
following network configuration:

* `riak@10.1.42.11` on `node1.localdomain`
  &rarr; IP address changing to 192.168.17.11
* `riak@10.1.42.12` on `node2.localdomain`
  &rarr; IP address changing to 192.168.17.12
* `riak@10.1.42.13` on `node3.localdomain`
  &rarr; IP address changing to 192.168.17.13
* `riak@10.1.42.14` on `node4.localdomain`
  &rarr; IP address changing to 192.168.17.14
* `riak@10.1.42.15` on `node5.localdomain`
  &rarr; IP address changing to 192.168.17.15

The above list shows the network configuration details for our 5 nodes,
including the Erlang node name value, the node's fully qualified domain name,
and the new IP address each node will be configured to use.

The nodes in our example cluster are currently configured to use the
*10.1.42.* private subnetwork range. Our goal for this example will be to
configure the nodes to instead use the private subnetwork *192.168.17.* and
do so in a rolling fashion without interrupting cluster operation.

## Process
This process can be accomplished in three phases. The details and steps
required of each phase are presented in the following section.

1. [[Down the node to be reconfigured|Renaming-Nodes#down]]
2. [[Reconfigure node to use new address|Renaming-Nodes#reconfigure]]
3. [[Repeat previous steps on each node|Renaming-Nodes#repeat]]


<a id="down"></a>
### Down the Node

Stop Riak on `node1.localdomain`:

```
riak stop
```

The output should look like this:

```
Attempting to restart script through sudo -H -u riak
ok
```

**From the `node2.localdomain` node**, mark `riak@10.1.42.11` down:

```
riak-admin down riak@10.1.42.11
```

Successfully marking the node down should produce output like this:

```
Attempting to restart script through sudo -H -u riak
Success: "riak@10.1.42.11" marked as down
```

This step informs the cluster that `riak@10.1.42.11` is offline and
ring-state transitions should be allowed. While we're executing the
`riak-admin down` command from `node2.localdomain` in this example, the
command can be executed from any currently running node.

<a id="reconfigure"></a>
### Reconfigure Node to Use New Address

Reconfigure `node1.localdomain` to listen on the new private IP addresses
*192.168.17.11* by following these steps:

1. Edit the node's `vm.args` configuration file and set the `-name` argument
as follows:

        -name riak@192.168.17.11

2. Change IP addresses to *192.168.17.11* in `app.config` as appropriate.
   Specifically, set `pb_ip`, `http`, `https`, and `cluster_mgr` parameters
   to the new address.

3. Rename the node's `ring` directory. The location of the ring directory is
   listed in the `app.config` file.

4. Start Riak on `node1.localdomain`.
        riak start

5. Join the node back into the cluster.

        riak-admin cluster join riak@10.1.42.12

     Successful staging of the join request should have output like this:

        Attempting to restart script through sudo -H -u riak
        Success: staged join request for 'riak@192.168.17.11' to 'riak@10.1.42.12'

6. Use `riak-admin cluster force-replace` to change all ownership references
   from `riak@10.1.42.11` to `riak@192.168.17.11`:

        riak-admin cluster force-replace riak@10.1.42.11 riak@192.168.17.11

     Successful force replacement staging output looks like this:

        Attempting to restart script through sudo -H -u riak
        Success: staged forced replacement of 'riak@10.1.42.11' with 'riak@192.168.17.11'

7. Review the new changes with `riak-admin cluster plan:`

        riak-admin cluster plan

     Example output:

        Attempting to restart script through sudo -H -u riak
        =========================== Staged Changes ============================
        Action         Nodes(s)
        -----------------------------------------------------------------------
        join           'riak@192.168.17.11'
        force-replace  'riak@10.1.42.11' with 'riak@192.168.17.11'
        -----------------------------------------------------------------------

        WARNING: All of 'riak@10.1.42.11' replicas will be lost

        NOTE: Applying these changes will result in 1 cluster transition

        #######################################################################
                             After cluster transition 1/1
        #######################################################################

        ============================= Membership ==============================
        Status     Ring    Pending    Node
        -----------------------------------------------------------------------
        valid      20.3%      --      'riak@192.168.17.11'
        valid      20.3%      --      'riak@10.1.42.12'
        valid      20.3%      --      'riak@10.1.42.13'
        valid      20.3%      --      'riak@10.1.42.14'
        valid      18.8%      --      'riak@10.1.42.15'
        -----------------------------------------------------------------------
        Valid:5 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

        Partitions reassigned from cluster changes: 13
        13 reassigned from 'riak@10.1.42.11' to 'riak@192.168.17.11'

     Note: When using the `riak-admin force-replace` command, you will always get a warning message like: `WARNING: All of 'riak@10.1.42.11' replicas will be lost`.
     Since we didn't delete any data files and we are replacing the node with itself under a new name, we will not lose any replicas.

8. Commit the new changes to the cluster with `riak-admin cluster commit`:

        riak-admin cluster commit

     Output from the command should resemble this example:

        Attempting to restart script through sudo -H -u riak
        Cluster changes committed

9. Check that the node is participating in the cluster and functioning
   as expected:

        riak-admin member-status

     Output should resemble this example:

        Attempting to restart script through sudo -H -u riak
        ============================= Membership ==============================
        Status     Ring    Pending    Node
        -----------------------------------------------------------------------
        valid      20.3%      --      'riak@192.168.17.11'
        valid      20.3%      --      'riak@10.1.42.12'
        valid      20.3%      --      'riak@10.1.42.13'
        valid      20.3%      --      'riak@10.1.42.14'
        valid      18.8%      --      'riak@10.1.42.15'
        -----------------------------------------------------------------------
        Valid:5 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

10. Monitor hinted handoff transfers to ensure they have finished with the
    `riak-admin transfers` command.

11. Clean up by deleting the renamed `ring` directory once all previous steps
    have been successfully completed.

<a id="repeat"></a>
### Repeat previous steps on each node

Repeat the steps above for each of the the remaining nodes in the cluster.

Use *riak@192.168.17.11* as the target node for further
`riak-admin cluster join` commands issued from subsequently reconfigured
nodes to join those nodes to the cluster.

```
riak-admin cluster join riak@192.168.17.11
```

A successful join request staging produces output similar to this example:

```
Attempting to restart script through sudo -H -u riak
Success: staged join request for 'riak@192.168.17.12' to 'riak@192.168.17.11'
```
