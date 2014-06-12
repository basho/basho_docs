---
title: Renaming Nodes
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: advanced
keywords: [operator]
moved: {
    '1.4.0-': '/cookbooks/Renaming-Nodes'
}
---

## Single Node Clusters

For a single-node development cluster, renaming a Riak node is quite simple:

1. Stop the node with `riak stop`.

2. Edit the node's `vm.args` configuration file and set the `-name` argument to the new name.

3. Change IP addresses in `app.config` if necessary. Specifically, set `pb_ip`, `http`, `https`, and `cluster_mgr` parameters to the new address.

4. Delete the contents of the node's `ring` directory. The location of the ring directory is the value for the `ring_state_dir` key within the `app.config` file.

5. Start Riak on the node with `riak start`.


## Multi-Node Clusters

For multinode clusters, a rename is a slightly more complex procedure; however, it
is very similar to the process for renaming a single node.

Previous to Riak version 1.2, a cluster node's IP address could be changed
the `[[riak-admin reip|riak-admin Command Line#reip]]` command,
which involves downtime for the entire cluster.

As of Riak version 1.2, that method has been superseded by
`[[riak-admin cluster force-replace|riak-admin Command Line#cluster-force-replace]]` which is safer and does not require cluster wide downtime.

The following example describes reconfiguring node IP addresses with the new `riak-admin cluster force-replace` method.

### Example Scenario

For this example scenario, Riak is operating in a cluster of 5 nodes with the
following network configuration:

* `riak@10.1.42.11` on `node1.localdomain` &rarr; IP address changing to 192.168.17.11
* `riak@10.1.42.12` on `node2.localdomain` &rarr; IP address changing to 192.168.17.12
* `riak@10.1.42.13` on `node3.localdomain` &rarr; IP address changing to 192.168.17.13
* `riak@10.1.42.14` on `node4.localdomain` &rarr; IP address changing to 192.168.17.14
* `riak@10.1.42.15` on `node5.localdomain` &rarr; IP address changing to 192.168.17.15

The above list shows the network configuration details for our 5 nodes,
including the Erlang node name value, the node's fully qualified domain name,
and the new IP address each node will be configured to use.

The nodes in our example cluster are currently configured to use the
*10.1.42.* private subnetwork range. Our goal for this example will be to
configure the nodes to instead use the private subnetwork *192.168.17.* and
do so in a rolling fashion without interrupting cluster operation.

### Process

This process can be accomplished in three phases. The details and steps
required of each phase are presented in the following section.

1. [[Down the node to be reconfigured|Renaming-Nodes#down]]
2. [[Reconfigure node to use new address|Renaming-Nodes#reconfigure]]
3. [[Repeat previous steps on each node|Renaming-Nodes#repeat]]


<a id="down"></a>
#### Down the Node

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
#### Reconfigure Node to Use New Address

Reconfigure `node1.localdomain` to listen on the new private IP addresses
*192.168.17.11* by following these steps:

1. Edit the node's `vm.args` configuration file and set the `-name` argument
as follows:

        -name riak@192.168.17.11

2. Change IP addresses to *192.168.17.11* in `app.config` as appropriate.
   Specifically, set `pb_ip`, `http`, `https`, and `cluster_mgr` parameters
   to the new address. {{1.4.0-}}

2. Change IP addresses to *192.168.17.11* in `app.config` as appropriate.
   Specifically, set `pb`, `http`, `https`, and `cluster_mgr` parameters
   to the new address. {{1.4.0+}}

3. Rename the node's `ring` directory. The location of the ring directory is
   the value for the `ring_state_dir` key within the `app.config` file.  You may 
   rename it to whatever you like, as it will only be used as a backup during the
   node renaming process. 

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

<div class="note"><div class="title">Note</div>
When using the `riak-admin force-replace` command, you will always get a warning message like: `WARNING: All of 'riak@10.1.42.11' replicas will be lost`. Since we didn't delete any data files and we are replacing the node with itself under a new name, we will not lose any replicas.
</div>

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
#### Repeat previous steps on each node

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

### Renaming a Cluster from Backups

The above steps describe a process for renaming nodes in a running cluster. When seeding a new cluster with backups where the nodes must have new names, typically done as a secondary cluster or in a disaster recovery scenario, a slightly different process must be used. This is because the nodenames, set as `nodename` in `riak.conf`, must resolve to the new hosts in order for the nodes to start and communicate with each other.

Expanding on the Example Scenario above, the below steps can be used to rename nodes in a cluster that is being restored from backups. The below steps assume every node is offline, and they will indicate when to bring each node online.

#### Bringing Up the First Node

In order to bring our first node online, we'll first need to utilize the `riak-admin reip` command on a single node. In this example, we'll use `riak@10.1.42.11` as our first node.

1.  In `riak.conf` change `nodename` from `riak@10.1.42.11` to your new nodename, `riak@192.168.17.11`.

2.  On `node1.localdomain` run `riak-admin reip riak@10.1.42.11 riak@192.168.17.11`. This will change the name of `riak@10.1.42.11` to `riak@192.168.17.11` in the Riak ring.

3.  Start Riak on `node1.localdomain`.

4.  Once Riak is started on `node1.localdomain`, mark the rest of the nodes in the cluster down, using `riak-admin down`. For example, we would down `riak@10.1.42.12` with `riak-admin down riak@10.1.42.12`.

5.  Confirm every other node in the cluster is marked down by running `riak-admin member-status` on `node1.localdomain`:

    ```
    ================================= Membership ==================================
    Status     Ring        Pending    Node
    -------------------------------------------------------------------------------
    valid       20.3%      --      'riak@192.168.17.11'
    down        20.3%      --      'riak@10.1.42.12'
    down        20.3%      --      'riak@10.1.42.13'
    down        20.3%      --      'riak@10.1.42.14'
    down        18.8%      --      'riak@10.1.42.15'
    -------------------------------------------------------------------------------
    Valid:1 / Leaving:0 / Exiting:0 / Joining:0 / Down:4

    ```

6.  Ensure `riak@192.168.17.11` is listed as the claimant by running `riak-admin ring-status` on `node1.localdomain`:

    ```
    ================================== Claimant ===================================
    Claimant:  'riak@192.168.17.11'
    Status:     up
    Ring Ready: true

    ============================== Ownership Handoff ==============================
    No pending changes.

    ============================== Unreachable Nodes ==============================
    All nodes are up and reachable
    ```

Once all nodes are marked as down and our first node is listed as the claimant, we can proceed with the rest of the nodes.

#### Bringing Up the Remaining Nodes

1.  On each of the remaining nodes, change nodename in `riak.conf` as described above.

2.  Move aside the ring directory, typically found in `/var/lib/riak/ring`.

3.  Start each node. They will start as if they are each a member of their own cluster, but will retain their restored data.

4.  Join each node to our first node using `riak-admin cluster join riak@192.168.17.11`.

5.  Force replace each node with its old name. For example, `riak-admin cluster force-replace riak@10.1.42.12 riak@192.168.17.12`.

6.  Once the above is complete for each node, run `riak-admin cluster plan` on any node. The output should look similar to below:

    ```
    =============================== Staged Changes ================================
    Action         Details(s)
    -------------------------------------------------------------------------------
    force-replace  'riak@10.1.42.12' with 'riak@192.168.17.12'
    force-replace  'riak@10.1.42.13' with 'riak@192.168.17.13'
    force-replace  'riak@10.1.42.14' with 'riak@192.168.17.14'
    force-replace  'riak@10.1.42.15' with 'riak@192.168.17.15'
    join           'riak@192.168.17.12'
    join           'riak@192.168.17.13'
    join           'riak@192.168.17.14'
    join           'riak@192.168.17.15'
    -------------------------------------------------------------------------------

    WARNING: All of 'riak@10.1.42.12' replicas will be lost
    WARNING: All of 'riak@10.1.42.13' replicas will be lost
    WARNING: All of 'riak@10.1.42.14' replicas will be lost
    WARNING: All of 'riak@10.1.42.15' replicas will be lost

    NOTE: Applying these changes will result in 1 cluster transition

    ###############################################################################
                             After cluster transition 1/1
    ###############################################################################

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid       20.3%      --      'riak@192.168.17.11'
    valid       20.3%      --      'riak@192.168.17.12'
    valid       20.3%      --      'riak@192.168.17.13'
    valid       20.3%      --      'riak@192.168.17.14'
    valid       18.8%      --      'riak@192.168.17.15'
    -------------------------------------------------------------------------------
    Valid:5 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

    Partitions reassigned from cluster changes: 51
      13 reassigned from 'riak@10.1.42.12' to 'riak@192.168.17.12'
      13 reassigned from 'riak@10.1.42.13' to 'riak@192.168.17.13'
      13 reassigned from 'riak@10.1.42.14' to 'riak@192.168.17.14'
      12 reassigned from 'riak@10.1.42.15' to 'riak@192.168.17.15'
    ```

    The plan will alert you that replicas will be lost, however in this application of `riak-admin force-replace` that is not the case.

7.  If the above plan looks correct, commit the cluster changes with `riak-admin cluster commit`.

8.  Once the cluster transition has completed, all node names should be changed and be marked as valid in `riak-admin member-status` like below, completing the rename from backups:

    ```
    ================================= Membership ==================================
    Status       Ring       Pending    Node
    -------------------------------------------------------------------------------
    valid        20.3%      --        'riak@192.168.17.11'
    valid        20.3%      --        'riak@192.168.17.12'
    valid        20.3%      --        'riak@192.168.17.13'
    valid        20.3%      --        'riak@192.168.17.14'
    valid        18.8%      --        'riak@192.168.17.15'
    -------------------------------------------------------------------------------
    Valid:5 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

    ```


