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

The above steps describe a process for renaming nodes in a running cluster. When building a new cluster using backups from another cluster, it may not be possible to start every node in the cluster to perform the above steps. If the nodenames, as set by the `-name` paramenter in `vm.args`, do not resolve to the new hosts, the nodes will be unable to communicate with each other to perform the `force-replace` operations.

Expanding on the Example Scenario above, the below steps can be used to rename nodes in a cluster that is being restored from backups.

**Bringing Up the First Node**  
Starting with the first node, which has 192.168.17.11 as its IP, and was created with the backups from 10.1.42.11:

- Leave the `-name` parameter in the `vm.args` as `riak@10.1.42.11` and add an entry in `/etc/hosts` mapping 10.1.42.11 to 192.168.17.11.
- Start the node with `riak start`.

At this time, the node should start, and its members in `riak-admin member-status` should be listed as those from the backed up cluster.

- Mark the rest of the nodes listed in `riak-admin member-status` as down with `riak-admin down <node_name>`. For example, `riak@10.1.42.12` would be marked down with `riak-admin down riak@10.1.42.12`.

Marking these nodes down, which are part of the cluster we backed up, will allow us to make changes to the cluster using the `riak-admin cluster` commands.

**Bringing Up and Renaming the Rest of the Nodes**  
On the second node, which has the 192.168.17.12 as its IP:

- Add an entry in `/etc/hosts` mapping 10.1.42.11 to 192.168.17.11. This will allow us to connect to our first node.
- Move aside the contents of the `ring` directory.
- Change the `-name` parameter in the `vm.args` to `riak@192.168.17.12`.
- Start the node with `riak start`.
- Perform the steps from step 5 of [[Reconfigure Node to Use New Address|Renaming-Nodes#reconfigure]] on 192.168.17.12. Those steps are:
      
      ```
      riak-admin cluster join riak@10.1.42.11
      riak-admin cluster force-replace riak@10.1.42.12 riak@192.168.17.12
      riak-admin cluster plan
      riak-admin cluster commit
      ```

At this time, `riak@10.1.42.12` will have been successfully replaced with `riak@192.168.17.12` and the cluster will have two members currently up, `riak@10.1.42.11` and `riak@192.168.17.12`.

The steps performed on 192.168.17.12 can be repeated on 192.168.17.13, 192.168.17.14, and 192.168.17.15.

Once complete, a `riak-admin member-status` will look like the one below:

```
Attempting to restart script through sudo -H -u riak
============================= Membership ==============================
Status     Ring    Pending    Node
-----------------------------------------------------------------------
valid      20.3%      --      'riak@10.1.42.11'
valid      20.3%      --      'riak@192.168.17.12'
valid      20.3%      --      'riak@192.168.17.13'
valid      20.3%      --      'riak@192.168.17.14'
valid      18.8%      --      'riak@192.168.17.15'
-----------------------------------------------------------------------
Valid:5 / Leaving:0 / Exiting:0 / Joining:0 / Down:0
```

**Renaming the First Node**

The final step in this process is to rename our first node.

On 192.168.17.11:

- Stop Riak with `riak stop`.
- Move aside the contents of the `ring` directory.
- Change the `-name` parameter in the `vm.args` from `riak@10.1.42.11` to `riak@192.168.17.11`.
- Start Riak with `riak start`.

On 192.168.17.12:

- Mark `riak@10.1.42.11` down with `riak-admin down riak@10.1.42.11`.

On 192.168.17.11, run the same steps to join and force-replace from above:
      
```
riak-admin cluster join riak@192.168.17.12
riak-admin cluster force-replace riak@10.1.42.11 riak@192.168.17.11
riak-admin cluster plan
riak-admin cluster commit
```

The `riak-admin member-status` will now show every node successfully renamed:

```
Attempting to restart script through sudo -H -u riak
============================= Membership ==============================
Status     Ring    Pending    Node
-----------------------------------------------------------------------
valid      20.3%      --      'riak@192.168.17.11'
valid      20.3%      --      'riak@192.168.17.12'
valid      20.3%      --      'riak@192.168.17.13'
valid      20.3%      --      'riak@192.168.17.14'
valid      18.8%      --      'riak@192.168.17.15'
-----------------------------------------------------------------------
Valid:5 / Leaving:0 / Exiting:0 / Joining:0 / Down:0
```

The entry for 10.1.42.11 can now be removed from the `/etc/hosts` file of each node, as the renaming is complete.