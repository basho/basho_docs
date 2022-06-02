---
title: "Changing Cluster Information"
description: ""
project: "riak_kv"
project_version: 2.9.4
menu:
  riak_kv-2.9.4:
    name: "Changing Cluster Info"
    identifier: "cluster_operations_change_info"
    weight: 101
    parent: "managing_cluster_operations"
toc: true
aliases:
  - /riak/2.9.4/ops/running/nodes/renaming
  - /riak/kv/2.9.4/ops/running/nodes/renaming
---

[config reference]: {{<baseurl>}}riak/kv/2.9.4/configuring/reference

## Change the Node Name

The node name is an important setting for the Erlang VM, especially when
you want to build a cluster of nodes, as the node name identifies both
the Erlang application and the host name on the network. All nodes in
the Riak cluster need these node names to communicate and coordinate
with each other.

In your configuration files, the node name defaults to `riak@127.0.0.1`.
To change the node name, change the following line:

```riakconf
nodename = riak@127.0.0.1
```

```vmargs
-name riak@127.0.0.1
```

Change it to something that corresponds to either the IP address or a
resolvable host name for this particular node, like so:

```riakconf
nodename = riak@192.168.1.10
```

```vmargs
-name riak@192.168.1.10
```

## Change the HTTP and Protocol Buffers binding address

By default, Riak's HTTP and Protocol Buffers services are bound to the
local interface, i.e. 127.0.0.1, and are therefore unable to serve
requests from the outside network. The relevant setting is in your
[configuration files][config reference]:

```riakconf
# For HTTP
listener.http.internal = 127.0.0.1:8098

# For Protocol Buffers
listener.protobuf.internal = 127.0.0.1:8087
```

```appconfig
% In the riak_api section

% For HTTP
{http, [ {"127.0.0.1", 8098 } ]},

% For Protocol Buffers
{pb, [ {"127.0.0.1", 8087} ] },
```

Either change it to use an IP address that corresponds to one of the
server's network interfaces, or 0.0.0.0 to allow access from all
interfaces and networks, e.g.:

```riakconf
listener.http.internal = 0.0.0.0:8098
```

```appconfig
% In the riak_core section
{http, [ {"0.0.0.0", 8098 } ]},
```

The same configuration should be changed for the Protocol Buffers
interface if you intend on using it (which we recommend). Change the
following line:

```riakconf
listener.protobuf.internal = 0.0.0.0:8087
```

```appconfig
% In the riak_core section
{pb, [ {"0.0.0.0", 8087} ] },
```

## Rename Single Node Clusters

To rename a single-node development cluster:

1. Stop the node with `riak stop`.

2. Change the node's `nodename` parameter in `riak.conf`, or `-name` parameter in `vm.args` to the new name.

3. Change any IP addresses in `riak.conf` or `app.config` if necessary. Specifically: `listener.protobuf.$name`, `listener.http.$name`, and `listener.https.$name` in `riak.conf`, and `pb_ip`, `http`, `https`, and `cluster_mgr` in `app.config`.

4. Delete the contents of the node's `ring` directory. The location of the ring directory is the value for the `ring.state_dir` in `riak.conf`, or `ring_state_dir` in `app.config`.

5. Start Riak on the node with `riak start`.


## Rename Multi-Node Clusters

For multi-node clusters, a rename is a slightly more complex procedure; however, it is very similar to the process for renaming a single node.

Previous to Riak version 1.2, a cluster node's name could only be changed with the [`riak-admin reip`]({{<baseurl>}}riak/kv/2.9.4/using/admin/riak-admin/#reip) command, which involves downtime for the entire cluster. As of Riak version 1.2, that method has been superseded by [`riak-admin cluster force-replace`]({{<baseurl>}}riak/kv/2.9.4/using/admin/riak-admin/#cluster-force-replace), which is safer and does not require cluster wide downtime.

There still exist scenarios that require nodes to be renamed while stopped, such as seeding a cluster with backups from another cluster that does not share the same node names. Please see the [Clusters from Backups](#clusters-from-backups) section for more details on renaming in this scenario.

The following example describes reconfiguring node names with the new `riak-admin cluster force-replace` method.

### Example Scenario

For this example scenario, Riak is operating in a cluster of 5 nodes with the following network configuration:

* `riak@10.1.42.11` on `node1.localdomain` &rarr; IP address changing to 192.168.17.11
* `riak@10.1.42.12` on `node2.localdomain` &rarr; IP address changing to 192.168.17.12
* `riak@10.1.42.13` on `node3.localdomain` &rarr; IP address changing to 192.168.17.13
* `riak@10.1.42.14` on `node4.localdomain` &rarr; IP address changing to 192.168.17.14
* `riak@10.1.42.15` on `node5.localdomain` &rarr; IP address changing to 192.168.17.15

The above list shows the network configuration details for our 5 nodes, including the Erlang node name value, the node's fully qualified domain name, and the new IP address each node will be configured to use.

The nodes in our example cluster are currently configured to use the *10.1.42.* private subnetwork range. Our goal for this example will be to configure the nodes to instead use the  *192.168.17.* private subnetwork range and do so in a rolling fashion without interrupting cluster operation.

### Process

This process can be accomplished in three phases. The details and steps required of each phase are presented in the following section.

1. [Down the node to be reconfigured](#down)
2. [Reconfigure node to use new address](#reconfigure)
3. [Repeat previous steps on each node](#repeat)


<a id="down"></a>
#### Down the Node

1. Stop Riak on `node1.localdomain`:

    ```bash
    riak stop
    ```

    The output should look like this:

    ```
    Attempting to restart script through sudo -H -u riak
    ok
    ```

2. From the `node2.localdomain` node, mark `riak@10.1.42.11` down:

    ```bash 
    riak-admin down riak@10.1.42.11
    ```

    Successfully marking the node down should produce output like this:

    ```bash
    Attempting to restart script through sudo -H -u riak
    Success: "riak@10.1.42.11" marked as down
    ```

    This step informs the cluster that `riak@10.1.42.11` is offline and ring-state transitions should be allowed. While we're executing the `riak-admin down` command from `node2.localdomain` in this example, the command can be executed from any currently running node.

<a id="reconfigure"></a>
#### Reconfigure Node to Use New Address

Reconfigure `node1.localdomain` to listen on the new private IP address *192.168.17.11* by following these steps:

1. Change the node's `nodename` parameter in `riak.conf`, or `-name` parameter in `vm.args`, to reflect the new node name. For example:

    `riak.conf`: `nodename = riak@192.168.17.11`  
    `vm.args` : `-name riak@192.168.17.11`

2. Change any IP addresses to *192.168.17.11* in `riak.conf` or `app.config` as previously described in step 3 of [Single Node Clusters](#single-node-clusters).

3. Rename the node's `ring` directory, the location of which is described in step 4 of [Single Node Clusters](#single-node-clusters).  You may rename it to whatever you like, as it will only be used as a backup during the node renaming process. 

4. Start Riak on `node1.localdomain`.
    
    ```bash    
    riak start
    ```

5. Join the node back into the cluster.

    ```bash
    riak-admin cluster join riak@10.1.42.12
    ```

    Successful staging of the join request should have output like this:

    ```bash
    Attempting to restart script through sudo -H -u riak
    Success: staged join request for 'riak@192.168.17.11' to 'riak@10.1.42.12'
    ```

6. Use `riak-admin cluster force-replace` to change all ownership references from `riak@10.1.42.11` to `riak@192.168.17.11`:

    ```bash
    riak-admin cluster force-replace riak@10.1.42.11 riak@192.168.17.11
    ```

    Successful force replacement staging output looks like this:

    ```bash
    Attempting to restart script through sudo -H -u riak
    Success: staged forced replacement of 'riak@10.1.42.11' with 'riak@192.168.17.11'
    ```

7. Review the new changes with `riak-admin cluster plan:`

    ```bash
    riak-admin cluster plan
    ```

    Example output:

    ```bash
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
    ```

8. Commit the new changes to the cluster with `riak-admin cluster commit`:

    ```bash
    riak-admin cluster commit
    ```

    Output from the command should resemble this example:

    ```bash
    Attempting to restart script through sudo -H -u riak
    Cluster changes committed
    ```

9. Check that the node is participating in the cluster and functioning as expected:

    ```bash
    riak-admin member-status
    ```

    Output should resemble this example:

    ```bash
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
    ```

10. Monitor hinted handoff transfers to ensure they have finished with the `riak-admin transfers` command.

11. Clean up by deleting the renamed `ring` directory once all previous steps have been successfully completed.

{{% note title="Note" %}}
When using the `riak-admin force-replace` command, you will always get a
warning message like: `WARNING: All of 'riak@10.1.42.11' replicas will be
lost`. Since we didn't delete any data files and we are replacing the node
with itself under a new name, we will not lose any replicas.
{{% /note %}}

<a id="repeat"></a>
#### Repeat previous steps on each node

Repeat the steps above for each of the remaining nodes in the cluster.

Use *riak@192.168.17.11* as the target node for further `riak-admin cluster join` commands issued from subsequently reconfigured nodes to join those nodes to the cluster.

```bash
riak-admin cluster join riak@192.168.17.11
```

A successful join request staging produces output similar to this example:

```bash
Attempting to restart script through sudo -H -u riak
Success: staged join request for 'riak@192.168.17.12' to 'riak@192.168.17.11'
```

## Clusters from Backups

The above steps describe a process for renaming nodes in a running cluster. When seeding a new cluster with backups where the nodes must have new names, typically done as a secondary cluster or in a disaster recovery scenario, a slightly different process must be used. This is because the node names must resolve to the new hosts in order for the nodes to start and communicate with each other.

Expanding on the Example Scenario above, the below steps can be used to rename nodes in a cluster that is being restored from backups. The below steps assume every node is offline, and they will indicate when to bring each node online.

#### Bringing Up the First Node

In order to bring our first node online, we'll first need to use the `riak-admin reip` command on a single node. In this example, we'll use `riak@10.1.42.11` as our first node.

1.  In `riak.conf` change `nodename`, `-name` in `vm.args`, from `riak@10.1.42.11` to your new nodename, `riak@192.168.17.11`.

2.  On `node1.localdomain` run `riak-admin reip riak@10.1.42.11 riak@192.168.17.11`. This will change the name of `riak@10.1.42.11` to `riak@192.168.17.11` in the Riak ring.

3.  Start Riak on `node1.localdomain`.

4.  Once Riak is started on `node1.localdomain`, mark the rest of the nodes in the cluster down, using `riak-admin down`. For example, we would down `riak@10.1.42.12` with `riak-admin down riak@10.1.42.12`.

5.  Confirm every other node in the cluster is marked down by running `riak-admin member-status` on `node1.localdomain`:

    ```bash
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

    ```bash
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

1.  On each of the remaining nodes, change `nodename` in `riak.conf`, or `-name` in `vm.args` as described above.

2.  Move aside the ring directory. As in [Multi-Node Clusters](#multi-node-clusters), we will save this ring directory as a backup until were finished.

3.  Start each node. They will start as if they are each a member of their own cluster, but will retain their restored data.

4.  Join each node to our first node using `riak-admin cluster join riak@192.168.17.11`.

5.  Force replace each node with its old node name. For example, `riak-admin cluster force-replace riak@10.1.42.12 riak@192.168.17.12`.

6.  Once the above is complete for each node, run `riak-admin cluster plan` on any node. The output should look similar to below:

    ```bash
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

7.  If the above plan looks correct, commit the cluster changes with `riak-admin cluster commit`.

8.  Once the cluster transition has completed, all node names should be changed and be marked as valid in `riak-admin member-status` like below:

    ```bash
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

