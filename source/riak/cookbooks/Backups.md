## Restoring a Node
The method you use to restore a node will differ depending on a combination
of factors, including node name changes and your network environment.

If you are replacing a node with a new node that has the same node name
(typically a fully qualified domain name or IP address), then restoring the
node is is a simple process.

1. Install Riak on the new node.
2. Restore your old node's configuration files, data directory, and ring
   directory.
3. Start the node and verify proper operation with `riak ping`,
   `riak-admin status`, and other methods you use to check node health.

If any node names have been changed (that is, the *-name* argument in the
`vm.args` configuration file for any node is different than the backup being
restored to that node), then all nodes will need to be updated using the
`riak-admin reip` command **prior to starting each node**.

Note that even if only one node name has changed, all nodes must be updated
to reflect the change. On each node, restore the data directories and
configuration files from the backup, and **before starting the node**,
execute the `riak-admin reip` command for each node whose name has changed.

For example, if there are 5 nodes in the cluster with the original node
names: *riak1.example.com* through *riak5.example.com* and their names are
changing to *riak101.example.com* through *riak105.example.com* then run the
following `riak-admin reip` commands **on each stopped node**:

```bash
# run these commands on every node in the cluster while the node is stopped
riak-admin reip riak@riak1.example.com riak@riak101.example.com
riak-admin reip riak@riak2.example.com riak@riak102.example.com
riak-admin reip riak@riak3.example.com riak@riak103.example.com
riak-admin reip riak@riak4.example.com riak@riak104.example.com
riak-admin reip riak@riak5.example.com riak@riak105.example.com
```

The *-name* setting in the `vm.args` configuration file should also be changed
to match the new name in additon to running the commands. If the IP address of
any node has changed, verify that the changes are reflected in the *app.config*
file to ensure that the HTTP and PB interfaces are binding to the correct
addresses.

A robust DNS configuration can simplify the restore process if the IP addresses
of the nodes change, but the hostnames are used for the node names and the
hostnames stay the same. Additonally, if the HTTP and PB interface settings are
configured to bind to all IP interfaces (0.0.0.0), then no changes will need to
be made to the *app.config* file.

It is recommended when performing restore operations involving `riak-admin
reip` to start only one node at a time, and verify that each node that is
started has the correct name for itself and any other nodes whose names have
changed.

First, verify that the correct name is present in the vm.args configuration
file. Then, once the node is started, run `riak attach` to connect to the node.
It may be necessary to enter an Erlang atom and press enter to obtain a prompt,
by typing `x.` and pressing enter. The prompt obtained should contain the
correct node name. Disconnect from the attached session with `^d` (control-d).
Finally, run `riak-admin member_status` to list all of the nodes and verify
that all nodes listed have the correct names.