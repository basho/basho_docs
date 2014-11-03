---
title: Basic Cluster Setup
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: beginner
keywords: [operator, cluster]
moved: {
    '1.4.0-': '/cookbooks/Basic-Cluster-Setup'
}
---

Configuration of a Riak cluster requires instructing a node to listen on
a non-local interface, i.e. not `127.0.0.1`, and then joining nodes
together for cluster participation.

Most configuration changes will be applied to the
`[[app.config|Configuration Files]]` file located in your
`rel/riak/etc/` directory (if you compiled from source) or `/etc/riak/`
(if you used a binary install of Riak).

The commands below presume that you are running from a source install,
but if you have installed Riak with a binary install, you can substitute
the usage of `bin/riak` with `sudo /usr/sbin/riak` and `bin/riak-admin`
with `sudo /usr/sbin/riak-admin`.

<div class="note">
<div class="title">Note on changing the <code>-name</code> value</div>
If possible, you should avoid starting Riak prior to editing the
<code>-name</code> parameter in <code>vm.args</code> as described below. If
you have already started Riak with the default settings, you cannot change
the <code>-name</code> setting and then successfully restart the
node.

If you cannot restart after changing -name value you have two options:
<ol>
<li>Discard the existing ring metadata by removing the contents of
the <code>ring</code> directory. This will require rejoining all nodes
into a cluster again.</li>
<li>Rename the node using the <code>[[riak-admin cluster 
replace|riak-admin Command Line#cluster-replace]]</code> command. This
will not work if you have previously only started riak with a single
node.</li>
</ol>
</div>

## Configure the First Node

First, stop your Riak node if it is currently running:

```bash
bin/riak stop
```

Change the default IP address located under `http{}` in the `riak_core`
section of `app.config`. The port, `8098`, does not need to be changed.
Lets say our machine's IP address is 192.168.1.10.

**Note**: This is just an example IP address. Yours will be specific to
your machine.

```appconfig
{riak_core, [
    % ...
    {http, [ {"127.0.0.1", 8098 } ]},
    % ...
]}
```

becomes

```appconfig
{riak_core, [
    % ...
    {http, [ {"192.168.1.10", 8098 } ]},
    % ...
]}
```

The same configuration should be changed for the Protocol Buffers
interface if you intend on using it:

{{#1.4.0-}}

```appconfig
{riak_core, [
    % ...
    {pb_ip,   "127.0.0.1" },
    % ...
]}
```    

becomes

```appconfig
{riak_core, [
    % ...
    {pb_ip,   "192.168.1.10" },
    % ...
]}
```   

{{/1.4.0-}}
{{#1.4.0+}}

```appconfig
{riak_core, [
    % ...
    {pb, [ {"127.0.0.1", 8098 } ]},
    % ...
]}
``` 

becomes

```appconfig
{riak_core, [
    % ...
    {pb, [ {"192.168.1.10", 8098 } ]},
    % ...
]}
``` 
{{/1.4.0+}}


Next edit the `etc/vm.args` file and change the `-name` to the correct hostname:

```vmargs
-name riak@127.0.0.1
```

becomes

```vmargs
-name riak@server.example.com
```

<div class="note">
<div class="title">Note on node names</div>
Use fully qualified domain names (FQDNs) rather than IP addresses for
the cluster member node names. For example, `riak@cluster.example.com`
and `riak@192.168.1.10` are both acceptable node naming schemes, but
using the FQDN style is preferred.

Once a node has been started, in order to change the name you must
either remove ring files from the data directory,
`[[riak-admin reip|riak-admin Command Line#reip]]` the node, or
`[[riak-admin cluster force-replace|riak-admin Command Line#cluster-force-replace]]`
the node.
</div>

Start the Riak node:

```bash
bin/riak start
```

If the Riak node has been previously started, you must use
`riak-admin cluster replace` to change the node name and
update the node's ring file.

```bash
bin/riak-admin cluster replace riak@127.0.0.1 riak@192.168.1.10
```

<div class="note">
<div class="title">Note on single nodes</div>
If a node is started singly using default settings (as, for example,
you might do when you are building your first test environment), you
will need to remove the ring files from the data directory after you
edit `etc/vm.args`. `riak-admin cluster replace` will not work, as the
node has not been joined to a cluster.
</div>

As with all cluster changes, you need to view the cluster plan by
running `riak-admin cluster plan` and then run `riak-admin cluster
commit` to finalize the changes.

The node is now properly configured to join other nodes for cluster
participation. Proceed to adding a second node to the cluster.

## Add a Second Node to Your Cluster

Repeat the above steps for a second host on the same network. Once the
second node has started, use `riak-admin cluster join` to join the
second node to the first node, thereby creating an initial Riak cluster.

```bash
bin/riak-admin cluster join riak@192.168.1.10
```

Output from the above should resemble the following:

```
Success: staged join request for `riak@192.168.1.11` to `riak@192.168.1.10`
```

Next, plan and commit the changes:

```bash
bin/riak-admin cluster plan
bin/riak-admin cluster commit
```

After the last command, you should see:

```
Cluster changes committed
```

If your output was similar, then the second Riak node is now part of the
cluster and has begun syncing with the first node. Riak provides several
ways to determine the cluster ring status. Here are two ways to examine
your Riak cluster's ring:

1. Using the `riak-admin` command:

    ```bash
    bin/riak-admin status | grep ring_members
    ```

    With output resembling the following:

    ```
    ring_members : ['riak@192.168.1.10','riak@192.168.1.11']
    ```

2. Using the `riak attach` command:

    ```bash
    riak attach
    ```

    That should open up the Erlang console, where you should enter the
    following:

    ```erlang
    1> {ok, R} = riak_core_ring_manager:get_my_ring().
    ```

    That should return output like this:

    ```
    {ok,{chstate,'riak@192.168.1.10',.........
    (riak@192.168.52.129)2> riak_core_ring:all_members(R).
    ['riak@192.168.1.10','riak@192.168.1.11']
    ```

To join additional nodes to your cluster, repeat the above steps. You
can also find more detailed instructions about [[Adding and Removing
Nodes]] to/from a cluster.

<div class="note">
<div class="title">Note on ring creation size</div>
All nodes in the cluster must have the same initial
`ring_creation_size` setting in order to join the cluster and to
participate in cluster activity. This setting can be adjusted in
`app.config`.

Check the value of all nodes if you receive a message like this:

```
Failed: riak@10.0.1.156 has a different ring_creation_size
```
</div>

## Running Multiple Nodes on One Host

If you built Riak from source code, or if you are using the Mac OS X
pre-built package, you can easily run multiple Riak nodes on the same
machine. The most common scenario for doing this is to experiment
with running a Riak cluster.

**Note**: If you have installed the `.deb` or `.rpm` package, you will
need to download and build Riak from source to follow the directions
below.

To run multiple nodes, make copies of the `riak` directory.

-   If you ran `make all rel`, then this can be found in `./rel/riak`
    under the Riak source root directory.
-   If you are running Mac OS X, then this is the directory where you
    unzipped the `.tar.gz` file.

Presuming that you copied `./rel/riak` into `./rel/riak1`,
`./rel/riak2`, `./rel/riak3`, and so on:

* In the `app.config` file for each node, change `handoff_port`,
  `pb_port`, and the port number specified in the `http{}` section to
  unique ports for each node.
* In `vm.args`, change the line that says `-name riak@127.0.0.1` to a
  unique name for each node, for example: `-name riak1@127.0.0.1`.

Now, start the nodes, changing path names and nodes as appropriate:

```bash
./rel/riak1/bin/riak start
./rel/riak2/bin/riak start
./rel/riak3/bin/riak start
# etc.
```

Next, join the nodes into a cluster:

```bash
./rel/riak2/bin/riak-admin cluster join riak1@127.0.0.1
./rel/riak3/bin/riak-admin cluster join riak1@127.0.0.1
./rel/riak2/bin/riak-admin cluster plan
./rel/riak2/bin/riak-admin cluster commit
```

## Multiple Clusters on One Host

Using the above technique, it is possible to run multiple clusters on
one computer. If a node hasnt joined an existing cluster it will behave
just as a cluster would. Running multiple clusters on one computer is
simply a matter of having two or more distinct nodes or groups of
clustered nodes.
