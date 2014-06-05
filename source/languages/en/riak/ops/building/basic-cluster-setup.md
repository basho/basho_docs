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

Configuring a Riak cluster involves instructing each node to listen on a
non-local interface, i.e. not `127.0.0.1`, and then joining all of the
nodes together to participate in the cluster.

Most configuration changes will be applied to the [[configuration file|Configuration Files]]
located in your `rel/riak/etc` directory (if you compiled from source)
or `/etc` (if you used a binary install of Riak).

The commands below presume that you are running from a source install,
but if you have installed Riak with a binary install, you can substitute
the usage of `bin/riak` with `sudo /usr/sbin/riak` and `bin/riak-admin`
with `sudo /usr/sbin/riak-admin`. The `riak` and `riak-admin` scripts
are located in the `/bin` directory of your installation.

<div class="info">
<div class="title">Note on changing the <code>name</code> value</div>
If possible, you should avoid starting Riak prior to editing the name of
a node. This setting corresponds to the <code>nodename</code> parameter in
the <code>riak.conf</code> file if you are using the newer configuration
system, and to the <code>-name</code> parameter in <code>vm.args</code> (as
described below) if you are using the older configuration system. If you
have already started Riak with the default settings, you cannot change
the <code>-name</code> setting and then successfully restart the node.

If you cannot restart after changing the <code>-name</code> value you have
two options:
<ol>
<li>Discard the existing ring metadata by removing the contents of the
<code>ring</code> directory. This will require rejoining all nodes into a
cluster again.</li>
<li>Rename the node using the [[riak-admin cluster replace|riak-admin Command Line#cluster-replace]]
command. This will not work if you have previously only started Riak
with a single node.</li>
</ol>
</div>

## Configure the First Node

First, stop your Riak node if it is currently running:

```bash
riak stop
```

#### Select an IP address and port

Let's say that the IP address for your cluster is 192.168.1.10 and that
you'll be using the default port (8087). If you're using the [[Protocol Buffers interface|PBC API]]
to Riak (which we recommend over the HTTP interface due to performance
gains), you should change your configuration file:

```riakconf
listener.protobuf.internal = 127.0.0.1:8087
```

```appconfig
%% In the pb section of riak_core:

{"127.0.0.1", 8087 },
```

becomes

```riakconf
listener.protobuf.internal = 192.168.1.10:8087
```

```appconfig
%% In the pb section of riak_core:

{"192.168.1.10", 8087 },
```

<div class="note">
<div class="title">Note</div>
If you are upgrading to Riak version 2.0 or later from an pre-2.0
release, you can use either your old <code>app.config</code>/
<code>vm.args</code> configuration files or the newer <code>riak.conf</code>
if you wish. If you have installed Riak 2.0 directly, you should use
only <code>riak.conf</code>.

Below, examples will be provided for both the old and new configuration
systems. Bear in mind that you need to use either the older or the newer
but never both simultaneously.
 
More on configuring Riak can be found in the <a href="/latest/ops/advanced/configs/configuration-files">Configuration Files</a> documentation.
</div>

If you're using the HTTP interface, you will need to alter your
configuration in an analogous way:

```riakconf
listener.http.internal = 127.0.0.1:8098
```

```appconfig
%% In the riak_core section:

{http, [ {"127.0.0.1", 8098 } ]},
```

becomes

```riakconf
listener.http.internal = 192.168.1.10:8098
```

```appconfig
{http, [ {"192.168.1.10", 8098 } ]},
```

#### Name your node

Every node in Riak has a name associated with it. The default name is
`riak@127.0.0.1`. Let's say that you want to change the name to
`riak@192.168.1.10`:


```riakconf
nodename = riak@127.0.0.1
```

```vmargs
-name riak@127.0.0.1
```

becomes

```riakconf
nodename = riak@192.168.1.10
```

```vmargs
-name riak@192.168.1.10
```

<div class="note">
<div class="title">Node Names</div>
Use fully qualified domain names (<a href="http://en.wikipedia.org/wiki/Fully_qualified_domain_name">FQDN</a>s)
rather than IP addresses for the cluster member node names. For example,
<code>riak@cluster.example.com</code> and <code>riak@192.168.1.10</code>
are both acceptable node naming schemes, but using the FQDN style is
preferred.

Once a node has been started, in order to change the name you must either
remove ring files from the <code>/data/ring</code> directory or
<code>[[riak-admin cluster force-replace|riak-admin Command Line#cluster-force-replace]]</code>
the node.
</div>

#### Start the node

Now that your node is properly configured, you can start it:

```bash
riak start
```

If the Riak node has been previously started, you must use the `riak-admin cluster replace`
command to change the node name and update the node's ring file.

```bash
riak-admin cluster replace riak@127.0.0.1 riak@192.168.1.10
```

<div class="note">
<div class="title">Single Nodes</div>
If a node is started singly using default settings, as you might do when
you are building your first test environment, you will need to remove
the ring files from the data directory after you edit your configuration
files. <code>riak-admin cluster replace</code> will not work as the node
has not been joined to a cluster.
</div>

As with all cluster changes, you need to view the planned changes by
running `riak-admin cluster plan` and then running `riak-admin cluster commit`
to finalize those changes.

The node is now properly set up to join other nodes for cluster
participation. You can proceed to adding a second node to the cluster.

## Add a Second Node to Your Cluster

Repeat the above steps for a second host on the same network, providing
the second node with a host/port and node name. Once the second node has
started, use `riak-admin cluster join` to join the second node to the
first node, thereby creating an initial Riak cluster. Let's say that
we've named our second node `riak@192.168.1.11`. From the new node's
`/bin` directory:

```bash
riak-admin cluster join riak@192.168.1.10
```

Output from the above should resemble:

```
Success: staged join request for `riak@192.168.1.11` to `riak@192.168.1.10`
```

Next, plan and commit the changes:

```bash
riak-admin cluster plan
riak-admin cluster commit
```

After the last command, you should see:

```
Cluster changes committed
```

If your output was similar, then the second Riak node is now part of the
cluster and has begun syncing with the first node. Riak provides several
ways to determine the cluster's ring status. Here are two ways to
examine your Riak cluster's ring:

1. Using the `riak-admin` command:

    ```bash
    bin/riak-admin status | grep ring_members
    ```

    With output resembling the following:

    ```bash
    ring_members : ['riak@192.168.1.10','riak@192.168.1.11']
    ```

2. Running the `riak attach` command. This will open up an Erlang shell, into which you can type the following command:

    ```erlang
    1> {ok, R} = riak_core_ring_manager:get_my_ring().
    
    %% Response:
    
    {ok,{chstate,'riak@192.168.1.10',.........
    (riak@192.168.52.129)2> riak_core_ring:all_members(R).
    ['riak@192.168.1.10','riak@192.168.1.11']
    ```

To join additional nodes to your cluster, repeat the above steps.
You can also find more detailed instructions about [[Adding and Removing Nodes]]
from a cluster.

<div class="note">
<div class="title">Ring Creation Size</div>
All nodes in the cluster must have the same initial ring size setting in
order to join, and participate in cluster activity. This setting can be
adjusted in your configuration file using the <code>ring_creation_size</code>
parameter if you're using the older configuration system or <code>ring_size</code>
in the new system.

Check the value of all nodes if you receive a message like this:

<code>Failed: riak@10.0.1.156 has a different ring_creation_size</code>
</div>

## Running Multiple Nodes on One Host

If you built Riak from source code, or if you are using the Mac OS X
pre-built package, then you can easily run multiple Riak nodes on the
same machine. The most common scenario for doing this is to experiment
with running a Riak cluster.

**Note**: If you have installed the `.deb` or `.rpm` package, then you
will need to download and build Riak source to follow the directions
below.

To run multiple nodes, make copies of the `riak` directory.

-   If you ran `make all rel`, then this can be found in `./rel/riak`
    under the Riak source root directory.
-   If you are running Mac OS X, then this is the directory where you
    unzipped the `.tar.gz` file.

Presuming that you copied `./rel/riak` into `./rel/riak1`, `./rel/riak2`,
`./rel/riak3`, and so on, you need to make two changes:

1. Set your handoff port and your Protocol Buffers or HTTP port (depending on which interface you are using) to different values on each node. For example:
    
    ```riakconf
    # For Protocol Buffers:
    listener.protobuf.internal = 127.0.0.1:8187

    # For HTTP:
    listener.http.internal = 127.0.0.1:8198

    # For either interface:
    handoff.port = 8199
    ```

    ```appconfig
    %% In the pb section of riak_core:
    {"127.0.0.1", 8187 }

    %% In the http section of riak_core:
    {"127.0.0.1", 8198}
    ```

2. Change the name of each node to a unique name.

Now, start the nodes, changing path names and nodes as appropriate:

```bash
./rel/riak1/bin/riak start
./rel/riak2/bin/riak start
./rel/riak3/bin/riak start

# etc
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
one computer. If a node hasn’t joined an existing cluster, it will
behave just as a cluster would. Running multiple clusters on one
computer is simply a matter of having two or more distinct nodes or
groups of clustered nodes.
