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

Configuring a Riak cluster involves instructing a node to listen on
a non-local interface (i.e., not `127.0.0.1`) and then joining nodes
together for cluster participation.

Most configuration changes will be applied to the configuration file located in your `rel/riak/etc/` directory (if you compiled from source) or `/etc/riak/` (if you used a binary install of Riak).

The commands below presume that you are running from a source install,
but if you have installed Riak with a binary install, you can substitute
the usage of `bin/riak` with `sudo /usr/sbin/riak` and `bin/riak-admin`
with `sudo /usr/sbin/riak-admin`. The `riak` and `riak-admin` executables are located in the `/bin` directory of your installation.

<div class="info">
<div class="title">Note on changing the <tt>-name</tt> value</div>
If possible, you should avoid starting Riak prior to editing the
<tt>-name</tt> parameter in <tt>vm.args</tt> as described below. If
you have already started Riak with the default settings, you cannot change
the <tt>-name</tt> setting and then successfully restart the
node.

If you cannot restart after changing the <tt>-name</tt> value you have two options:
<ol>
<li>Discard the existing ring metadata by removing the contents of
the <tt>ring</tt> directory. This will require rejoining all nodes into
a cluster again</li>
<li>Rename the node using the [[riak-admin cluster replace|riak-admin
Command Line#cluster-replace]] command. This will not work if you have
previously only started riak with a single node.</li>
</ol>
</div>

## Configure the First Node

First, stop your Riak node if it is currently running:

```bash
riak stop
```

#### Select an IP address and port

Let's say that the IP address for your cluster is 192.168.1.10 and that you'll be using the default port. If you're using the [[Protocol Buffers interface|PBC API]] to Riak (which we recommend over the HTTP interface due to performance gains), you should change your configuration file:

```riakconf
listener.protobuf.$name = 127.0.0.1:8087
```

```appconfig
%% In the pb section of riak_core:

{"127.0.0.1", 8087 },
```

becomes

```riakconf
listener.protobuf.$name = 192.168.1.10:8087
```

```appconfig
%% In the pb section of riak_core:

{"192.168.1.10", 8087 },
```

<div class="note">
<div class="title">Note</div>
If you are upgrading to Riak version 2.0 or later from an pre-2.0 release, you can use either your old <tt>app.config</tt>/<tt>vm.args</tt> configuration files or the newer <tt>riak.conf</tt> if you wish. If you have installed Riak 2.0 directly, you should use only <tt>riak.conf</tt>.

Below, examples will be provided for both the old and new configuration systems. Bear in mind that you need to use one or the other and never both.
 
More on configuring Riak can be found in the <a href="/latest/ops/advanced/configs/configuration-files">Configuration Files</a> doc.
</div>

If you're using the HTTP interface, you will need to alter your configuration in an analogous way:

```riakconf
listener.http.$name = 127.0.0.1:8098
```

```appconfig
%% In the riak_core section:

{http, [ {"127.0.0.1", 8098 } ]},
```

becomes

```riakconf
listener.http.$name = 192.168.1.10:8098
```

```appconfig
{http, [ {"192.168.1.10", 8098 } ]},
```

#### Name your node

Every node in Riak has a name associated with it. The default name is `riak@127.0.0.1`. Let's say that you want to change the name to `riak@192.168.1.10`:


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
Use fully qualified domain names (FQDNs) rather than IP addresses for the cluster member node names. For example, <tt>riak@cluster.example.com</tt> and <tt>riak@192.168.1.10</tt> are both acceptable node naming schemes, but using the FQDN style is preferred.

Once a node has been started, in order to change the name you must either
remove ring files from the data directory or [[riak-admin cluster force-replace|riak-admin Command Line#cluster-force-replace]] the node.
</div>

#### Start the node

Now that your node is properly configured, you can start it:

```bash
riak start
```

If the Riak node has been previously started, you must use `riak-admin cluster replace` to change the node name and update the node's ring file.

```bash
riak-admin cluster replace riak@127.0.0.1 riak@192.168.1.10
```

<div class="note">
<div class="title">Single Nodes</div>
If a node is started singly using default settings, as you might do when you are building your first test environment, you will need to remove the ring files from the data directory after you edit your configuration files. <tt>riak-admin cluster replace</tt> will not work as the node has not been joined to a cluster.
</div>

As with all cluster changes, you need to view the planned changes by running `riak-admin cluster plan` and then running `riak-admin cluster commit` to finalize those changes.

The node is now properly set up to join other nodes for cluster participation. You can proceed to adding a second node to the cluster.

## Add a Second Node to Your Cluster

Repeat the above steps for a second host on the same network, providing the second node with a host/port and node name. Once the second node has started, use `riak-admin cluster join` to join the second node to the first node, thereby creating an initial Riak cluster. Let's say that we've named our second node `riak@192.168.1.11`.

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

If your output was similar, then the second Riak node is now part of the cluster and has begun syncing with the first node. Riak provides several ways to determine the cluster's ring status. Here are two ways to examine your Riak cluster's ring:

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
You can also find more detailed instructions about [[Adding and Removing Nodes]] from a cluster.

<div class="note">
<div class="title">Ring Creation Size</div>
All nodes in the cluster must have the same initial ring size setting in order to join, and participate in cluster activity. This setting can be adjusted in your configuration file using the <tt>ring_creation_size</tt> parameter if you're using the older configuration system or <tt>ring_size</tt> in the new system.

Check the value of all nodes if you receive a message like this:

<tt>Failed: riak@10.0.1.156 has a different ring_creation_size</tt>
</div>

## Running Multiple Nodes on One Host

If you built Riak from source code, or if you are using the Mac OS X pre-built package, then you can easily run multiple Riak nodes on the same machine. The most common scenario for doing this is to experiment with running a Riak cluster.

**Note**: If you have installed the `.deb` or `.rpm` package, then you will need to download and build Riak source to follow the directions below.

To run multiple nodes, make copies of the `riak` directory.

-   If you ran `make all rel`, then this can be found in `./rel/riak`
    under the Riak source root directory.
-   If you are running Mac OS X, then this is the directory where you
    unzipped the `.tar.gz` file.

Presuming that you copied `./rel/riak` into `./rel/riak1`, `./rel/riak2`, `./rel/riak3`, and so on, you need to make two changes:

1. Set your handoff port and your Protocol Buffers or HTTP port (depending on which interface you are using) to different values on each node. For example:
    
    ```riakconf
    # For Protocol Buffers:
    listener.protobuf.$name = 127.0.0.1:8187

    # For HTTP:
    listener.http.$name = 127.0.0.1:8198

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

Using the above technique, it is possible to run multiple clusters on one computer. If a node hasn’t joined an existing cluster, it will behave just as a cluster would. Running multiple clusters on one computer is simply a matter of having two or more distinct nodes or groups of clustered nodes.
