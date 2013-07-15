---
title: Basic Cluster Setup
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: beginner
keywords: [operator, cluster]
---

Configuration of a Riak cluster requires instructing a node to listen on
a non-local interface (i.e., not `127.0.0.1`), and then joining nodes
together for cluster participation.

Begin by editing the `app.config` file. The [[app.config|Configuration Files#app.config]]
file will be located in your `rel/riak/etc/` directory if you compiled
from source, and `/etc/riak/` if you used a binary install of Riak.

The commands below presume that you are running from a source install,
but if you have installed Riak with a binary install, you can substitute
the usage of `bin/riak` with `sudo /usr/sbin/riak` and `bin/riak-admin`
with `sudo /usr/sbin/riak-admin`.

<div class="info"><div class="title">Note on changing -name value</div>
<p>If possible, you should avoid starting Riak prior to editing the
<code>-name</code> parameter in <code>vm.args</code> as described below. If
you have already started Riak with the default settings, you cannot change
the <code>-name</code> setting and then successfully restart the
node.</p>
<<<<<<< HEAD
If you cannot restart after changing -name value you have two
=======
If you cannot restart after chaning -name value you have two
>>>>>>> 88da8bb125bed951cf3e22e36836d5c015abf41f
options:
<ol>
<li>Discard the existing ring metadata by removing the contents of
the <code>ring</code> directory. This will require rejoining all nodes into
a cluster again</li>
<li>Rename the node using the [[riak-admin cluster replace|Command-Line-Tools---riak-admin#cluster-replace]] command. This will not work if you have previously only started riak with a single node.</li>
</ol>
</div>

Configure the First Node
------------------------

First, stop your Riak node if it is currently running:

    bin/riak stop

Change the default IP address located under `http{}` in the riak_core
section of `app.config` . (The port, 8098, does not need to be changed.)
Let’s say our machine’s IP address is 192.168.1.10. (This is just an
example IP address. Yours will be specific to your machine.)

    {http, [ {"127.0.0.1", 8098 } ]},

becomes

    {http, [ {"192.168.1.10", 8098 } ]},

The same configuration should be changed for the Protocol Buffers interface if you intend on using it. Do the same as above for the line in the riak_kv section:

    {pb_ip,   "127.0.0.1" },

becomes

    {pb_ip,   "192.168.1.10" },


Next edit the `etc/vm.args` file and change the `-name` to your new IP:

    -name riak@127.0.0.1

becomes

    -name riak@192.168.1.10

<div class="info">
<strong>Node Names</strong>
<p>Use fully qualified domain names (FQDNs) rather than IP addresses
for the cluster member node names. For example, "riak@cluster.example.com" and "riak@192.168.1.10" are both acceptable node naming schemes, but using the FQDN style is preferred.</p>
<p>Once a node has been started, in order to change the name you must either remove ring files from the data directory, [[riak-admin reip|Command-Line-Tools---riak-admin]] the node, or [[riak-admin cluster force-replace|Command-Line-Tools---riak-admin]] the node.
</p>
<<<<<<< HEAD
=======
<p>
Although it is not recommended, if you have previously started a single node instance (as, for example,
you might do when you are building your first test environment), you
will need to remove the ring files from the data directory after you edit
<code>etc/vm.args</code>.
</p>
>>>>>>> 88da8bb125bed951cf3e22e36836d5c015abf41f
</div>

Start the Riak node:

    bin/riak start

If the Riak node has been previously started, you must use
`riak-admin cluster replace` to change the node name and
update the node's ring file.

    bin/riak-admin cluster replace riak@127.0.0.1 riak@192.168.1.10

<div class="info">
<strong>Single Nodes</strong>
If a node is started singly using default settings (as, for example,
you might do when you are building your first test environment), you
will need to remove the ring files from the data directory after you edit
<code>etc/vm.args</code>. <code>riak-admin cluster replace</code> will not work as the node
has not been joined to a cluster.
</div>

As with all cluster changes, you need to view the plan `riak-admin cluster plan`,
then run `riak-admin cluster commit` to finalize the changes.

The node is now properly configured to join other nodes for cluster
participation. Proceed to adding a second node to the cluster.

Add a Second Node to Your Cluster
---------------------------------

Repeat the above steps for a second host on the same network. Once the
second node has started, use `riak-admin cluster join` to join the second node
to the first node, thereby creating an initial Riak cluster.

    bin/riak-admin cluster join riak@192.168.1.10

Output from the above should resemble:

Success: staged join request for `riak@192.168.1.11` to `riak@192.168.1.10`

Next, plan and commit the changes:

    bin/riak-admin cluster plan
    bin/riak-admin cluster commit

After the last command, you should see:

    Cluster changes committed

If your output was similar, then the second Riak node is now part of the cluster and has begun syncing with the first node. Riak provides several ways to determine the cluster ring status; here are two ways to examine your Riak cluster's ring:

Using the `riak-admin` command:

    bin/riak-admin status | grep ring_members

With output resembling the following:

    ring_members : ['riak@192.168.1.10','riak@192.168.1.11']

Using the `riak attach` command:

    riak attach
    1> {ok, R} = riak_core_ring_manager:get_my_ring().
    {ok,{chstate,'riak@192.168.1.10',.........
    (riak@192.168.52.129)2> riak_core_ring:all_members(R).
    ['riak@192.168.1.10','riak@192.168.1.11']

To join additional nodes to your cluster, repeat the above steps.
You can also find more detailed instructions about [[Adding and Removing Nodes]]
from a cluster.

<div class="info"><strong>Ring Creation Size</strong>
<p>All nodes in the cluster must have the same initial <code>ring_creation_size</code> setting in order to join, and participate in cluster activity. This setting can be adjusted in app.config.</p>
<p>Check the value of all nodes if you receive a message like this:<br/><code>Failed: riak@10.0.1.156 has a different ring_creation_size</code></p></div>

Running Multiple Nodes on One Host
----------------------------------

If you built Riak from source code, or if you are using the Mac OS X
pre-built package, then you can easily run multiple Riak nodes on the
same machine. The most common scenario for doing this is to experiment
with running a Riak cluster. (Note: if you have installed the .deb or
.rpm package, then you will need to download and build Riak source to
follow the directions below.)

To run multiple nodes, make copies of the `riak` directory.

-   If you ran `make all rel`, then this can be found in `./rel/riak`
    under the Riak source root directory.
-   If you are running Mac OS X, then this is the directory where you
    unzipped the .tar.gz file.

Presuming that you copied `./rel/riak` into `./rel/riak1`,
`./rel/riak2`, `./rel/riak3`, and so on:

* In the `app.config` file for each node, change `handoff_port`, `pb_port`, and the port number specified in the `http{}` section to unique ports for each node.
* In `vm.args`, change the line that says `-name riak@127.0.0.1` to a unique name for each node, for example: `-name riak1@127.0.0.1`.

Now, start the nodes, changing path names and nodes as appropriate:

    ./rel/riak1/bin/riak start
    ./rel/riak2/bin/riak start
    ./rel/riak3/bin/riak start
    (etc.)

Next, join the nodes into a cluster:

    ./rel/riak2/bin/riak-admin cluster join riak1@127.0.0.1
    ./rel/riak3/bin/riak-admin cluster join riak1@127.0.0.1
    ./rel/riak2/bin/riak-admin cluster plan
    ./rel/riak2/bin/riak-admin cluster commit

Make a Developer Release
------------------------

Alternatively, you can run {{#1.3.0+}}`make devrel DEVNODES=N`{{/1.3.0+}}
{{#1.3.0-}}`make devrel`{{/1.3.0-}}, which will create
{{#1.3.0+}}N (defaults to four){{/1.3.0+}}{{#1.3.0-}}four{{/1.3.0-}} copies
of Riak under the directories `./dev/dev1`, `./dev/dev2`, `./dev/dev3`,
and `./dev/dev4` with the configuration pre-set to allow you to run the
nodes simultaneously. (Note: The Web port for the three nodes is 8091,
8092, 8093 and 8094, which is different from the default of 8098 that
you get when you run `make all rel`.)

Once you have built the four nodes using `make devrel` can start each of
them and join them together using the commands already covered above,
substituting directory names as appropriate.

Multiple Clusters on One Host
-----------------------------

Using the above technique it is possible to run multiple clusters on one
computer. If a node hasn’t joined an existing cluster it will behave
just as a cluster would. Running multiple clusters on one computer is
simply a matter of having two or more distinct nodes or groups of
clustered nodes.
