---
title: Five-Minute Install
project: riak
version: 1.1.0+
document: tutorials
toc: true
audience: beginner
keywords: [developers, 2i]
moved: {
  '1.4.0-': '/tutorials/fast-track/Building-a-Development-Environment'
}
---

Let's install Riak and build a [five-node](http://basho.com/why-your-riak-cluster-should-have-at-least-five-nodes/) cluster running on your local machine.

## Install Riak

Basho's pre-packaged Riak binaries (found under [[Downloads]]) embed the Erlang runtime. However, this tutorial is based on a source build, so if you do not have Erlang already installed, first [[install Erlang|Installing Erlang]].

We strongly recommend using Basho's patched version of Erlang to install Riak 2.0 if you wish to build from source. The tar file for this version can be downloaded [here](http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho4.tar.gz).

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho4.tar.gz
tar -xvf otp_src_R16B02-basho4.tar.gz
cd otp_src_R16B02-basho4
```

### Get the Source

The following links provide platform-specific instructions for downloading and installing Riak from source.

  * [[Debian and Ubuntu|Installing on Debian and Ubuntu#Installing-From-Source]]
  * [[RHEL and CentOS|Installing on RHEL and CentOS#Installing-From-Source]]
  * [[Mac OS X|Installing on Mac OS X#Installing-From-Source]]
  * [[FreeBSD|Installing on FreeBSD#Installing-From-Source]]
  * [[SUSE|Installing on SUSE]]
  * [[Windows Azure|Installing on Windows Azure]]
  * [[AWS Marketplace|Installing on AWS Marketplace]]
  * [[Unlisted Operating System|Installing Riak from Source]]

### Build Riak

So now you have a copy of Riak installed locally. It's time to build it. Do this by accessing the *riak* directory and running `make all`

```bash
cd riak-{{VERSION}}
make all
```

`make all` grabs all of Riak's dependencies for you so that you don't have to chase them down on your own. This process will likely take a few moments.

## Start Up Five Nodes

Now that Riak is built, we are going to use [Rebar](https://github.com/basho/rebar), a packaging and build system for Erlang applications, to get five self-contained Riak nodes running on your machine. When you put Riak into production, Rebar will enable you to ship a pre-built Riak package to your deployment machines. But for now, we'll just stick to the five nodes. You can set the number of nodes you wish to create via `DEVNODES` To start these up, run:

```bash
make devrel DEVNODES=5
```

You have just generated a `dev` directory. Let's go into that directory to check out its contents:

```bash
cd dev; ls
```

That should give you the following:

```bash
dev1       dev2       dev3       dev4       dev5
```

Each directory starting with `dev` is a complete, self-contained package containing a Riak node. We need to start each node individually. Let's start with `dev1`:

```bash
dev1/bin/riak start
```

<div class="note">
<div class="title"><tt>ulimit</tt> warning</div>At this point you may receive a warning message to increase the number of open file handles (<tt>ulimit</tt>).  See [[Open Files Limit]] for platform-specific instructions on doing this.
</div>

Then do the same for `dev2` through `dev5`

```bash
dev2/bin/riak start
dev3/bin/riak start
dev4/bin/riak start
dev5/bin/riak start
```

Of if you prefer more succinct commands, you can use a `for` loop to iterate through and start the available nodes:

```bash
$ for node in `ls`; do $node/bin/riak start; done
```

### Check Running Nodes

After you have the nodes up and running, it's time to test them and make sure they are available. You can do this by taking a quick look at your process list. To do this, run:

```bash
ps aux | grep beam
```

This should give you granular details on the five running Riak nodes. If you'd like to simply check which nodes are running and which are not, you can run the `riak ping` command on a specific node:

```bash
dev1/bin/riak ping
```

If the response is `PONG`, then the node is up and running. Otherwise, the node is currently stopped and will return something like the following:

```bash
Node 'dev1@127.0.0.1' not responding to pings.
```

Alternatively, you can run a command like the following to iterate through each node and return its current status:

```bash
for node in `ls`; do $node/bin/riak ping; done
```

## Create the Cluster

The next step is to join these five nodes together to form a *cluster*. You can do this using the Riak Admin tool. Specifically, what we want to do is join `dev2`, `dev3`, `dev4`, and `dev5` to `dev1`:

```bash
dev2/bin/riak-admin cluster join dev1@127.0.0.1
dev3/bin/riak-admin cluster join dev1@127.0.0.1
dev4/bin/riak-admin cluster join dev1@127.0.0.1
dev5/bin/riak-admin cluster join dev1@127.0.0.1
```

Or alternatively:

```bash
for n in {2..5}; do dev$n/bin/riak-admin cluster join dev1@127.0.0.1; done
```

To make the above joins take effect, you first must review the `plan`:

```bash
dev1/bin/riak-admin cluster plan
```

**Note**: The plan for the entire cluster can be reviewed on *any* node in the cluster.

The plan will print out a synopsis of what it plans to do, and how the cluster will look after this is completed.

```bash
=============================== Staged Changes ================================
Action         Nodes(s)
-------------------------------------------------------------------------------
join           'dev2@127.0.0.1'
join           'dev3@127.0.0.1'
join           'dev4@127.0.0.1'
join           'dev5@127.0.0.1'
-------------------------------------------------------------------------------


NOTE: Applying these changes will result in 1 cluster transition

###############################################################################
                         After cluster transition 1/1
###############################################################################

================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
valid     100.0%     20.3%    'dev1@127.0.0.1'
valid       0.0%     20.3%    'dev2@127.0.0.1'
valid       0.0%     20.3%    'dev3@127.0.0.1'
valid       0.0%     20.3%    'dev4@127.0.0.1'
valid       0.0%     18.8%    'dev5@127.0.0.1'
-------------------------------------------------------------------------------
Valid:5 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

Transfers resulting from cluster changes: 51
  12 transfers from 'dev1@127.0.0.1' to 'dev5@127.0.0.1'
  13 transfers from 'dev1@127.0.0.1' to 'dev4@127.0.0.1'
  13 transfers from 'dev1@127.0.0.1' to 'dev3@127.0.0.1'
  13 transfers from 'dev1@127.0.0.1' to 'dev2@127.0.0.1'
```

Finally, you can commit the batch:

```bash
dev2/bin/riak-admin cluster commit
```

**Note**: Changes to a cluster can be committed from any node.

<div class="info">
<div class="title">About riak-admin</div>
<tt>riak-admin</tt> is Riak's administrative tool. It's used to perform any operational tasks other than starting and stopping a node (e.g. to join and leave a cluster), to back up data, and to manage general cluster operations. You can read more about the <tt>riak-admin</tt> command [[here|riak-admin Command Line]].
</div>

## Test the Cluster

Now we now a have a running five-node Riak cluster. Let's make sure it's working properly. For this we have a couple of options. A simple option is to run the `member-status` command on one of our nodes:

```bash
dev1/bin/riak-admin member-status
```

This will give us a high-level view of our cluster and tell us the percentage of the ring that each node manages:

```bash
================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
valid      20.3%      --      'dev1@127.0.0.1'
valid      20.3%      --      'dev2@127.0.0.1'
valid      20.3%      --      'dev3@127.0.0.1'
valid      20.3%      --      'dev4@127.0.0.1'
valid      18.8%      --      'dev5@127.0.0.1'
-------------------------------------------------------------------------------
Valid:5 / Leaving:0 / Exiting:0 / Joining:0 / Down:0
```

If you wish, you can add a file to your Riak cluster and test whether or not it's working properly. Let's say that we want to store an image in Riak and to make sure it's accessible. First, copy an image to your directory if you don't already have one:

```bash
cp ~/image/location/image_name.jpg .
```

We can then `PUT` that image into Riak using a `curl` command. Your HTTP port might differ, so check your [[configuration file|Configuration Files]] for the valid port for your cluster (in `app.config` if you're using the older configuration system, in `riak.conf` if you're using the newer system). Let's say that the HTTP port for your cluster is set to 10018. The following `curl` command would store an image named `1.jpg` in Riak, in the bucket `images`:

```curl
curl -XPUT \
  -H "Content-type: image/jpeg" \
  --data-binary @image_name.jpg \
  http://127.0.0.1:10018/buckets/images/keys/1.jpg
```

You can then verify that image was in fact stored. To do this, simply copy the URL where we `PUT` the image and paste it into a browser. Your image should appear. Alternatively, you can use `curl` to 

You should now have a five-node Riak cluster up and running. Congratulations!

<div class="note">
<div class="title">HTTP interface ports</div>
The above configuration sets up nodes with HTTP interfaces listening on ports `10018`, `10028`, `10038` and `10048` for `dev1`, `dev2`, `dev3`, `dev4`, and `dev5` respectively. The default port for single nodes to listen on is `8098`, and users will need to take note of this when trying to use any of the default settings for Riak client libraries.
</div>

## Setting Up Your Riak Client

Basho maintains official [[client libraries]] for Java, Ruby, Python, and Erlang. Below are links to client-specific documentation for each language:

[Java](http://basho.github.io/riak-java-client/2.0.0-SNAPSHOT/)
[Ruby](https://github.com/basho/riak-ruby-client)
[Python](http://basho.github.io/riak-python-client/)
[Erlang](http://basho.github.io/riak-erlang-client/)

In each of the above docs, you'll find detailed client installation and setup instructions. Here, we'll walk you through the basics of establishing a client connection to Riak in each of the four official clients. In each case, it will be assumed that the client has been successfully installed and that your application has been configured to use it.

#### Java

In the new 2.0 version of the Java client, Riak is accessed at the [[cluster|Clusters]] level rather than at the basic client level, as in previous versions of the client. This enables you to provide host and port information for all nodes of your cluster.

In order to set up a client, we must first create a `RiakNode` object for each node in your cluster and add each node to a single `RiakCluster` object. Let's say that your cluster consists of three nodes, each with a [[Protocol Buffers|PBC API]] port of 10017 and IPs of 101.0.0.1, 101.0.0.2, and 101.0.0.3, respectively. First, we need to create node objects for each:

```java
Integer port = 10017;
RiakNode node1 = RiakNode.Builder()
        .withRemotePort(port)
        .withRemoteAddress("101.0.0.1")
        .build();
RiakNode node2 = RiakNode.Builder()
        .withRemotePort(port)
        .withRemoteAddress("101.0.0.2")
        .build();
RiakNode node3 = RiakNode.Builder()
        .withRemotePort(port)
        .withRemoteAddress("101.0.0.3")
        .build();
```

Now, we need to add each of these nodes to a `RiakCluster` object that defines the cluster as a whole. We initialize the cluster with one node and then add the others:

```java
RiakCluster cluster = new RiakCluster.Builder(node1).build();
cluster.add(node2).add(node3);
```

Now, all that remains is to create a `RiakClient` object that refers to the `cluster` object from above:

```java
RiakClient client = new RiakClient(cluster);
```

Now, you can use this `client` object (or whatever you wish to name your client) to synchronously or asynchronously execute all calls to Riak:

```java
client.execute(syncRiakOperation); // synchronous
client.execute(asyncRiakOperation); // asynchronous
```

For some Java code samples to get you started, see our tutorials on [[the basics of Riak|The Basics]], [[Riak Data Types|Using Data Types]], and [[Riak Search 2.0|Using Search]], as well as a variety of other pages in the **Riak for Developers** section of the documentation (in the navbar on the left).

#### Ruby

How you connect to Riak with the Ruby client depends on whether you're using Riak in a development environment with a one-node [[cluster|Clusters]] or if you're using multiple nodes, as you would in any production environment.

If you're developing using a single-node cluster, you can create a `client` object and specify the host and [[Protocol Buffers|PBC API]] port. The below example connects the Ruby client to a one-node cluster running on the host 101.0.0.1 and the port 10017:

```ruby
require 'riak'

client = Riak::Client.new(host: '101.0.0.1', pb_port: 10017)
```

If connecting to multiple nodes, you can specify the connection information for those nodes when you instantiate the `client` object (or whatever you wish to call this object). Let's say that your cluster consists of three nodes, each with a Protocol Buffers port of 10017 and IPs of 101.0.0.1, 101.0.0.2, and 101.0.0.3, respectively. We can specify this information in the hash that we pass to the client:

```ruby
port = 10017

client = Riak::Client.new(nodes: [
  { host: '101.0.0.1', pb_port: port },
  { host: '101.0.0.2', pb_port: port },
  { host: '101.0.0.3', pb_port: port }
])
```

For some Ruby code samples to get you started, see our tutorials on [[the basics of Riak|The Basics]], [[Riak Data Types|Using Data Types]], [[data modeling with Riak Data Types]], and [[Riak Search 2.0|Using Search]], as well as a variety of other pages in the **Riak for Developers** section of the documentation (in the navbar on the left).

#### Python

How you connect to Riak with the Python client depends on whether you're using Riak in a development environment with a one-node [[cluster|Clusters]] or if you're using multiple nodes, as you would in any production environment.

If you're developing using a single-node cluster, you can create a `client` object and specify the host and [[Protocol Buffers|PBC API]] port. The below example connects the Ruby client to a one-node cluster running on the host 101.0.0.1 and the port 10017:

```python
from riak import RiakClient

client = RiakClient(host='101.0.0.1', pb_port=10017)
```

If connecting to multiple nodes, you can specify the connection information for those nodes when you instantiate the `client` object (or whatever you wish to call this object). Let's say that your cluster consists of three nodes, each with a Protocol Buffers port of 10017 and IPs of 101.0.0.1, 101.0.0.2, and 101.0.0.3, respectively. We can specify this information in the hash that we pass to the client:

```python
port = 10017

client = Riak::Client.new(nodes=[
  { 'host': '127.0.0.1', 'pb_port': port },
  { 'host': '127.0.0.2', 'pb_port': port },
  { 'host': '127.0.0.3', 'pb_port': port }
])
```

For some Python code samples to get you started, see our tutorials on [[the basics of Riak|The Basics]], [[Riak Data Types|Using Data Types]], and [[Riak Search 2.0|Using Search]], as well as a variety of other pages in the **Riak for Developers** section of the documentation (in the navbar on the left).

#### Erlang

How you connect to Riak with the Erlang client depends on whether you're using Riak in a development environment with a one-node [[cluster|Clusters]] or if you're using multiple nodes, as you would in any production environment.

If you're developing using a single-node cluster, you can specify a single process identifier (i.e. [pid](http://www.erlang.org/doc/reference_manual/data_types.html#id66818)) to which your client will connect on the basis of the host and Protocol Buffers port you provide. The example below connects the Erlang client to a one-node cluster running on the host 101.0.0.1 and the port 10017:

```erlang
{ok, Pid} = riakc_pb_socket:start_link("101.0.0.1", 10017).
```

If connecting to multiple nodes, you can specify the connection information for those nodes and produce multiple process identifiers. Let's say that your cluster consists of three nodes, each with a [[Protocol Buffers|PBC API]] port of 10017 and IPs of 101.0.0.1, 101.0.0.2, and 101.0.0.3, respectively. The following would produce separate process identifiers for each:

```erlang
{ok, Pid1} = riakc_pb_socket:start_link("101.0.0.1", 10017),
{ok, Pid2} = riakc_pb_socket:start_link("101.0.0.2", 10017),
{ok, Pid3} = riakc_pb_socket:start_link("101.0.0.3", 10017).
```

For some Erlang code samples to get you started, see our tutorials on [[the basics of Riak|The Basics]], [[Riak Data Types|Using Data Types]], and [[Riak Search 2.0|Using Search]], as well as a variety of other pages in the **Riak for Developers** section of the documentation (in the navbar on the left).

