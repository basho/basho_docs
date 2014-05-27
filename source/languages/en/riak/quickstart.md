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

In this tutorial, we'll install Riak and build a [five-node](http://basho.com/why-your-riak-cluster-should-have-at-least-five-nodes/) Riak cluster running on your local machine.

## Install Riak

Basho's pre-packaged Riak binaries (found under [[Downloads]]) embed the Erlang runtime. This tutorial, however, is based on a source build, so you will need to [[install Erlang|Installing Erlang]] if it is not installed on your machine already.

If you wish to build Riak 2.0 from source, we strongly recommend using Basho's patched version of Erlang. The tar file for this version can be downloaded [here](http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho4.tar.gz).

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho4.tar.gz
tar -xvf otp_src_R16B02-basho4.tar.gz
cd otp_src_R16B02-basho4
./configure
make
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

You should now have a copy of Riak installed locally. It's time to build it. Do this by accessing the `riak` directory from your install and running `make all`:

```bash
cd riak-{{VERSION}}
make all
```

`make all` grabs all of Riak's dependencies for you so that you don't have to chase them down on your own. This process will likely take a few moments.

## Start Up Five Nodes

Now that Riak is built, use [Rebar](https://github.com/basho/rebar), a packaging and build system for Erlang applications, to get five self-contained Riak nodes running on your machine. When you put Riak into production, Rebar will enable you to ship a pre-built Riak package to your deployment machines. But for now, we'll just stick to the five nodes. You can set the number of nodes you wish to create via `DEVNODES`. To start up five nodes:

```bash
make devrel DEVNODES=5
```

You have just generated a `dev` directory. Let's go into that directory to check out its contents:

```bash
cd dev; ls
```

You will see that five directories beginning with `dev` have been created:

```bash
dev1       dev2       dev3       dev4       dev5
```

Each of these directories is a complete, self-contained package containing a Riak node. We need to start each node individually. Let's start with `dev1`:

```bash
dev1/bin/riak start
```

<div class="note">
<div class="title"><tt>ulimit</tt> warning</div>
At this point, you may receive a warning message to increase the number of open file handles <tt>ulimit</tt> in your operating system. See [[Open Files Limit]] for platform-specific instructions on doing this.
</div>

Once you've started the node in `dev1`, do the same for `dev2` through `dev5`:

```bash
dev2/bin/riak start
dev3/bin/riak start
dev4/bin/riak start
dev5/bin/riak start
```

Of if you prefer more succinct commands, you can use a `for` loop to iterate through and start the available nodes:

```bash
for node in `ls`; do $node/bin/riak start; done
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

If the response is `pong`, then the node is up and running. Otherwise, the node is currently stopped and will return something like the following:

```bash
Node 'dev1@127.0.0.1' not responding to pings.
```

Alternatively, you can run a command to iterate through each node and return its current status:

```bash
for node in `ls`; do $node/bin/riak ping; done
```

## Create the Cluster

You now have five nodes up and running, they are not yet connected to one another, i.e. they do not yet form a Riak [[cluster|Clusters]]. And so the next step is to join the nodes together into a cohesive unity. You can do this using the `[[riak-admin|riak-admin Command Line]]` command interface. The `riak-admin` script, like the `riak` script used above, is found in the `bin` directory of each Riak node.

This command will link `dev1` to `dev2`, `dev3`, `dev4`, and `dev5`:

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

Notice that you don't need to connect every node to every single other node. Once two nodes have been joined, they will share all the information necessary to join _all_ of the nodes into a unity. Thus, if `dev1` is joined to `dev2` and also to `dev5`, `dev2` and `dev5` will be able to communicate with one another.

At this point, the nodes have not yet been joined. Instead, the join operations have been _staged_ and are ready to be committed. To make those joins take effect, you first must review the planned cluster changes:

```bash
dev1/bin/riak-admin cluster plan
```

**Note**: The plan for the entire cluster can be reviewed on *any* node in the cluster.

The `plan` command will print out a synopsis of what changes will be made to the cluster on commit and how the cluster will look after the changes are complete.

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

Finally, you can commit the join commands that you staged and then reviewed:

```bash
dev2/bin/riak-admin cluster commit
```

**Note**: Changes to a cluster can be committed from any node.

<div class="info">
<div class="title">About <tt>riak-admin</tt></div>
<tt>riak-admin</tt> is Riak's administrative tool. It's used to perform any operational tasks beyond starting and stopping a node (e.g. to join and leave a cluster), to back up data, and to manage general cluster operations. You can read more about the <tt>riak-admin</tt> command [[here|riak-admin Command Line]].
</div>

## Test the Cluster

Now we now a have a running five-node Riak cluster. Let's make sure it's working properly. For this we have a couple of options, the simplest of which is to run the `member-status` command on one of our nodes:

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

If you wish, you can add a file to your Riak cluster and test whether or not it's working properly. Let's store a simple object in Riak (a text snippet) and then fetch it. The easiest way to get started is Riak's [[HTTP API]].

We'll use [curl](http://httpkit.com/resources/HTTP-from-the-Command-Line/) to make a `PUT` request to the [[key|Keys and Objects#keys]] `german` in the [[bucket|Buckets]] `welcome`.

```curl
curl -XPUT http://localhost:8098/buckets/welcome/keys/german \
  -H 'Content-Type: text/plain' \
  -d 'herzlich willkommen'
```

Your HTTP port might differ, so check your [[configuration file|Configuration Files]] for the valid port in your cluster (in `/etc/app.config` if you're using the older configuration system, in `/etc/riak.conf` if you're using the newer system).

<div class="note">
<div class="title">Note on the HTTP API</div>
While the HTTP API can be useful for getting started or for running basic test operations, we strongly recommend using [[client libraries]] that utilize Riak's [[PBC API]] when building applications.
</div>

If the `PUT` request above succeeded, that means that you've stored your first object in Riak. Now attempt a `GET` request to the same HTTP endpoint:

```curl
curl http://localhost:8098/buckets/hello/keys/german
```

You should get the following result:

```
herzlich willkommen
```

Now try something a little bit more involved. Copy a [JPEG](http://en.wikipedia.org/wiki/JPEG) image file from somewhere on your hard drive into the root directory of your cluster:

```bash
cp ~/image/location/<image_name>.jpg .
```

Use curl to `PUT` the image into Riak:

```curl
curl -XPUT http://localhost:8098/buckets/images/keys/<image_name>.jpg \
  -H 'Content-Type: image/jpeg' \
  --data-binary @<image_name>.jpg
```

You can verify that the image has been propery stored by navigating to the URL above in a browser or issuing a `GET` request:

```curl
curl http://localhost:8098/buckets/images/keys/<image_name>.jpg
```

If the response is an indecipherable binary, then your read request has succeeded.

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

In order to set up a client, we must first create a `RiakNode` object for each node in your cluster and add each node to a single `RiakCluster` object. Let's say that your cluster consists of three nodes, each with a [[Protocol Buffers|PBC API]] port of 8087 and IPs of 101.0.0.1, 101.0.0.2, and 101.0.0.3, respectively. First, we need to create node objects for each:

```java
Integer port = 8087;
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

Now we need to add each of these nodes to a `RiakCluster` object that defines the cluster as a whole. We initialize the cluster with one node and then add the others:

```java
RiakCluster cluster = new RiakCluster.Builder(node1).build();
cluster.add(node2).add(node3);
```

All that remains is to create a `RiakClient` object that refers to the `cluster` object from above:

```java
RiakClient client = new RiakClient(cluster);
```

Before you can use the `client` object to make calls to Riak, however, you must start your cluster:

```java
cluster.start();

// If your cluster isn't started, you'll get this error:
Exception in thread "main" java.lang.IllegalStateException: required: [RUNNING] current: CREATED
```

When your cluster is started, can use this `client` object (or whatever you wish to name your client) to synchronously or asynchronously execute all calls to Riak:

```java
client.execute(syncRiakOperation); // synchronous
client.executeAsync(asyncRiakOperation); // asynchronous
```

For some Java code samples to get you started, see our tutorials on [[the basics of Riak|The Basics]], [[Riak Data Types|Using Data Types]], and [[Riak Search 2.0|Using Search]], as well as a variety of other pages in the **Riak for Developers** section of the documentation (in the navbar on the left).

#### Ruby

How you connect to Riak with the Ruby client depends on whether you're using Riak in a development environment with a one-node [[cluster|Clusters]] or if you're using multiple nodes, as you would in any production environment.

If you're developing using a single-node cluster, you can create a `client` object and specify the host and [[Protocol Buffers|PBC API]] port. The below example connects the Ruby client to a one-node cluster running on the host 101.0.0.1 and the port 8087:

```ruby
require 'riak'

client = Riak::Client.new(host: '101.0.0.1', pb_port: 8087)
```

If connecting to multiple nodes, you can specify the connection information for those nodes when you instantiate the `client` object (or whatever you wish to call this object). Let's say that your cluster consists of three nodes, each with a Protocol Buffers port of 8087 and IPs of 101.0.0.1, 101.0.0.2, and 101.0.0.3, respectively. We can specify this information in the hash that we pass to the client:

```ruby
port = 8087

client = Riak::Client.new(nodes: [
  { host: '101.0.0.1', pb_port: port },
  { host: '101.0.0.2', pb_port: port },
  { host: '101.0.0.3', pb_port: port }
])
```

For some Ruby code samples to get you started, see our tutorials on [[the basics of Riak|The Basics]], [[Riak Data Types|Using Data Types]], [[data modeling with Riak Data Types]], and [[Riak Search 2.0|Using Search]], as well as a variety of other pages in the **Riak for Developers** section of the documentation (in the navbar on the left).

#### Python

How you connect to Riak with the Python client depends on whether you're using Riak in a development environment with a one-node [[cluster|Clusters]] or if you're using multiple nodes, as you would in any production environment.

If you're developing using a single-node cluster, you can create a `client` object and specify the host and [[Protocol Buffers|PBC API]] port. The below example connects the Ruby client to a one-node cluster running on the host 101.0.0.1 and the port 8087:

```python
from riak import RiakClient

client = RiakClient(host='101.0.0.1', pb_port=8087)
```

If connecting to multiple nodes, you can specify the connection information for those nodes when you instantiate the `client` object (or whatever you wish to call this object). Let's say that your cluster consists of three nodes, each with a Protocol Buffers port of 8087 and IPs of 101.0.0.1, 101.0.0.2, and 101.0.0.3, respectively. We can specify this information in the hash that we pass to the client:

```python
port = 8087

client = Riak::Client.new(nodes=[
  { 'host': '127.0.0.1', 'pb_port': port },
  { 'host': '127.0.0.2', 'pb_port': port },
  { 'host': '127.0.0.3', 'pb_port': port }
])
```

For some Python code samples to get you started, see our tutorials on [[the basics of Riak|The Basics]], [[Riak Data Types|Using Data Types]], and [[Riak Search 2.0|Using Search]], as well as a variety of other pages in the **Riak for Developers** section of the documentation (in the navbar on the left).

#### Erlang

How you connect to Riak with the Erlang client depends on whether you're using Riak in a development environment with a one-node [[cluster|Clusters]] or if you're using multiple nodes, as you would in any production environment.

If you're developing using a single-node cluster, you can specify a single process identifier (i.e. [pid](http://www.erlang.org/doc/reference_manual/data_types.html#id66818)) to which your client will connect on the basis of the host and Protocol Buffers port you provide. The example below connects the Erlang client to a one-node cluster running on the host 101.0.0.1 and the port 8087:

```erlang
{ok, Pid} = riakc_pb_socket:start_link("101.0.0.1", 8087).
```

If connecting to multiple nodes, you can specify the connection information for those nodes and produce multiple process identifiers. Let's say that your cluster consists of three nodes, each with a [[Protocol Buffers|PBC API]] port of 8087 and IPs of 101.0.0.1, 101.0.0.2, and 101.0.0.3, respectively. The following would produce separate process identifiers for each:

```erlang
{ok, Pid1} = riakc_pb_socket:start_link("101.0.0.1", 8087),
{ok, Pid2} = riakc_pb_socket:start_link("101.0.0.2", 8087),
{ok, Pid3} = riakc_pb_socket:start_link("101.0.0.3", 8087).
```

For some Erlang code samples to get you started, see our tutorials on [[the basics of Riak|The Basics]], [[Riak Data Types|Using Data Types]], and [[Riak Search 2.0|Using Search]], as well as a variety of other pages in the **Riak for Developers** section of the documentation (in the navbar on the left).

