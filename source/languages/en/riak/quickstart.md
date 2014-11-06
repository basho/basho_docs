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

In this tutorial, we'll install Riak and build a
[five-node](http://basho.com/why-your-riak-cluster-should-have-at-least-five-nodes/)
Riak cluster running on your local machine.

## Install Riak

Basho's pre-packaged Riak binaries (found under [[Downloads]]) embed the
Erlang runtime. This tutorial, however, is based on a source build, so
you will need to [[install Erlang|Installing Erlang]] if it is not
installed on your machine already.

If you wish to build Riak 2.0 from source, we strongly recommend using
Basho's patched version of Erlang. The tar file for this version can be
downloaded
[here](http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho5.tar.gz).

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho5.tar.gz
tar -xvf otp_src_R16B02-basho5.tar.gz
cd otp_src_R16B02-basho5
```

Once you have unzipped the package, see our guide to [[installing
Erlang]] for installation instructions.

### Get the Source

The following links provide platform-specific instructions for
downloading and installing Riak from source.

  * [[Debian and Ubuntu|Installing on Debian and
    Ubuntu#Installing-From-Source]]
  * [[RHEL and CentOS|Installing on RHEL and
    CentOS#Installing-From-Source]]
  * [[Mac OS X|Installing on Mac OS X#Installing-From-Source]]
  * [[FreeBSD|Installing on FreeBSD#Installing-From-Source]]
  * [[SUSE|Installing on SUSE]]
  * [[Windows Azure|Installing on Windows Azure]]
  * [[AWS Marketplace|Installing on AWS Marketplace]]
  * [[Unlisted Operating System|Installing Riak from Source]]

### Build Riak

Now that you've downloaded and installed Riak using the instructions
above, it's time to build it. Access the `riak` directory from your
install and run `make all`:

```bash
cd riak-{{VERSION}}
make all
```

The `make all` command grabs all of Riak's dependencies for you so that
you don't have to chase them down on your own. This process will likely
take a few moments.

## Start Up Five Nodes

Now that Riak is built, use [Rebar](https://github.com/basho/rebar), a
packaging and build system for Erlang applications, to get five
self-contained Riak nodes running on your machine. When you put Riak
into production, Rebar will enable you to ship a pre-built Riak package
to your deployment machines. But for now, we'll just stick to the five
nodes. You can set the number of nodes you wish to create via
`DEVNODES`.

From the same directory in which you just ran `make all`, you can use
the following command to start up five nodes:

```bash
make devrel DEVNODES=5
```

You have just generated a `dev` directory. Let's go into that directory
to check out its contents:

```bash
cd dev; ls
```

You will see that five directories beginning with `dev` have been
created:

```bash
dev1       dev2       dev3       dev4       dev5
```

Each of these directories is a complete, self-contained package
containing a Riak node. We need to start each node individually using
the `start` command in the `bin` directory. Let's start with `dev1`:

```bash
dev1/bin/riak start
```

<div class="note">
<div class="title"><code>ulimit</code> warning</div>
At this point, you may receive a warning message to increase the number
of open file handles, i.e. <code>ulimit</code>, in your operating
system. See our [[Open Files Limit]] guide for platform-specific
instructions on doing so.  </div>

Once you've started the node in `dev1`, do the same for `dev2` through
`dev5`:

```bash
dev2/bin/riak start
dev3/bin/riak start
dev4/bin/riak start
dev5/bin/riak start
```

Of if you prefer more succinct commands, you can use a `for` loop to
iterate through and start the available nodes:

```bash
for node in dev*; do $node/bin/riak start; done
```

### Check Running Nodes

After you have the nodes up and running, it's time to test them and make
sure that they're available. You can do this by taking a quick look at
your process list. To do this, run:

```bash
ps aux | grep beam
```

This should give you granular details on the five running Riak nodes. If
you'd like to simply check which nodes are running and which are not,
you can run the `riak ping` command on a specific node:

```bash
dev1/bin/riak ping
```

If the response is `pong`, then the node is up and running. Otherwise,
the node is currently stopped and will return something like the
following:

```bash
Node 'dev1@127.0.0.1' not responding to pings.
```

Alternatively, you can run a command to iterate through each node and
return its current status:

```bash
for node in dev*; do $node/bin/riak ping; done
```

## Create the Cluster

Although you now have five nodes up and running, they are not yet
connected to one another, i.e. they do not yet form a Riak
[[cluster|Clusters]]. The next step is to join the nodes together into a
cohesive unity. You can do this using the `[[riak-admin|riak-admin
Command Line]]` command interface. The `riak-admin` script, like the
`riak` script used above, is found in the `bin` directory of each Riak
node.

First, try joining the node `dev2` to `dev1`:

```bash
dev2/bin/riak-admin cluster join dev1@127.0.0.1
```

If there is no response, then the join is successful. We still have
three running nodes that have not yet been joined, so let's join those
as well:

```bash
dev3/bin/riak-admin cluster join dev1@127.0.0.1
dev4/bin/riak-admin cluster join dev1@127.0.0.1
dev5/bin/riak-admin cluster join dev1@127.0.0.1
```

Or alternatively:

```bash
for n in {3..5}; do dev$n/bin/riak-admin cluster join dev1@127.0.0.1; done
```

Notice that you don't need to join every node to every single other
node. Once two nodes have been joined, they will share all the
information necessary to join _all_ of the nodes into a unity. Thus, if
`dev1` is joined to `dev2` and also to `dev5`, `dev2` and `dev5` will be
able to communicate with one another.

At this point, the nodes have not yet been actually joined. Instead, the
join operations have been _staged_ and are ready to be committed. To
make those joins take effect, you first must review the planned cluster
changes:

```bash
dev1/bin/riak-admin cluster plan
```

**Note**: The plan for the entire cluster can be reviewed on *any* node
in the cluster.

The `plan` command will print out a synopsis of what changes will be
made to the cluster on commit and how the cluster will look after the
changes are complete. The output should look like this:

```
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

Finally, you can commit the join commands that you staged and then
reviewed:

```bash
dev2/bin/riak-admin cluster commit
```

**Note**: Changes to a cluster can be committed from any node.

<div class="info">
<div class="title">About `riak-admin`</div>
`riak-admin` is Riak's administrative tool. It's used to perform any
operational tasks beyond starting and stopping a node (e.g.  to make a
node join and leave a cluster), to back up data, and to manage general
cluster operations. You can read more about the `riak-admin` command in
the [[riak-admin command line]] documentation.
</div>

## Test the Cluster

Now that we have a running five-node Riak cluster, let's make sure that
it's working properly. For this we have a couple of options. The
simplest is to run the `member-status` command on one of our nodes:

```bash
dev1/bin/riak-admin member-status
```

This will give us a high-level view of our cluster and tell us the
percentage of the data in the cluster that each node manages:

```
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

In order to test whether our cluster is working properly, let's store an
object (just a short text snippet) and then attempt to fetch it. The
easiest way to get started is by using Riak's [[HTTP API]]. We'll use
[curl](http://httpkit.com/resources/HTTP-from-the-Command-Line/) to make
a `PUT` request to the [[key|Keys and Objects#keys]] `german` in the
[[bucket|Buckets]] `welcome`.

```curl
curl -XPUT http://localhost:10018/buckets/welcome/keys/german \
  -H 'Content-Type: text/plain' \
  -d 'herzlich willkommen'
```

Your HTTP port might differ, so check your [[configuration files]] for
the valid port in your cluster. That information can be found in
`/etc/riak.conf` if you're using the newer configuration system or in
`/etc/app.config` if you're using the older system.

<div class="note">
<div class="title">Note on the HTTP API</div>
While the HTTP API can be useful for getting started or for running
basic test operations, we strongly recommend using [[client libraries]]
that utilize Riak's [[Protocol Buffers API|PBC API]] when building
applications.  </div>

If the `PUT` request above succeeded, that means that you've stored your
first object in Riak. Now attempt a `GET` request to the same HTTP
endpoint:

```curl
curl http://localhost:10018/buckets/welcome/keys/german
```

You should get the following result:

```
herzlich willkommen
```

Now try something a little bit more involved. Copy a
[JPEG](http://en.wikipedia.org/wiki/JPEG) image file from somewhere on
your hard drive into the root directory of your cluster:

```bash
cp ~/image/location/<image_name>.jpg .
```

Use curl to `PUT` the image into Riak:

```curl
curl -XPUT http://localhost:10018/buckets/images/keys/<image_name>.jpg \
  -H 'Content-Type: image/jpeg' \
  --data-binary @<image_name>.jpg
```

You can verify that the image has been properly stored by navigating to
the URL above in a browser or issuing a `GET` request:

```curl
curl -O http://localhost:10018/buckets/images/keys/<image_name>.jpg
```

This will save the image to the current directory. You can open it with
an image editor to verify that the image has been stored and retrieved
correctly.

Congratulations! You should now have a five-node Riak cluster up and
running.

<div class="note">
<div class="title">HTTP interface ports</div>
The above configuration sets up nodes with HTTP interfaces listening on
ports `10018`, `10028`, `10038`, `10048`, and `10058` for `dev1`,
`dev2`, `dev3`, `dev4`, and `dev5` respectively. The default port for
single nodes to listen on is `10018`, and users will need to take note
of this when trying to use any of the default settings for Riak client
libraries.  </div>

## Setting Up Your Riak Client

Basho maintains official [[client libraries]] for Java, Ruby, Python,
and Erlang. Below are links to client-specific documentation for each
language:

* [Java](http://basho.github.io/riak-java-client/2.0.0-SNAPSHOT/)
* [Ruby](https://github.com/basho/riak-ruby-client)
* [Python](http://basho.github.io/riak-python-client/)
* [Erlang](http://basho.github.io/riak-erlang-client/)

In each of the above docs, you'll find detailed client installation and
setup instructions. Here, we'll walk you through the basics of
establishing a client connection to Riak in each of the four official
clients. In each case, it will be assumed that the client has been
successfully installed and that your application has been configured to
use it.

### Java

In the new 2.0 version of the Java client, Riak is accessed at the
[[cluster|Clusters]] level rather than at the basic client level, as in
previous versions of the client. This enables you to provide host and
port information for all of the nodes in your cluster.

There are a variety of ways to set up cluster interaction with the Java
client. We'll start with the simplest way, which is to create a
connection to a single node, add that node to our cluster object, and
then create a client object (which must refer to a cluster). Let's
assume the node is listening on `localhost` and the [[Protocol
Buffers|PBC API]] port 10017.

```java
RiakNode node = new RiakNode.Builder()
        .withRemoteAddress("127.0.0.1")
        .withRemotePort(10017)
        .build();
RiakCluster cluster = new RiakCluster.Builder(node)
        .build();
RiakClient client = new RiakClient(cluster);
```

An important thing to always bear in mind is that you must start your
cluster object before it can be used.

```java
cluster.start();

// There is also a method to shut the cluster down:

cluster.shutdown();
```

If you do not start up your cluster object, you will see an error like
this:

```java
Exception in thread "main" java.lang.IllegalStateException: required: [RUNNING] current: CREATED
```

Now let's try to make a more complex cluster with three different nodes.
Each node will listen on `localhost` but they will listen on ports
10017, 10027, and 10037, respectively. Since they all share the same
host, we can create a node builder object and create new nodes using
that builder. This time, though, we'll add those nodes to a Java `List`
that we can pass to our cluster object:

```java
List<RiakNode> nodes = new LinkedList<RiakNode>();
RiakNode.Builder nodeBuilder = new RiakNode.Builder()
        .withRemoteAddress("127.0.0.1");

nodes.add(nodeBuilder.withRemotePort(10017).build());
nodes.add(nodeBuilder.withRemotePort(10027).build());
nodes.add(nodeBuilder.withRemotePort(10037).build());

RiakCluster cluster = new RiakCluster(nodes).build();
RiakClient client = new RiakClient(cluster)l
```

Remember once again that you must run the `start()` method on your
cluster object.

A third way to create a cluster object is by using a list of
remote addresses (as strings) and a node builder object to create a
list of nodes in one go. The following example would create three node
objects for nodes on the hosts `101.0.0.1`, `101.0.0.2`, and
`101.0.0.3`, respectively, all of which are listening on port 10017:

```java
List<String> addresses = new LinkedList<String>();
addresses.add("101.0.0.1");
addresses.add("101.0.0.2");
addresses.add("101.0.0.3");

RiakNode.Builder nodeBuilder = new RiakNode.Builder()
        .withRemotePort(10017);
List<RiakNode> nodes = RiakNode.Builder.buildNodes(nodeBuilder, addresses);
```

For some Java code samples to get you started, see our tutorials on
[[the basics of Riak|The Basics]], [[Riak Data Types|Using Data Types]],
and [[Riak Search 2.0|Using Search]], as well as a variety of other
pages in the **Riak for Developers** section of the documentation (in
the navbar on the left).

### Ruby

How you connect to Riak with the Ruby client depends on whether you're
using Riak in a development environment with a one-node
[[cluster|Clusters]] or if you're using multiple nodes, as you would in
any production environment.

If you're developing using a single-node cluster, you can create a
`client` object and specify the host and [[Protocol Buffers|PBC API]]
port. The example below connects the Ruby client to a one-node cluster
running on the host 101.0.0.1 and the port 8087:

```ruby
require 'riak'

client = Riak::Client.new(host: '101.0.0.1', pb_port: 8087)
```

If connecting to multiple nodes, you can specify the connection
information for those nodes when you instantiate the `client` object (or
whatever you wish to call this object). Let's say that your cluster
consists of three nodes, each with a Protocol Buffers port of 8087 and
IPs of 101.0.0.1, 101.0.0.2, and 101.0.0.3, respectively. We can specify
this information in the hash that we pass to the client:

```ruby
port = 8087

client = Riak::Client.new(nodes: [
  { host: '101.0.0.1', pb_port: port },
  { host: '101.0.0.2', pb_port: port },
  { host: '101.0.0.3', pb_port: port }
])
```

For some Ruby code samples to get you started, see our tutorials on
[[the basics of Riak|The Basics]], [[Riak Data Types|Using Data Types]],
[[data modeling with Riak Data Types]], and [[Riak Search 2.0|Using
Search]], as well as a variety of other pages in the **Riak for
Developers** section of the documentation (in the navbar on the left).

### Python

How you connect to Riak with the Python client depends on whether you're
using Riak in a development environment with a one-node
[[cluster|Clusters]] or if you're using multiple nodes, as you would in
any production environment.

If you're developing using a single-node cluster, you can create a
`client` object and specify the host and [[Protocol Buffers|PBC API]]
port. The example below connects the Python client to a one-node cluster
running on host 101.0.0.1 and port 8087:

```python
from riak import RiakClient

client = RiakClient(host='101.0.0.1', protocol='pbc', pb_port=8087)
```

If connecting to multiple nodes, you can specify the connection
information for those nodes when you instantiate the `client` object (or
whatever you wish to call this object). Let's say that your cluster
consists of three nodes, each with a Protocol Buffers port of 8087 and
IPs of 101.0.0.1, 101.0.0.2, and 101.0.0.3, respectively. We can specify
this information in the hash that we pass to the client:

```python
protocol = 'pbc'
port = 8087

client = Riak::Client.new(nodes=[
  { 'host': '127.0.0.1', 'protocol': protocol, 'pb_port': port },
  { 'host': '127.0.0.2', 'protocol': protocol, 'pb_port': port },
  { 'host': '127.0.0.3', 'protocol': protocol, 'pb_port': port }
])
```

For some Python code samples to get you started, see our tutorials on
[[the basics of Riak|The Basics]], [[Riak Data Types|Using Data Types]],
and [[Riak Search 2.0|Using Search]], as well as a variety of other
pages in the **Riak for Developers** section of the documentation (in
the navbar on the left).

### Erlang

How you connect to Riak with the Erlang client depends on whether you're
using Riak in a development environment with a one-node
[[cluster|Clusters]] or if you're using multiple nodes, as you would in
any production environment.

If you're developing using a single-node cluster, you can specify a
single process identifier (i.e.
[pid](http://www.erlang.org/doc/reference_manual/data_types.html#id66818))
to which your client will connect on the basis of the host and Protocol
Buffers port you provide. The example below connects the Erlang client
to a one-node cluster running on the host 101.0.0.1 and the port 8087:

```erlang
{ok, Pid} = riakc_pb_socket:start_link("101.0.0.1", 8087).
```

If connecting to multiple nodes, you can specify the connection
information for those nodes and produce multiple process identifiers.
Let's say that your cluster consists of three nodes, each with a
[[Protocol Buffers|PBC API]] port of 8087 and IPs of 101.0.0.1,
101.0.0.2, and 101.0.0.3, respectively. The following would produce
separate process identifiers for each:

```erlang
{ok, Pid1} = riakc_pb_socket:start_link("101.0.0.1", 8087),
{ok, Pid2} = riakc_pb_socket:start_link("101.0.0.2", 8087),
{ok, Pid3} = riakc_pb_socket:start_link("101.0.0.3", 8087).
```

For some Erlang code samples to get you started, see our tutorials on
[[the basics of Riak|The Basics]], [[Riak Data Types|Using Data Types]],
and [[Riak Search 2.0|Using Search]], as well as a variety of other
pages in the **Riak for Developers** section of the documentation (in
the navbar on the left).

