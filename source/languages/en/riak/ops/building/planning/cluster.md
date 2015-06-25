---
title: Cluster Capacity Planning
project: riak
version: 0.10.0+
document: appendix
toc: true
keywords: [planning, cluster]
moved: {
    '1.4.0-': '/references/appendices/Cluster-Capacity-Planning'
}
---

This document outlines the various elements and variables that should be
considered when planning your Riak cluster. Your use case and
environment variables will be specific to what you're building, but this
document should set you on the right path when planning and launching a
Riak cluster.

## RAM

[RAM](http://en.wikipedia.org/wiki/Random-access_memory) should be
viewed as the most important resource when sizing your Riak cluster.
Aside from helping you keep more data closer to your users, memory will
also be required when running complex MapReduce queries and for caching
data to provide low-latency request times.

### Bitcask and Memory Requirements

Your choice of local storage backend for Riak directly impacts your RAM
needs. Though Riak has pluggable backend storage, Bitcask is the
default.  Why? Because it's built for:

* low-latency request times
* high throughput
* the ability to handle data sets much larger than RAM w/o degradation

Bitcask's one major requirement, however, is that it must keep the
entire **keydir** in memory. The keydir is a hash table that maps each
concatenated bucket + key name in a Bitcask (“a Bitcask” is the name for
each file contained within each Bitcask backend) to a fixed-size
structure giving the file, offset, and size of the most recently written
entry for that bucket + key on disk.

If you want to read more about what the keydir is and what it entails,
as well as more about Bitcask in general, see the [Hello
Bitcask](http://basho.com/hello-bitcask/) article from
the Basho blog as well as Basho's [Introduction to
Bitcask](http://basho.com/assets/bitcask-intro.pdf) paper.


When you calculate that your RAM needs will exceed your hardware
resources--in other words, if you can't afford the RAM to enable you to
use Bitcask---we recommend that you use LevelDB.

Check out [[Bitcask Capacity Planning]] for more details on designing a
Bitcask-backed cluster.

### LevelDB

If RAM requirements for Bitcask are prohibitive, Basho recommends use of
the LevelDB backend. While LevelDB doesn't require a large amount of RAM
to operate, supplying it with the maximum amount of memory available
will lead to higher performance.

For more information see [[LevelDB]].

## Disk

Now that you have an idea of how much RAM you'll need, it's time to
think about disk space. Disk space needs are much easier to calculate
and essentially boil down to this simple equation:

#### Estimated Total Objects * Average Object Size * n_val

For example, with

* 50,000,000 objects
* an average object size of two kilobytes (2,048 bytes)
* the default `n_val` of 3

then you would need just over approximately **286 GB** of disk space in
the entire cluster to accommodate your data.

We believe that databases should be durable out of the box. When we
built Riak, we did so in a way that you could write to disk while
keeping response times below your users' expectations. So this
calculation assumes that you'll be keeping the entire data set on disk.

Many of the considerations taken when configuring a machine to serve a
database can be applied to configuring a node for Riak as well. Mounting
disks with noatime and having separate disks for your OS and Riak data
lead to much better performance. See [[System Planning|Planning for a
Riak System]] for more information.

### Disk Space Planning and Ownership Handoff

When Riak nodes fail or leave the cluster for some other reason, other
nodes in the cluster begin engaging in the process of **ownership
handoff**, whereby the remaining nodes assume ownership of the data
partitions handled by the node that has left. While this is an expected
state of affairs in Riak, one side effect is that this requires more
intensive disk space usage from the other nodes, in rare cases to the
point of filling the disk of one or more of those nodes.

When making disk space planning decisions, we recommend that you:

* assume that one or more nodes may be down at any time, and
* monitor your disk space usage and add additional space when usage
  exceeds 50-60% of available space.

Another possibility worth considering is using Riak with a filesystem
that allows for growth, for example
[LVM](http://en.wikipedia.org/wiki/Logical_Volume_Manager_(Linux)),
[RAID](http://en.wikipedia.org/wiki/RAID), or
[ZFS](http://en.wikipedia.org/wiki/ZFS).

## Read/Write Profile

Read/write ratios, as well as the distribution of key access, should
influence the configuration and design of your cluster. If your use case
is write heavy, you will need less RAM for caching, and if only a
certain portion of keys is accessed regularly, such as a [Pareto
distribution](http://en.wikipedia.org/wiki/Pareto_distribution), you
won't need as much RAM available to cache those keys' values.

## Number of Nodes

The number of nodes (i.e. physical servers) in your Riak Cluster depends
on the number of times data is [[replicated|Replication]] across the
cluster.  To ensure that the cluster is always available to respond to
read and write requests, Basho recommends a "sane default" of N=3
replicas.  This requirement can be met with a three- or four-node
cluster (you can tweak nodes installed through the [[Five Minute
Install]]).

For production deployments, however, we recommend using no fewer than 5
nodes, as node failures in smaller clusters can compromise the
fault-tolerance of the system.  Additionally, in clusters smaller than 5
nodes, a high percentage of the nodes (75-100% of them) will need to
respond to each request, putting undue load on the cluster that may
degrade performance.  For more details on this recommendation, see our
blog post on [Why Your Riak Cluster Should Have at Least Five
Nodes](http://basho.com/posts/technical/Why-Your-Riak-Cluster-Should-Have-At-Least-Five-Nodes/).

## Scaling

Riak can be scaled in two ways: vertically, via improved hardware, and
horizontally, by adding more nodes. Both ways can provide performance
and capacity benefits, but should be used in different circumstances.
The [[riak-admin cluster command|riak-admin Command Line#cluster]] can
assist in scaling in both directions.

#### Vertical Scaling

Vertical scaling, or improving the capabilities of a node/server,
provides greater capacity to the node but does not decrease the overall
load on existing members of the cluster. That is, the ability of the
improved node to handle existing load is increased but the load itself
is unchanged. Reasons to scale vertically include increasing IOPS (I/O
Operations Per Second), increasing CPU/RAM capacity, and increasing disk
capacity.

#### Horizontal Scaling

Horizontal scaling, or increasing the number of nodes in the cluster,
reduces the responsibilities of each member node by reducing the number
of partitions and providing additional endpoints for client connections.
That is, the capacity of each individual node does not change but its
load is decreased. Reasons to scale horizontally include increasing I/O
concurrency, reducing the load on existing nodes, and increasing disk
capacity.

<div class="note">
<div class="title">Note on horizontal scaling</div>
When scaling horizontally, it's best to add all planned nodes at once
with multiple <code>riak-admin cluster join</code> commands followed by
a <code>riak-admin cluster plan</code> and <code>riak-admin cluster
commit</code>.  This will help reduce the amount of data transferred
between nodes in the cluster.
</div>

#### Reducing Horizontal Scale

In the case in which a Riak cluster is over provisioned, or in response
to seasonal usage decreases, the horizontal scale of a Riak cluster can
be decreased using the `riak-admin cluster leave` command.

## Ring Size/Number of Partitions

Ring size is the number of partitions that make up your Riak cluster.
This is a number that is configured before you cluster is started, and
is set in your [[configuration files]].

The default number of partitions in a Riak cluster is 64. This works for
smaller clusters, but if you plan to grow your cluster past 5 nodes it
is recommended that you consider a larger ring size. Ring sizes must be
a power of 2. The minimum number of partitions recommended per node is
10, and you can determine the number of partitions that will be
allocated per node by dividing the number of partitions by the number of
nodes.

Because Riak clusters vary so greatly in terms of the features that are
used, the use cases that are served, and so on, there are no
hard-and-fast rules regarding the ideal partitions-per-node ratio. A
good rule of thumb, however, is that you should have between 10 and 50
data partitions per node. So if you're running a 3-node development
cluster, a ring size of 64 or 128 should work just fine, while a 10-node
cluster should work well with a ring size of 128 or 256 (64 is too small
while 512 is likely too large).

The table below provides some suggested combinations:

Number of nodes | Number of data partitions
:---------------|:-------------------------
3, 4, 5 | 64, 128
5 | 64, 128
6 | 64, 128, 256
7, 8, 9, 10 | 128, 256
11, 12 | 128, 256, 512

By extension, a ring size of 1024 is advisable only in clusters with
more than 20 nodes, 2048 in clusters with more than 40 nodes, etc.

If you're unsure about the best number of partitions to use, consult the
[Riak mailing
list](http://lists.basho.com/mailman/listinfo/riak-users_lists.basho.com)
for suggestions from the Riak community.

## Other Factors

Riak is built to run in a clustered environment, and while it will
compensate for network partitions, they do cause increased load on the
system. In addition, running in a virtualized environment that lacks
low-latency IO access can drastically decrease performance. Before
putting your Riak cluster in production is recommended that you gain a
full understanding of your environment's behavior so that you know how
your cluster performs under load for an extended period of time. Doing
so will help you size your cluster for future growth and lead to optimal
performance.

Basho recommends using [[Basho Bench]] for benchmarking the performance
of your cluster.

### Bandwidth

Riak uses Erlang's built-in distribution capabilities to provide
reliable access to data. A Riak cluster can be deployed in many
different network topologies, but it is recommended that you produce as
little latency between nodes as possible, as high latency leads to
sub-optimal performance. It is not recommended that you deploy a single
Riak cluster across two datacenters. If your use case requires this
capability, Basho offers a [[Multi Data Center Replication:
Architecture]] option that is built to keep multiple Riak clusters in
sync across several geographically diverse deployments.

* [Learn more about Riak Enterprise](http://basho.com/products/riak-overview/).

### I/O

In general, the biggest bottleneck for Riak will be the amount of I/O
available to it, especially in the case of write-heavy workloads. Riak
functions much like any other database and the design of your disk
access should take this into account. Because Riak is clustered and your
data is stored on multiple physical nodes, you should consider forgoing
a traditional RAID setup for redundancy and focus on providing the least
latency possible using SATA Drives or SSDs, for example.

## Additional resources

* [[System Planning|Planning for a Riak System]]
* [[Basho Bench]]
