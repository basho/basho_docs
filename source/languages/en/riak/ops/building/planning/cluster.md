---
title: Cluster Capacity Planning
project: riak
version: 0.10.0+
document: appendix
toc: true
keywords: [planning, cluster]
moved: {
    '1.4.0-': '/references/appendices/Cluster-Capacity-Planning/'
}
---

This is a short document that outlines the various elements and
variables that should be considered when planning your Riak cluster.
Your use case and environment variables will obviously be specific to
what you're building, but this document should set you on the right path
to planning and launching the suitable Riak cluster.

RAM
---

[RAM](http://en.wikipedia.org/wiki/Random-access_memory) should be
viewed as the most important resource when sizing your Riak cluster.
Aside from helping you keep more data closer to your users, memory will
also be required when running complex MapReduce queries, and caching
data to provide low latency request times.

### Bitcask and Memory Requirements

Your choice of local storage backend for Riak directly impacts your RAM
needs. Though Riak has pluggable backend storage, it ships with Bitcask
by default, and this is the recommended production backend. Why? Because
it's purpose built for:

* low latency request times
*  high throughput
*  the ability to handle data sets much larger than RAM w/o degradation

Bitcask's one major requirement, however, is that it must keep the
entire “keydir” in memory. The “keydir” is a hash table that maps each
concatenated bucket+key name in a Bitcask (“a Bitcask” is the name for
each file contained within each Bitcask backend ) to a fixed-size
structure giving the file, offset, and size of the most recently written
entry for that bucket+key on disk.

If you want to read more about what the keydir is and what it entails,
and some more about Bitcask in general, go
[here](http://blog.basho.com/2010/04/27/hello-bitcask/) and
[here](http://downloads.basho.com/papers/bitcask-intro.pdf). (You should
read these.)

When you calculate that your RAM needs will exceed your hardware resources,
(In other words, if you can't afford the RAM to enable you to use Bitcask.),
we recommend you use LevelDB.

Check out [[Bitcask Capacity Planning]] for more details on designing a bitcask backed cluster.

### LevelDB

If RAM requirements for Bitcask are prohibitive, Basho recommends use of the
LevelDB backend. While LevelDB doesn't require a large amount of RAM to
operate, supplying it with the maximum amount of memory available will
lead to higher performance.

<div class="info">
For more information see [[LevelDB]].
</div>

Disk
----

Now that you have an idea of how much RAM you'll need, it's time to
think about disk space. Disk space needs are much easier to calculate
and essentially boil down to this simple equation:

<div class="info">
Estimated Total Objects * Average Object Size * n_val

</div>
For example with:

* 50,000,000 objects
* an average object size of two kilobytes (2,048 bytes)
* the default n_val of 3

then you would need just over approximately **286 GBs** of disk space in
the entire cluster to accommodate your data.

(Here at Basho, we believe that databases should be durable out of the
box. When we built Riak, we did so in a way that you could write to disk
while keeping response times below your users' expectations. So this
calculation assumes you'll be keeping the entire data set on disk.)

Many of the considerations taken when configuring a machine to serve a
database can be applied to configuring a node for Riak as well. Mounting
disks with noatime and having separate disks for your OS and Riak data
lead to much better performance. See [[System Planning]] for more
information.

Read/Write Profile
------------------

Read/write ratios, as well as the the distribution of key access, should
influence the configuration and design of your cluster. If your use case
is write heavy you will need less RAM for caching, in addition if only a
certain portion of keys is accessed regularly, such as a [pareto
distribution](http://en.wikipedia.org/wiki/Pareto_distribution), you
won't need as much RAM available to cache those keys' values.

Number of Nodes
---------------
The number of nodes (i.e. physical servers) in your Riak Cluster depends
on the number of times data is [[Replicated|Replication]] across the cluster.
To ensure that the cluster is always available to respond to read and write requests, Basho recommends
a "sane default" of N=3 replicas.  This requirement can be met with a three
or four node cluster (you can tweak nodes installed through the [[Five Minute Install]]).
However, for production deployments we recommend using no fewer than 5 nodes, as node failures
in smaller clusters can compromise the fault-tolerance of the system.  Additionally, in clusters smaller than
5 nodes, a high percentage of the nodes (75-100% of them) will need to respond to each request, putting undue load on the
cluster that may degrade performance.  For more details on this recommendation, see this [blog post](http://basho.com/blog/technical/2012/04/27/Why-Your-Riak-Cluster-Should-Have-At-Least-Five-Nodes/).


Ring Size/Number of Partitions
------------------------------

Ring size is the number of partitions that make up your Riak Cluster.
This is a number that is configured before you cluster is started, and
is set in your app.config file under the
[[ring_creation_size|Configuration Files#app-config]]
parameter.

The default number of partitions in a Riak cluster is 64. This works for
smaller clusters, but if you plan to grow your cluster past 5 nodes it
is recommended you consider a larger ring size. Ring sizes must be a
power of 2. The minimum number of partitions recommended per node is 10,
and you can determine the number of partitions that will be allocated
per node by dividing the number of partitions by the number of nodes.
{{#<1.4.0}}
**At the moment, the ring size you choose will be the same for the life
of the cluster, so taking growth into consideration is extremely
important.**{{/<1.4.0}}

For most moderate-sized Riak clusters (8-16 nodes) 128, 256, and 512
partitions are excellent options that will allow you to incrementally
grow (or shrink) your cluster. If you're unsure about the best number of
partitions to use, [consult the Riak Mailing
List](http://lists.basho.com/mailman/listinfo/riak-users_lists.basho.com)
for some suggestions.

Other Factors
-------------

Riak is built to run in a clustered environment and while it will
compensate for network partitions they cause increased load on the
system. In addition, running in a virtualized environment that lacks low
latency IO access can drastically decrease performance. Before putting
your Riak cluster in production is recommended you gain a full
understanding of your environment's behavior so you know how your
cluster performs under load for an extended period of time. Doing so
will help you size your cluster for future growth and lead to optimal
performance.

Basho recommends using [[Basho Bench|Benchmarking]] for benchmarking the performance of your cluster.

### Bandwidth

Riak uses Erlang's built in distribution capabilities to provide
reliable access to data. A Riak cluster can be deployed in many
different network topologies, but it is recommended to have as little
latency between nodes as possible. High latency leads to sub-optimal
performance. It is not recommended to deploy a single Riak cluster
across two data centers. If you need this capability Basho offers an
inter-datacenter replication option that is built to keep multiple Riak
clusters in sync across several geographically diverse deployments.

* [Learn more about Riak Enterprise](http://basho.com/products/riak-overview/).

### IO

In general the biggest bottleneck for Riak will be the amount of IO
available to it, especially in the case of write-heavy work loads. Riak
functions much like any other database and the design of your disk
access should take this into account. Because Riak is clustered and your
data is stored on multiple physical nodes, you can consider forgoing a
tradition RAID setup for redundancy and focus on providing the least
latency possible using SATA Drives or SSDs, for example.

Additional resources
--------------------

* [[System Planning]]
* [[Basho Bench|Benchmarking]]
