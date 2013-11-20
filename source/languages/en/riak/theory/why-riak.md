---
title: Why Riak
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, fast-track]
versions: false
interest: [
"[[Clusters]]",
"[[Buckets]]",
"[[Eventual Consistency]]",
"[[Vector Clocks]]",
"[[Replication]]"
]
moved: {
  '1.4.0-': '/tutorials/fast-track/What-is-Riak'
}
---

## What is Riak?

Riak is a distributed database designed for maximum availability: so long as your client can reach one server, it should be able to write data. In most failure scenarios the data you want to read should be available, albeit possibly stale.

This fundamental tradeoff, high availability in exchange for possibly outdated information, informs the key architectural decisions behind Riak. This idea of "eventual consistency" is a common one in distributed systems, with DNS and web caches as two notable examples.


### Basho's goals for Riak

Goal | Description
-------|-------
**Availability** | Riak writes to and reads from multiple servers to offer availability even when hardware or the network itself are experiencing failure conditions
**Operational simplicity** | Add new machines to your Riak cluster easily without incurring a larger operational burden
**Scalability** | Riak automatically distributes data around the cluster and yields a near-linear performance increase as you add capacity
**Masterless** | Your requests are not held hostage to a specific server in the cluster that may or may not be available


### When Riak makes sense

You should definitely take a close look at Riak whenever your data does not fit on a single server.  Distributed databases are a **very** hard to problem to solve well.

Riak's availability focus makes it a good fit whenever downtime is unacceptable. No one can promise 100% uptime, but Riak is designed to survive network partitions and hardware failures that would significantly disrupt most databases.

A less-heralded feature of Riak is its predictable latency. Because its fundamental operations (read, write, delete) do not involve complex data joins or locks, it services those requests promptly, and thanks to this is often selected as a data storage backend for other data management software.

### When Riak is less of a fit

Basho recommends no fewer than 5 servers in a cluster, so Riak is typically overkill for small databases. If you don't know that you need a distributed database, you probably don't need Riak.

(Having said that, if explosive growth is a possibility, you are highly advised to prepare for that in advance. Scaling at Internet speeds is sometimes compared to overhauling an airplane mid-flight.)

Riak's simple data model, keys and values, means that to be reasonably performant your data must be denormalized. For most applications this is not a serious hurdle, but if your data simply cannot be effectively managed as keys and values, Riak will most likely not be the best fit for you.

Related: while Riak offers ways to find values that match certain criteria, if your application demands a high query load by any means other than the keys, Riak will not be as efficient as other databases. Basho offers a tool named **basho_bench** to help measure its performance so you can decide whether the availability and operational benefits of Riak outweigh its disadvantages.

## How Does a Riak Cluster Work?

### What is a Riak node?

A Riak node is not quite the same as a server, but in a production environment the two should be equivalent. A developer may run multiple nodes on a single laptop, but never would this be advisable in a real cluster.

Each node in a Riak cluster is equivalent, containing a complete, independent copy of the Riak package. There is no "master." No node has more or different responsibilities. This uniformity provides the basis for Riak's fault-tolerance and scalability.

Each node is responsible for multiple data partitions as discussed below.

### Riak Automatically Re-Distributes Data When Capacity is Added

When you add (or remove) machines, data is rebalanced automatically with no downtime. New machines claim data until ownership is equally spread around the cluster, with the resulting cluster status updates shared to every node via a gossip protocol and used to route requests. This is what makes it possible for any node in the cluster to receive requests - developers don't need to deal with the underlying complexity of where data lives.

### Consistent Hashing

Data is distributed across nodes using consistent hashing. Consistent hashing ensures data is evenly distributed around the cluster and makes possible the automatic redistribution of data as the cluster scales.

How does consistent hashing work? Riak stores data using a simple key/value scheme. These keys are associated with a namespace called a bucket. When you perform key/value operations in Riak, the bucket and key combination is hashed. The resulting hash maps onto a 160-bit integer space. You can think of this integer space as a ring used to determine what data to put on which physical machines.

How? Riak divides the integer space into equally-sized partitions. Each partition owns a range of values on the ring, and is responsible for all buckets and keys that, when hashed, fall into that range. Each partition is managed by a process called a virtual node (or "vnode"). Physical machines evenly divide responsibility for vnodes. Let's say you have a 4 node cluster with 32 partitions, managed by 32 vnode processes. Each of the four physical machines claim eight vnodes as illustrated below. Each physical machine thus becomes responsible for all keys represented by its eight vnodes.

![A Riak Ring](/images/riak-ring.png)

### Intelligent Replication

Riak's replication scheme means that if nodes go down, you can still read, write and update data. Riak allows you to set a replication number, "n". An _n_ value of 3 (default) means that each object is replicated 3 times. When an object's key is mapped onto a given partition, Riak won't stop there - it automatically replicates the data onto the next two partitions as well.

![A Riak Ring](/images/riak-data-distribution.png)

## When Things Go Wrong

Riak retains fault-tolerance, data integrity, and availability even in failure conditions like hardware failure and network partitions. Riak has a number of properties to address these scenarios and other bumps in the road, like version conflicts in data.

### Hinted Handoff

Hinted handoff lets Riak handle node failure. If a node fails, a neighboring node will take over its storage operations. When the failed node returns, the updates received by the neighboring node are handed back to it. This ensures availability for writes and updates and happens automatically, minimizing the operational burden of failure conditions.

### Version Conflicts

In any system that replicates data, conflicts can arise - e.g., if two clients update the same object at the exact same time; or if not all updates have yet reached hardware that is experiencing lag. Further, in Riak, replicas are "eventually consistent" - while data is always available, not all replicas may have the most recent update at the exact same time, causing brief periods (generally on the order of milliseconds) of inconsistency while all state changes are synchronized.

How is divergence addressed? When you make a read request, Riak looks up all replicas for that object. By default, Riak will return the most updated version, determined by looking at the object's vector clock. Vector clocks are metadata attached to each replica when it is created. They are extended each time a replica is updated to keep track of versions. You can also allow clients to resolve conflicts themselves.

### Read Repair
Further, when an outdated replica is returned as part of a read request, Riak will automatically update the out-of-sync replica to make it consistent. Read repair, a self-healing property of the database, will even update a replica that returns a "not_found" in the event that a node loses it due to physical failure.

### Reading and Writing Data in Failure Conditions
In Riak, you can set an _r_ value for reads and a _w_ value for writes. These values give you control over how many replicas must respond to a request for it to succeed. Let's say you have an _n_ value of 3, but one of the physical nodes responsible for a replica is down. With r=2, only 2 replicas must return results for a successful read. This allows Riak to provide read availability even when nodes are down or laggy. The same applies for the _w_ in writes. If you don't specify, Riak defaults to quorum: the majority of nodes must respond. There will be more on [[Replication Properties]].
