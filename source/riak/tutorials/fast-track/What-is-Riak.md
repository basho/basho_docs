---
title: What is Riak
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, fast-track]
prev: "[[The Riak Fast Track]]"
up:   "[[The Riak Fast Track]]"
next: "[[Building a Dev Environment|Building a Development Environment]]"
versions: false
interest: [
"[[Clusters]]",
"[[Buckets]]",
"[[Eventual Consistency]]",
"[[Vector Clocks]]",
"[[Replication]]"
]
---

This page introduces the architecture behind Riak's core principles: availability, fault-tolerance, operational simplicity and predictable scaling. If you already know this, you can skip it and go [[build a four-node cluster|Building a Development Environment]].

## How Does a Riak Cluster Work?

### What is a Riak node?

Each node in a Riak cluster is the same, containing a complete, independent copy of the Riak package. There is no "master." No node has more or different responsibilities. This uniformity provides the basis for Riak's fault-tolerance and scalability. Riak is written in Erlang, a language designed for massively scalable systems requiring high availability, concurrency and fault-tolerance. 
 
### Consistent Hashing

Data is distributed across nodes using consistent hashing. Consistent hashing ensures data is evenly distributed around the cluster and new nodes can be added with automatic, minimal reshuffling of data. This significantly decreases risky "hot spots" in the database and lowers the operational burden of scaling.

How does consistent hashing work? Riak stores data using a simple key/value scheme. These keys and values are stored in a namespace called a bucket. When you add new key/value pairs to a bucket in Riak, each object's bucket and key combination is hashed. The resulting value maps onto a 160-bit integer space. You can think of this integer space as a ring used to figure out what data to put on which physical machines. 

How? Riak divides the integer space into equally-sized partitions (default is 64). Each partition owns the given range of values on the ring, and is responsible for all buckets and keys that, when hashed, fall into that range. Each partition is managed by a process called a virtual node (or "vnode"). Physical machines evenly divide responsibility for vnodes. Let's say you have a 4 node cluster with 32 partitions, managed by 32 vnode processes. Each of the four physical machines claim eight vnodes as illustrated below. Each physical machine thus becomes responsible for all keys represented by its eight vnodes. 

![A Riak Ring](/images/riak-ring.png)

Due to the even distribution created by the hashing function and how physical nodes share responsibility for keys, Riak ensures data is evenly dispersed.   

### Intelligent Replication

Riak's replication scheme means that if nodes go down, you can still read, write and update data. Riak allows you to set a replication number, "n". An _n_ value of 3 (default) means that each object is replicated 3 times. When an object's key is mapped onto a given partition, Riak won't stop there - it automatically replicates the data onto the next two partitions as well.

![A Riak Ring](/images/riak-data-distribution.png)

### Riak Automatically Re-Distributes Data When Capacity is Added

When you add machines, data is rebalanced automatically with no downtime. New machines take responsibility for their share of data by assuming ownership of some of the partitions; existing cluster members hand off the relevant partitions and the associated data. The new node continues claiming partitions until data ownership is equal, updating a picture of which nodes own what as it goes. This picture of cluster state is shared to every node using a gossip protocol and serves as a guide to route requests. This is what makes it possible for any node in the cluster to receive requests - developers don't need to deal with the underlying complexity of what data is where.  

## When Things Go Wrong

Riak retains fault-tolerance, data integrity, and availability even in failure conditions like hardware failure and network partitions. Riak has a number of properties to address these scenarios and other bumps in the road, like version conflicts in data. 

### Hinted Handoff

Hinted handoff lets Riak handle node failure. If a node fails, a neighboring node will take over its storage operations. When the failed node returns, the updates received by the neighboring node are handed back to it. This ensures availability for writes and updates and happens automatically, minimizing the operational burden of failure conditions.

### Version Conflicts

In any system that replicates data, conflicts can arise - e.g., if two clients update the same object at the exact same time; or if not all updates have yet reached hardware that is experiencing lag. Further, in Riak, replicas are "eventually consistent"-  while data is always available, not all replicas may have the most recent update at the exact same time, causing brief periods (generally on the order of milliseconds) of inconsistency while all state changes are synchronized. 

How is divergence addressed? When you make a read request, Riak looks up all replicas for that object. By default, Riak will return the most updated version, determined by looking at the object's vector clock. Vector clocks are metadata attached to each replica when it is created. They are extended each time a replica is updated to keep track of versions. You can also allow clients to resolve conflicts themselves.

### Read Repair
Further, when an outdated replica is returned as part of a read request, Riak will automatically update the out-of-sync replica to make it consistent. Read repair, a self-healing property of the database, will even update a replica that returns a "not_found" in the event that a node loses it due to physical failure.

### Reading and Writing Data in Failure Conditions
In Riak, you can set an _r_ value for reads and a _w_ value for writes. These values give you control over how many replicas must respond to a request for it to succeed. Let's say you have an _n_ value of 3, but one of the physical nodes responsible for a replica is down. With r=2, only 2 replicas must return results for a successful read. This allows Riak to provide read availability even when nodes are down or laggy. The same applies for the _w_ in writes. If you don't specify, Riak defaults to quorum: the majority of nodes must respond. There will be more on [[Tunable CAP Controls in Riak]] later in the Fast Track.
