---
title: Concepts
project: riak
version: 0.10.0+
document: appendix
toc: true
audience: intermediate
keywords: [appendix, concepts]
---

This section is a high level overview of concepts, technology choices,
and implementation details that are at work in Riak.

What is Riak
------------

Simply put, Riak is a distributed, scalable, open source key/value
store. We like to say that Riak is the most powerful open-source,
distributed database you’ll ever put into production. Riak scales
predictably and easily and simplifies development by giving users the
ability to quickly prototype, test, and deploy their applications.

Basics and History
------------------

Riak is based on technology originally developed by [[Basho
Technologies|http://basho.com]] to run a Salesforce automation business.
There was more interest in the datastore technology than the
applications built on it so Basho decided to build a business around
Riak itself.

Riak is heavily influenced by Dr. Eric Brewer’s [[CAP Theorem|http://en.wikipedia.org/wiki/CAP_theorem]] and [[Amazon’s Dynamo Paper|Dynamo]].
Most of the core team comes from Akamai which informed Riak’s focus on operational ease and fault tolerance.

The Riak APIs
-------------

The team that wrote Riak is also responsible for the Erlang REST
framework [Webmachine](http://webmachine.basho.com), so it’s not surprising Riak uses [[a REST
API|HTTP API]] for one of the two ways you can access data in Riak.
Storage operations use HTTP PUTs or POSTs and fetches use HTTP GETs.
Storage operations are submitted to a pre-defined URL which defaults to
‘/riak’.

In addition to HTTP, Riak also ships with a fully-featured [[Protocol
Buffers API|PBC-API]]. This is a simple binary protocol based on the
library Google’s open source project of the same name.

Client Libraries
----------------

Basho and the Riak community support and develop a wide variety of
client libraries that connect to Riak.

Currently Basho [[supports libraries|Client Libraries]] for Ruby, Java, Erlang, Python, PHP, and C/C++.

The [[Riak Community writes and supports|Community Developed Libraries
and Projects]] client code for languages and frameworks like Node.js, Go, Groovy,
Haskell, and much more.

Buckets, Keys, and Values
-------------------------

[[Buckets]] and [[keys|Keys and Objects]] are the only way to organize
data inside of Riak. Data is stored and referenced by bucket/key pairs.
Each key is attached to a unique value that can be any data type.

Clustering
----------

Central to any Riak cluster is a 160-bit integer space which is divided
into equally-sized partitions.

Physical servers, referred to in the cluster as "nodes," run a certain
number of virtual nodes, or "vnodes". Each vnode will claim a partition
on the ring. The number of active vnodes is determined by the number of
physical nodes in the cluster at any given time.

As a rule, each node in the cluster is responsible for 1/(total number
of physical nodes) of the ring. You can determine the number of vnodes
on each node by calculating (number of partitions)/(number of nodes).
More simply put, a ring with 32 partitions, composed of four physical
nodes, will have approximately eight vnodes per node. This setup is
represented in the diagram below.

![Riak Ring](/images/riak-ring.png)

Nodes can be added and removed from the cluster dynamically and Riak
will redistribute the data accordingly.

Riak is designed, from the ground up, to run in a distributed
environment. Core operations, such as read/writing data and executing
map/reduce jobs, actually become faster when more Riak nodes are added
to a cluster.

### No master node

All nodes in a Riak cluster are equal. Each node is fully capable of
serving any client request. This is possible due to the way Riak uses
consistent hashing to distribute data around the cluster.

### Storage implications

Riak communicates bucket information around the cluster using a [[gossip
protocol|Riak-Glossary#Gossiping]]. In general, large numbers of
buckets within a Riak cluster is not a problem. In practice, there are
two potential restrictions on the maximum number of buckets a Riak
cluster can handle.

First, buckets which use a non-standard set of properties will force
Riak to gossip more data around the cluster. The additional data can
slow processing and place an upper limit on performance. Second, some
backends store each bucket as a separate entity. This
can cause a node to run out of resources such as file handles. These
resource restrictions might not impact performance but they can
represent another limit on the maximum number of buckets.

Replication
-----------

[[Replication]] is built into the core of Riak’s architecture. Riak
controls how many replicas of a datum are stored via a setting called
the "N value". This value has a per-node default but can be overridden
on each bucket. Riak objects inherit the N value of their parent bucket.
All nodes in the same cluster should agree on and use the same N value.

For example, here is a depiction of what happens when n\_val = 3 (This
is the default setting). When you store a datum in a bucket with an N
value of three, the datum will replicated to three separate partitions
on the Riak Ring.

![Riak Data Distribution](/images/riak-data-distribution.png)

Riak uses a technique called ‘hinted handoff’ to compensate for failed
nodes in a cluster. Neighbors of a failed node will pick up the slack
and perform the work of the failed node allowing the cluster to continue
processing as usual. This can be considered a form of self-healing.

Reading, Writing, and Updating Data
-----------------------------------

Using the Riak APIs, Riak objects can be fetched directly if the client
knows the bucket and key. This is the fastest way to get data out of
Riak.

### R Value

Riak allows the client to supply an "R value" on each direct fetch. The
R value represents the number of Riak nodes which must return results
for a read before the read is considered successful. This allows Riak to
provide read availability even when nodes are down or laggy.

### Read Failure Tolerance

Subtracting R from N will tell you the number of down or laggy nodes a
Riak cluster can tolerate before becoming unavailable for reads. For
example, an 8 node cluster with an N of 8 and a R of 1 will be able to
tolerate up to 7 nodes being down before becoming unavailable for reads.

### Link Walking

Riak can also return objects based on links stored on the object. Link
walking can be used to return a set of related objects from a single
request.

### Vector Clocks

Each update to a Riak object is tracked by a [[vector clock|vector clocks]]. Vector clocks allow
Riak to determine causal ordering and detect conflicts in a distributed
system.

### Conflict Resolution

Riak has two ways of resolving update conflicts on Riak objects. Riak
can allow the last update to automatically "win" or Riak can return both
versions of the object to the client. This gives the client the
opportunity to resolve the conflict on its own.

### W Value

Riak’s API allows the client to supply a "W value" on each update. The W
value represents the number of Riak nodes which must report success
before an update is considered complete. This allows Riak to provide
write availability even when nodes are down or laggy.

### Write Failure Tolerance

Subtracting W from N will tell you the number of down or laggy nodes a
Riak cluster can tolerate before becoming unavailable for writes. For
example, an 8 node cluster with an N of 8 and a W of 2 will be able to
tolerate up to 6 nodes being down before becoming unavailable for
writes.

Local Disk Storage and Pluggable Backends
-----------------------------------------

Riak uses a [[backend API]] to interact with its storage subsystem. The
API allows Riak to support multiple backends which can be selected based
on use-cases. See [[Choosing a backend]] for a full list of what we
currently support. The two most heavily-used backend are Bitcask and
LevelDB.

As of the 0.12 release, [[Bitcask]] is the default backend for Riak.
Bitcask is a simple yet powerful local key/value store that serves as
Riak’s low latency, high throughput storage back end.

<div class="info">
<div class="title">More on Bitcask</div>

* [[Hello, Bitcask (from the Basho Blog)|http://blog.basho.com/2010/04/27/hello-bitcask/]]
* [[An Architectural Overview of Bitcask (PDF)|http://downloads.basho.com/papers/bitcask-intro.pdf]]

</div>

[[LevelDB]] is an open source library release by Google. It has
different production properties than Bitcask and is required if you’re
planning to use Riak’s [[Secondary Indexes]] functionality.

MapReduce
---------

[[MapReduce|Using MapReduce]] in Riak allows you to process your data in real-time in
parallel utilizing the hardware resources of your entire cluster.
MapReduce jobs are described in JSON using a set of nested hashes
describing the inputs, phases, and timeout for a job. A job can consist
of an arbitrary number of Map and Reduce phases. For this reason,
MapReduce in Riak can be thought of as a real-time "mini-Hadoop". A job
is submitted via HTTP and the results are returned in JSON-encoded form.
(A Protocol Buffers interface is also supported.)

Secondary Indexes
-----------------

Version 1.0 of Riak added support for [[Secondary Indexes]]. This
feature allows a developer to tag a Riak value with one or more
field/value pairs. The object is indexed under these field/value pairs,
and the application can later query the index to retrieve a list of
matching keys.

Indexes are set on an object-by-object basis, there is no schema. The
indexes are defined at the time the object is written. To change the
indexes for an object, simply write the object with a different set of
indexes.

Indexing is real-time and atomic; the results show up in queries
immediately after the write operation completes, and all indexing occurs
on the partition where the object lives, so the object and its indexes
stay in sync.

Indexes can be stored and queried via the HTTP interface or the Protocol
Buffers interface. Additionally, index results can feed directly into a
Map/Reduce operation, allowing further filtering and processing of index
query results.

Riak Search
-----------

[[Riak Search]] is a distributed, easily-scalable, failure-tolerant,
real-time, full-text search engine built around Riak Core and tightly
integrated with Riak KV.

Riak Search allows you to find and retrieve your Riak objects using the
objects’ values. When a Riak KV bucket has been enabled for Search
integration (by installing the Search pre-commit hook), any objects
stored in that bucket are also indexed seamlessly in Riak Search.

Commit Hooks
------------

[[Commit Hooks|Using Commit Hooks]] are invoked before or after a value is persisted and
can greatly enhance the functionality of any application. Commit hooks
can:

-   allow a write to occur with an unmodified object
-   modify the object
-   Fail the update and prevent any modifications

Post-commit hooks are notified after the fact and should not modify the
riak\_object. Updating riak\_objects in post-commit hooks can cause
nasty feedback loops which will wedge the hook into an infinite cycle
unless the hook functions are carefully written to detect and
short-circuit such cycles.

Pre- and post-commit hooks are defined on a per-bucket basis and are
stored in the target bucket’s properties. They are run once per
successful response to the client.

Links and Link Walking
----------------------

[[Links]] are metadata that establish one-way relationships between
objects in Riak. They can be used to loosely model graph like
relationships between objects in Riak.
