---
title: Riak Glossary
project: riak
version: 0.10.0+
document: appendix
toc: true
audience: intermediate
keywords: [appendix, concepts]
moved: {
  '1.4.0-': '/references/appendices/concepts/Riak-Glossary'
}
---

Below is a list of terms that you may run into frequently in the
documentation for Riak, along with links to more in-depth treatments.

## Active Anti-Entropy (AAE)

A continuous background process that compares and repairs any divergent,
missing, or corrupted replicas. Unlike [[read
repair|Replication#Read-Repair]], which is only triggered when data is
read, the Active Anti-Entropy system ensures the integrity of all data
stored in Riak. This is particularly useful in clusters containing “cold
data,” i.e. data that may not be read for long periods of time,
potentially years. Furthermore, unlike the repair command, Active
Anti-Entropy is an automatic process requiring no user intervention. It
is enabled by default in Riak 1.3 and greater.

* [[Replication|Replication#Active-Anti-Entropy-AAE-]]

## Basho Bench

Basho Bench is a benchmarking tool created to conduct accurate and
repeatable performance tests and stress tests and to produce performance
graphs.

* [[Basho Bench]]
* [[GitHub repository|http://github.com/basho/basho_bench/]]

## Bucket

A bucket is a namespace for data stored in Riak, with a set of common
properties for its contents, e.g. the number of replicas (`n_val`),
whether siblings are returned on reads (`allow_mult`), etc. Buckets'
properties are determined by their bucket type (see below).

* [[Buckets]]
* [[HTTP Bucket Operations|HTTP API#Bucket-Operations]]

## Bucket Type

Bucket types enable you to create and manage sets of bucket properties
that, when applied to buckets, dictate those buckets' behavior. They
also act as a third namespace in Riak in addition to buckets and keys.

* [[Bucket Types|Using Bucket Types]]

## Cluster

A Riak cluster is a 160-bit integer space which is divided into
equally-sized partitions. Each vnode in the Riak Ring is responsible for
one of these partitions.

* [[Clusters]]
* [[Dynamo]]

## Consistent Hashing

Consistent hashing is a technique used to limit the reshuffling of keys
when a hash-table data structure is rebalanced (i.e. when slots are
added or removed). Riak uses consistent hashing to organize its data
storage and replication. Specifically, the vnodes in the Riak Ring
responsible for storing each object are determined using the consistent
hashing technique.

* [[Clusters]]
* [[Dynamo]]
* [[Wikipedia:Consistent Hashing|http://en.wikipedia.org/wiki/Consistent_hashing]]

## Data Types

Riak Data Types are data objects inspired by research on
[CRDTs](http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf) that use
certain rules of convergence to dictate how conflicts between replicas
are resolved in Riak's eventually consistent system. There are five Riak
Data Types in total: flags, registers, counters, sets, and maps.

* [[Data Types Concept|Data Types]]
* [[Using Data Types]]
* [[Data Modeling with Riak Data Types]]

## Eventual Consistency

A consistency model that informally guarantees that if no new updates
are made to a given data item, all reads on that item will eventually
return the last updated value. Details about what this means in Riak can
be found in the document below.

* [[Eventual Consistency]]

## Gossiping

Riak uses a "gossip protocol" to share and communicate ring state and
bucket properties around the cluster. Whenever a node changes its claim
on the ring, it announces its change via this protocol. Each node also
periodically sends its current view of the ring state to a randomly
selected peer in case any nodes missed previous updates.

* [[Clusters]]
* [[Adding and Removing Nodes|Adding and Removing Nodes#The-Node-Join-Process]]

## Hinted Handoff

Hinted handoff is a technique for dealing with node failure in the Riak
cluster in which neighboring nodes temporarily take over storage
operations for the failed node. When the failed node returns to the
cluster, the updates received by the neighboring nodes are handed off to
it.

Hinted handoff allows Riak to ensure database availability.  When a node
fails, Riak can continue to handle requests as if the node were still
there.

* [[Recovering a Failed Node]]

## Key

Keys are unique object identifiers in Riak and are scoped within buckets
and bucket types.

* [[Keys and Objects]]
* [[Developer Basics|The Basics]]

## Lager

[[Lager|https://github.com/basho/lager]] is an Erlang/OTP framework that
ships as Riak's default logger.

## MapReduce

Riak's MapReduce gives developers the capability to perform more
powerful queries over the data stored in their key/value data.

* [[Using MapReduce]]
* [[Advanced MapReduce]]

## Node

A node is analogous to a physical server. Nodes run a certain number of
nodes, each of which claims a partition in the Riak Ring key space.

* [[Clusters]]
* [[Adding and Removing Nodes]]

## Object

An object is another name for a value.

* [[Keys and Objects]]
* [[Developer Basics|The Basics]]

## Partition

Partitions are the spaces into which a Riak cluster is divided. Each
vnode in Riak is responsible for a partition. Data is stored on a set
number of partitions determined by the `n_val` setting, with the target
partitions chosen statically by applying consistent hashing to an
object's key.

* [[Clusters]]
* [[Eventual Consistency]]
* [[Cluster Capacity Planning|Cluster Capacity Planning#Ring-Size-Number-of-Partitions]]

## Quorum

Quorum in Riak has two meanings:

* The quantity of replicas that must respond to a read or write request
  before it is considered successful. This is defined as a bucket
  property or as one of the relevant parameters to a single request
  (R,W,DW,RW).
* A symbolic quantity for the above, `quorum`, which is equivalent to
  `n_val` / 2 + 1. The default setting is `2`.

* [[Eventual Consistency]]
* [[Replication Properties]]
* [[Understanding Riak's Configurable Behaviors: Part
  2|http://basho.com/riaks-config-behaviors-part-2/]]

## Sloppy Quorum

During failure scenarios, in which available nodes < total nodes, sloppy
quorum is used to ensure that Riak is still available to take writes.
When a primary node is unavailable, another node will accept its write
requests. When the node returns, data is transferred to the primary node
via the [[Hinted Handoff|Riak Glossary#Hinted-Handoff]] process.

## Read Repair

Read repair is an anti-entropy mechanism that Riak uses to
optimistically update stale replicas when they reply to a read request
with stale data.

* [[More about Read Repair|Replication]]

## Replica

Replicas are copies of data stored in Riak. The number of replicas
required for both successful reads and writes is configurable in Riak
and should be set based on your application's consistency and
availability requirements.

* [[Eventual Consistency]]
* [[Understanding Riak's Configurable Behaviors: Part
  2|http://basho.com/riaks-config-behaviors-part-2/]]

## Riak Core

Riak Core is the modular distributed systems framework that serves as
the foundation for Riak's scalable architecture.

* [[Riak Core|https://github.com/basho/riak_core]]
* [[Where To Start With Riak
  Core|http://basho.com/where-to-start-with-riak-core/]]

## Riak KV

Riak KV is the key/value datastore for Riak.

* [[Riak KV|https://github.com/basho/riak_kv]]

## Riak Pipe

Riak Pipe is the processing layer that powers Riak's MapReduce. It's
best described as "UNIX pipes for Riak."

* [[Riak Pipe|https://github.com/basho/riak_pipe]]
* [[Riak Pipe - the New MapReduce Power|http://basho.com/riak-pipe-the-new-mapreduce-power/]]
* [[Riak Pipe - Riak's Distributed Processing Framework|http://vimeo.com/53910999]]

## Riak Search

Riak Search is a distributed, scalable, failure-tolerant, realtime,
full-text search engine integrating [Apache
Solr](https://lucene.apache.org/solr/) with Riak KV.

* [[Using Search]]
* [[Search Details]]

## Ring

The Riak Ring is a 160-bit integer space. This space is equally divided
into partitions, each of which is claimed by a vnode, which themselves
reside on actual physical server nodes.

* [[Clusters]]
* [[Dynamo]]
* [[Cluster Capacity Planning|Cluster Capacity Planning#Ring-Size-Number-of-Partitions]]

## Secondary Indexing (2i)

Secondary Indexing in Riak gives developers the ability to tag an object
stored in Riak with one or more values which can then be queried.

* [[Using Secondary Indexes]]
* [[Advanced Secondary Indexes]]
* [[Repairing Indexes]]

## Strong Consistency

While Riak is most well known as an [[eventually consistent|Eventual
Consistency]] data storage system, versions of Riak 2.0 and greater
enable you to apply strong consistency guarantees to some or all of your
data, thus using Riak as a CP (consistent plus partition-tolerant)
rather than AP (highly available plus partition-tolerant) system.

* [[Strong Consistency Concept|Strong Consistency]]
* [[Using Strong Consistency]]

## Value

Riak is best described as a key/value store. In versions of Riak prior
to 2.0, all "values" are opaque BLOBs (binary large objects) identified
with a unique key. Values can be any type of data, including a string, a
JSON object, a text document, etc. Modifying values involves fetching
the value that exists in Riak and substituting it for a new value;
operations on values are thus basic CRUD operations.

[[Riak Data Types|Data Types]], added in version 2.0, are an important
exception to this. While still considered values---because they are
stored in bucket type/bucket/key locations, like anything in Riak---Riak
Data Types are not BLOBs and are modified by Data Type-specific
operations.

* [[Keys and Objects]]
* [[Developer Basics|The Basics]]
* [[Data Types]]
* [[Using Data Types]]

## Vector Clock

Riak utilizes vector clocks (or _vclocks_) to handle version control.
Since any node in a Riak cluster is able to handle a request, and not
all nodes need to participate, data versioning is required to keep track
of a current value. When a value is stored in Riak, it is tagged with a
vector clock and establishes the initial version. When it is updated,
the client provides the vector clock of the object being modified so
that this vector clock can be extended to reflect the update.  Riak can
then compare vector clocks on different versions of the object and
determine certain attributes of the data.

* [[Vector clocks]]

## Vnode

Vnodes, or "virtual nodes," are responsible for claiming a partition in
the Riak Ring, and they coordinate requests for these partitions. Vnodes
reside on physical nodes in a Riak cluster, and the number of vnodes per
physical node is determined by the total number of vnodes and the number
of active physical nodes in the cluster. Riak balances the assignment of
vnodes across the active physical nodes.

* [[Clusters]]
* [[Dynamo]]

