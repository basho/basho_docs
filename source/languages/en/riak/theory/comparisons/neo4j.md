---
title: Riak Compared to Neo4j
project: riak
version: 1.1.0+
document: appendix
toc: true
index: true
keywords: [comparisons, neo4j]
moved: {
  '1.4.0-': '/references/appendices/comparisons/Riak-Compared-to-Neo4j'
}
---

This document is intended as a brief, objective, and technical
comparison of Riak and Neo4j. If you feel this comparison is unfaithful
for whatever reason, please [submit an issue](https://github.com/basho/basho_docs/issues/new)
or send an email to **docs@basho.com**.

## High-Level Differences

Riak and Neo4j are meant for storing fundamentally different types of
data:

* Riak is primarily a key/value store with a variety of features included, e.g. [[Riak Data Types|Using Data Types]], [[Riak Search|Using Search]], [[secondary indexes|Using Secondary Indexes]], and more
* Neo4j is a graph database, designed to store and traverse a network of related information, e.g. social network-related data

In most cases, the needs of your application will clearly dictate
whether you should use a key/value store or a graph database, and in
many cases it may make sense to combine the two. An application like
Facebook, for example, might store user profile information, wall posts,
and images in a key/value database like Riak while storing information
about connections between friends in a graph database like Neo4j.

## Scalability

Riak was built to scale elastically, meaning that you can scale your
cluster from one node to 100 nodes and beyond with ease. As you add
nodes to your cluster, Riak automatically takes care of redistributing
an equal share of the load to each server in your cluster. Likewise, if
you scale your cluster down in size, Riak takes care of evenly
re-apportioning the data from the removed node to the remaining nodes.
Information on [[adding nodes to Riak|Basic Configuration]] elsewhere in
our documentation.

In contrast, Neo4j was designed to run on one machine and contains no
built-in support for scaling to multiple machines. While it's not
impossible to scale to multiple machines, your application must create
its own sharding layer and be able to cleanly divide the data and
coordinate all transactions. This can be a challenge, as graph databases
generally store randomly connected webs of data.

## Data Model

Riak allows you to store semi-structured documents or objects of
varying sizes. Riak is equally adept at storing a user profile as JSON,
an image, a purchase order, or session information for a website. It
even enables you to interact with special [[data types]] with rules of
convergence built in. And for larger files like videos or MP3s you can
use [Riak CS](http://docs.basho.com/riakcs/latest/).

In contrast, Neo4j is not a general purpose data storage system.
Instead, it stores data using nodes, relationships (imagine a line
connecting the nodes), and properties. You can associate a list of
properties on the node and the relationship. Properties are limited to
Java primitives (int, byte, float, etc.), Strings, or an array of
primitives and strings. Relationships are typed, allowing you to express
things like "PersonA KNOWS PersonB" or "PersonA IS_RELATED_TO PersonC."

## Conflicting Writes

Riak can detect when two processes try to update the same data with
conflicting information by means of a [[vector clocks]]. In a
distributed environment, this happens more often than you may think: a
client may update a cached version of an object, or a network split may
have caused a client to delay its write. Riak can detect both of these
cases, and uses vector clocks to determine which update should win, or
to bubble the conflicting versions (called siblings) up to the client,
where the application can choose which version wins, often with input
from the user. To illustrate this problem, imagine two people editing a
wiki at the same time.

In contrast, Neo4j supports configurable ACID transactions, similar to a
traditional RDBMS. This allows a client to update a section of the graph
in an isolated environment, hiding changes from other processes until
the transaction is committed. If multiple transactions try to modify the
same data, the Neo4j kernel will try to synchronize them. If
interdependencies between the transactions would cause a deadlock, this
will be detected and a corresponding exception will be thrown.

* [Transactions in Neo4j](http://docs.neo4j.org/chunked/milestone/transactions.html)

Riak's approach ensures that the data store is always write available
and that writes always succeed, even in the face of a network split or
hardware failure, so long as the client can reach at least one node in
the cluster. The tradeoff is that the client performing the read must do
a little extra work to resolve the conflict (which is the default
setting). Alternatively, you can task Riak with resolving object
conflicts on its own, although this is not recommended. More information
can be found in our documentation on [[conflict resolution]].

Neo4j's approach prevents conflicts from happening in the first place.
The tradeoff is that the client performing the write must do a little
extra work to detect and retry a failed transaction, and, as previously
mentioned, the transaction can only affect data on a single machine.

## Querying

Riak allows you to access your data using a simple key/value model or to
use additional features like [[secondary indexes|Using Secondary Indexes]]
or [[Riak Search|Using Search]] or to write your own custom Erlang
[[MapReduce|Using MapReduce]] operations.

Neo4j, on the other hand, excels at querying networks of information.
Again, drawing from the Facebook example above, a graph database would
make short work of finding all of the people who are friends of your
friends. In relational parlance, if your queries start on a single row
and explode into thousands of rows via recursive joins, then those
relations should likely be stored in a graph database.

Neo4j requires you to provide a starting node before you can perform any
queries or traversals. The starting node can be the result of a previous
traversal, or may be retrieved by using the integer ID of the node
generated by Neo4j. In this latter case, an application needs some way
to map a real world value, such as a username, to a node ID. Neo4j
currently supports tight integration with Lucene for this purpose, with
support for ACID transactions on operations that touch both Neo4j and
Lucene. Other than Lucene, any JTA compliant XA resource can participate
in Neo4j transactions.

More information can be found in the [Neo4j manual](http://docs.neo4j.org).
