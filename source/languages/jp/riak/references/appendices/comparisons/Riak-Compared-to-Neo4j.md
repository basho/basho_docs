---
title: Riak Compared to Neo4j
project: riak
version: 1.1.0+
document: appendix
toc: true
index: true
keywords: [comparisons, neo4j]
---

This is intended to be a brief, objective and technical comparison of Riak and Neo4j.

## High Level Differences

Riak and Neo4j are meant for storing fundamentally different types of data:

* Riak is a document database and key/value store, designed to store semi-structured documents or objects of varying sizes.
* Neo4j is a graph database, designed to store and traverse a network of related information. (e.g. Social Networks)

In most cases, the needs of your application will clearly dictate whether you should use a key/value store or a graph database. And in many cases, it may make sense to combine the two. An application like Facebook, for example, might store user profile information, wall posts, and images in a key/value or document database, and store the network of friends and associations in a graph database.

## Scalability

Riak was built to scale elastically, meaning that you can scale your cluster from one node to 100 nodes and beyond simply and easily. As you add nodes to your cluster, Riak automatically takes care of redistributing an equal share of the load to each server in your cluster. Likewise, if you scale your cluster down in size, Riak takes care of re-apportioning the data from the removed node evenly to the remaining nodes.

[[Adding Nodes to Riak|Basic Cluster Setup#Add a Second Node to Your Cluster]]

In contrast, Neo4j was designed to run on one machine and contains no built-in support to scale to multiple machines. That's not to say that you can't scale to multiple machines, it just mean that your application must create its own sharding layer, and be smart enough to cleanly divide the data, which is a challenge, as graph databases generally store randomly connected webs of data. If data is not cleanly shardable, and instead is duplicated across multiple machines, then the sharding layer must be smart enough to coordinate Neo4j transactions, as Neo4j transactions are currently bound to a single machine.

_[[http://lists.neo4j.org/pipermail/user/2009-January/000997.html]]_
_[[http://en.wikipedia.org/wiki/Six_degrees_of_separation]]_


## Data Model

Riak allows you to store semi-structured documents or objects of varying sizes. Riak is equally adept at storing a user profile, an image, an .mp3, a purchase order, or session information for a website.

[[Data Storage in Riak|Concepts#DataStorage]]

In contrast, Neo4j stores data using nodes, relationships (imagine a line connecting the nodes), and properties. You can associate a list of properties on the node and the relationship. Properties are limited to Java primitives (int, byte, float, etc.), Strings, or an array of primitives and Strings. Relationships are typed, allowing you to express things like "PersonA KNOWS PersonB" and "PersonA IS_RELATED_TO PersonC".


[[http://api.neo4j.org/current/org/neo4j/graphdb/PropertyContainer.html]]

## Conflicting Writes

Riak can detect when two processes try to update the same data with conflicting information by means of a vector clock. In a distributed environment, this happens more often than you would think: a client may update a cached version of an object, or a network split may have caused a client to delay its write. Riak can detect both of these cases, and uses the vector clock to determine which update should win, or to bubble the conflicting versions (called siblings) up to the client, where the application can choose which version wins, often with input from the user. (Think of what happens when two people edit a Wiki at the same time.)

[[Vector Clocks]]

In contrast, Neo4j supports configurable ACID transactions, similar to a traditional RDBMS. This allows a client to update a section of the graph in an isolated environment, hiding changes from other processes until the transaction is committed. If multiple transactions try to modify the same data, the Neo4j kernel will try to synchronize them. If interdependencies between the transactions would cause a deadlock, this will be detected and a corresponding exception will be thrown.

* [[http://docs.neo4j.org/chunked/milestone/transactions.html]]

Riak's approach ensures that the datastore is always write-available, and that writes always succeed, even in the face of a network split or hardware failure, so long as the client can reach at least one node in the cluster. The tradeoff is that the client performing the read must do a little extra work to resolve the conflict, or can optionally choose to take the latest version of the object (this is the default setting.)

Neo4j's approach prevents conflicts from happening in the first place. The tradeoff is that client performing the write must do a little extra work to detect and retry a failed transaction, and, as previously mentioned, the transaction can only affect data on a single machine.

## Querying

Riak allows you to access your data using a simple key/value model. In addition, Riak supports links and Javascript-based Map/Reduce:

With links, you create lightweight pointers between your data, for example, from 'projects' to 'milestones' to 'tasks', and then select data along that hierarchy using simple client API commands. (In a pinch, this can substitute as a lightweight graph database, as long as the number of links is kept reasonably low; think dozens, not thousands.)

With Javascript-based MapReduce you can define a MapReduce operation with multiple map and reduce phases, and then pass the operation to a Riak cluster along with a set of starting keys. Riak works like a mini-Hadoop, running the MapReduce operation in real time and returning the results. Any data processing happens in parallel across all machines, and any operations that run on a specific piece of data run on the machine that holds that data.

* [[MapReduce in Riak|MapReduce]]

Neo4j, on the other hand, excels at querying networks of information. Again, drawing from Facebook, a graph database would make short work of finding all of the people who are friends of your friends. In relational parlance, if your queries start on a single row and explode into thousands of rows via recursive joins, then those relations should likely be stored in a graph database.

Neo4J requires you to provide a starting node before you can perform any queries or traversals. The starting node can be the result of a previous traversal, or may be retrieved by using the integer ID of the node generated by Neo4j. In this latter case, an application needs some way to map a real world value, such as a username, to a node ID. Neo4j currently supports tight integration with Lucene for this purpose, with support for ACID transactions on operations that touch both Neo4j and Lucene. Other than Lucene, any JTA compliant XA resource can participate in Neo4j transactions.

* [[Neo4j Manual|http://docs.neo4j.org/]]
* [[http://highscalability.com/neo4j-graph-database-kicks-buttox]]
