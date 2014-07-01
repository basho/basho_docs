---
title: Riak Compared to MongoDB
project: riak
version: 1.1.0+
document: appendix
toc: true
index: true
keywords: [comparisons, mongodb]
moved: {
  '1.4.0-': '/references/appendices/comparisons/Riak-Compared-to-MongoDB'
}
---

This is intended to be a brief, objective, and technical comparison of Riak and MongoDB. The MongoDB version described is 2.2.x. The Riak version described is Riak 1.2.x. If you feel this comparison is unfaithful for whatever reason, please [fix it](https://github.com/basho/basho_docs/issues/new) or send an email to **docs@basho.com**.

## At A Very High Level

* Riak is Apache 2.0 licensed; MongoDB is distributed under the AGPL
* Riak is written primarily in Erlang with some bits in C; MongoDB is written in C++

## Feature/Capability Comparison

The table below provides a high-level comparison of Riak and MongoDB features and capabilities. To keep this page relevant in the face of rapid development on both sides, low-level details can be found in links to specific pages in the online documentation for both systems.

<table>
    <tr>
        <th WIDTH="15%">Feature/Capability</th>
        <th WIDTH="42%">Riak</th>
        <th WIDTH="43%">MongoDB</th>
    </tr>
    <tr>
        <td>Data Model</td>
        <td>Riak stores key/value pairs under [[keys|Keys and Objects]] in [[buckets]]. [[Using bucket types]] you can set bucket-level configurations for things like [[replication properties]]. In addition to basic [[key/value lookup|Key/Value Modeling]], Riak has a variety of features for discovering objects, including [[Riak Search|Using Search]] and [[secondary indexes|Using Secondary Indexes]].</td>
        <td>MongoDB's data format is BSON (a binary equivalent to JSON) stored as documents (self-contained records with no intrinsic relationships). Documents in MongoDB may store any of the defined BSON types and are grouped in collections.
            <ul>
                <li>[[Documents|http://www.mongodb.org/display/DOCS/Documents]]</li>
                <li>[[Data Types and Conventions|http://www.mongodb.org/display/DOCS/Data+Types+and+Conventions]]</li>

            </ul>
        </td>
    </tr>
    <tr>
        <td>Storage Model</td>
        <td>Riak has a modular, extensible local storage system that lets you plug in a backend store of your choice to suit your use case. The default backend is [[Bitcask]].
            <ul>
              <li>[[Riak Supported Storage Backends|Choosing a Backend]]</li>
            </ul>

        You can also write your own storage backend for Riak using our [[backend API|Backend API]].
     </td>
        <td>MongoDB's default storage system is the Memory-Mapped Storage Engine. It uses memory-mapped files for all disk I/O. It is the responsibility of the OS to manage flushing data to disk and paging data in and out.
            <ul>
             <li>[[Caching|http://www.mongodb.org/display/DOCS/Caching]]</li>
            </ul>
        </td>
    </tr>
    <tr>
        <td>Data Access and APIs</td>
        <td>Riak offers two primary interfaces (in addition to raw Erlang access):
			<ul>
            <li>[[Protocol Buffers|PBC API]] (strongly recommended)</li>
			<li>[[HTTP|HTTP API]]</li>
			</ul>
			Riak [[client libraries]] are wrappers around these APIs, and client support exists for dozens of languages. Basho currently has officially supported clients for [[Java|https://github.com/basho/riak-java-client]], [[Ruby|https://github.com/basho/riak-ruby-client]], [[Python|https://github.com/basho/riak-python-client]], and [[Erlang|https://github.com/basho/riak-erlang-client]].
			</td>
        <td> MongoDB uses a custom, socket-based wire protocol with BSON as the interchange format. 
	
			<ul>
				<li><a href="http://www.mongodb.org/display/DOCS/Mongo+Wire+Protocol">Mongo Wire Protocol</a></li>
			</ul>
			
			10Gen and the Mongo community support many client libraries.
			<ul>	
			  <li>[[Client Libraries|http://www.mongodb.org/display/DOCS/Drivers]]</li>
			</ul>
	 </td>
    </tr>
    <tr>
        <td>Query Types and Queryability</td>
        <td>There are currently five ways to query data in Riak:
            <ul>
            <li>Via [[primary key operations|The Basics]] (GET, PUT, DELETE, UPDATE)</li>
            <li>[[Using MapReduce]]</li>
            <li>[[Using secondary indexes]]</li>
            <li>[[Using Search]]</li>
            <li>[[Using Data Types]]</li>
            </ul>

    </td>
        <td>MongoDB has a query interface that has some similarities to relational databases, including secondary indexes that can be derived from the stored documents. MongoDB also has facilities for performing MapReduce queries and ad-hoc queries on documents. Hadoop support is available as well.
                <ul>
                <li>[[Querying|http://www.mongodb.org/display/DOCS/Querying]]</li>
                <li>[[Indexes|http://www.mongodb.org/display/DOCS/Indexes]]</li>
                <li>[[MapReduce|http://www.mongodb.org/display/DOCS/MapReduce]]</li>
                <li>[[MongoDB Hadoop Adapter|https://github.com/mongodb/mongo-hadoop]]</li>
            <ul>
    </td>
    </tr>
    <tr>
        <td>Data Versioning and Consistency</td>
        <td>Riak uses a data structure called a [[vector clock|Vector Clocks]] to reason about causality and staleness of stored values. Vector clocks enable clients to always write to the database in exchange for consistency conflicts being resolved either at read time by application or client code or by Riak's [[active anti-entropy]] subsystem. Vector clocks can be configured to store copies of a given object based on the size and age of that object. There is also an option to disable vector clocks and fall back to simple timestamp-based resolution, known as [[last write wins|Conflict Resolution#Client-and-Server-side-Conflict-Resolution]].

        <ul>
            <li>[[Why Vector Clocks Are Easy|http://basho.com/blog/technical/2010/01/29/why-vector-clocks-are-easy/]]</li>
            <li>[[Why Vector Clocks Are Hard|http://basho.com/blog/technical/2010/04/05/why-vector-clocks-are-hard/]]</li>
        </ul>
        
        In addition, as of version 2.0 you can use Riak in a [[strongly consistent|Strong Consistency]] fashion.
        </td>

        <td>MongoDB exhibits strong consistency. Eventually consistent reads can be accomplished via secondaries. A MongoDB cluster (with auto-sharding and replication) has a master server at any given point in time for each shard.
            <ul>
              <li>[[On Distributed Consistency|http://blog.mongodb.org/post/475279604/on-distributed-consistency-part-1]]</li>
            </ul>
     </td>
    </tr>
        <td>Concurrency</td>
        <td>In Riak, any node in the cluster can coordinate a read/write operation for any other node. Riak stresses availability for writes and reads, and puts the burden of resolution on the client at read time.
         </td>

        <td>MongoDB relies on locks for consistency. As of version 2.2, MongoDB has a DB Level Lock for all operations.
            <ul>
                <li>[[Locks|http://docs.mongodb.org/manual/administration/monitoring/#locks]]</li>
                <li>[[DB Level Locking|https://jira.mongodb.org/browse/SERVER-4328]]</li>
                <li>[[How Does Concurrency Work?|http://www.mongodb.org/display/DOCS/How+does+concurrency+work]]</li>
            </ul>
     </td>
    </tr>
    <tr>
        <td>Replication</td>
        <td>Riak's replication system is heavily influenced by the [[Dynamo Paper|http://docs.basho.com/riak/2.0.0beta1/theory/dynamo/]] and by Dr. Eric Brewer's CAP Theorem. Riak uses consistent hashing to replicate and distribute N copies of each value around a Riak cluster composed of any number of physical machines. Under the hood, Riak uses virtual nodes to handle the distribution and dynamic rebalancing of data, thus decoupling the data distribution from physical assets.
            <ul>
              <li>[[Replication]]</li>
              <li>[[Clustering|Clusters]]</li>
            </ul>

            Riak's APIs expose tunable [[consistency and availability parameters|Replication Properties]] that let you select which configuration is best for your use case. Replication is configurable at the bucket level [[using bucket types]] when first storing data in Riak. Subsequent reads and writes to that data can have request-level parameters.
     </td>
        <td>Mongo manages replication via replica sets, a form of asynchronous master/slave replication. Traditional master/slave replication is available but not recommended.
            <ul>
            <li>[[Replication|http://www.mongodb.org/display/DOCS/Replication]]</li>
            <li>[[Replica Sets|http://www.mongodb.org/display/DOCS/Replica+Sets]]</li>
            <li>[[Master/Slave|http://www.mongodb.org/display/DOCS/Master+Slave]]</li>
            </ul>
     </td>
    </tr>
    <tr>
        <td>Scaling Out and In</td>
        <td>Riak allows you to [[elastically grow and shrink|Adding and Removing Nodes]] your cluster while evenly balancing the load on each machine. No node in Riak is special or has any particular role. In other words, all nodes are masterless. When you add a physical machine to Riak, the cluster is made aware of its membership via gossiping of [[ring state|Clusters#the-ring]]. Once it's a member of the ring, it's assigned an equal percentage of the partitions and subsequently takes ownership of the data belonging to those partitions. The process for removing a machine is the inverse of this. Riak also ships with a comprehensive suite of [[command line tools|riak-admin Command Line]] to help make node operations simple and straightforward.
        </td>
        <td>Mongo relies on sharding for scaling out. This involves designating a certain server to hold certain chunks of the data as the data set grows.

            <ul>
                <li>[[Sharding in MongoDB|http://www.mongodb.org/display/DOCS/Sharding]]</li>
                <li>[[Sharding Introduction|http://www.mongodb.org/display/DOCS/Sharding+Introduction]]</li>
                <li>[[Sharding (on Wikipedia)|http://en.wikipedia.org/wiki/Sharding]]</li>

            </ul>

            To scale in, MongoDB has support for removing shards from your database.
            <ul>
                <li>[[Removing Shards|http://docs.mongodb.org/manual/administration/sharding/#remove-a-shard-from-a-cluster]]</li>
            </ul>

    </td>
    </tr>
    <tr>
        <td>Multi-Datacenter Replication and Awareness</td>

        <td>Riak features two distinct types of [[replication]]. Users can replicate to any number of nodes in one cluster (which is usually contained within one datacenter over a LAN) using the Apache 2.0-licensed database. Riak Enterprise, Basho's commercial extension to Riak, is required for Multi-Datacenter deployments (meaning the ability to run active Riak clusters in N datacenters).

        <ul>
            <li><a href="http://basho.com/products/riak-enterprise/">Riak Enterprise</a></li>
        </ul>

        </td>
        <td>MongoDB can be configured to run in multiple datacenters via various options.

            <ul>
                    <li><a href="http://www.mongodb.org/display/DOCS/Data+Center+Awareness">Datacenter Awareness</a></li>
            </ul>

    </td>
    </tr>
    <tr>
        <td>Graphical Monitoring/Admin Console</td>
        <td>Riak ships with [[Riak Control]], an open source graphical console for monitoring and managing Riak clusters.</td>
        <td>MongoDB does not ship with a graphical monitoring/admin console. However, several community projects have developed graphical monitoring/admin programs.
            <ul>
                <li>[[Monitoring and Diagnostics|http://www.mongodb.org/display/DOCS/Monitoring+and+Diagnostics]]</li>
                <li>[[Admin UIs|http://www.mongodb.org/display/DOCS/Admin+UIs]]</li>
            </ul>

            10Gen offers a hosted monitoring service.

            <ul>
            <li><a href="http://www.10gen.com/mongodb-monitoring-service">Mongo Monitoring Service</a></li>
            </ul>
     </td>
    </tr>
</table>
