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

This is intended to be a brief, objective and technical comparison of Riak and MongoDB.  The MongoDB version described is 2.2.x. The Riak version described is Riak 1.2.x. If you feel this comparison is unfaithful at all for whatever reason, please [fix it](https://github.com/basho/basho_docs/issues/new) or send an email to **docs@basho.com**.

## At A Very High Level

* Riak is Apache 2.0 licensed; MongoDB is distributed under the AGPL
* Riak is written primarily in Erlang with some bits in C; MongoDB is written in C++

## Feature/Capability Comparison

The table below gives a high level comparison of Riak and MongoDB features/capabilities.  To keep this page relevant in the face of rapid development on both sides, low level details are found in links to Riak and MongoDB online documentation.

<table>
    <tr>

        <th WIDTH="15%">Feature/Capability</th>
        <th WIDTH="42%">Riak</th>
        <th WIDTH="43%">MongoDB</th>
    </tr>
    <tr>
        <td>Data Model</td>
        <td>Riak stores key/value pairs in a higher level namespace called a bucket.
            <ul>
              <li>[[Buckets, Keys, and Values|Concepts#Buckets-Keys-and-Values]] </li>
            </ul>
        </td>
        <td>MongoDB's data format is BSON (binary equivalent to JSON) stored as documents (self-contained records with no intrinsic relationships). Documents in MongoDB may store any of the defined BSON types and are grouped in collections.
            <ul>
                <li>[[Documents|http://www.mongodb.org/display/DOCS/Documents]]</li>
                <li>[[Data Types and Conventions|http://www.mongodb.org/display/DOCS/Data+Types+and+Conventions]]</li>

            </ul>
        </td>
    </tr>
    <tr>
        <td>Storage Model</td>
        <td>Riak has a modular, extensible local storage system which lets you plug-in a backend store of your choice to suit your use case. The default backend is Bitcask.
            <ul>
              <li>[[Riak Supported Storage Backends|Choosing a Backend]]</li>
            </ul>

        You can also write your own storage backend for Riak using our [[backend API|Backend API]].
     </td>
        <td> MongoDB's default storage system is the Memory-Mapped Storage Engine. It uses memory mapped files for all disk I/O.  It is the responsibility of the OS to manage flushing data to disk and paging data in and out.
            <ul>
             <li>[[Caching|http://www.mongodb.org/display/DOCS/Caching]]</li>
            </ul>
        </td>
    </tr>
    <tr>
        <td>Data Access and APIs</td>
        <td>Riak offers two primary interfaces (in addition to raw Erlang access):
			<ul>
			  <li>[[HTTP|HTTP API]]</li>
			<li>[[Protocol Buffers|PBC API]]</li>
			</ul>
			Riak Client libraries are wrappers around these APIs, and client support exists for dozens of languages. 
			<ul>
			  <li>[[Client Libraries]]</li>
			  <li>[[Community Projects]]</li>
			</ul>
			</td>
        <td> MongoDB uses a custom, socket-based wire protocol with BSON as the interchange format. 
	
			<ul>
				<li><a href="http://www.mongodb.org/display/DOCS/Mongo+Wire+Protocol">Mongo Wire Protocol</a></li>
			</ul>
			
			10Gen and the Mongo community support many client libraries.
			<ul>	
			  <li>[[Client-Libraries|http://www.mongodb.org/display/DOCS/Drivers]]</li>
			</ul>
	 </td>
    </tr>
    <tr>
        <td>Query Types and Query-ability</td>
        <td>There are currently four ways to query data in Riak
            <ul>
            <li>Primary key operations (GET, PUT, DELETE, UPDATE)</li>
            <li>[[MapReduce|Using MapReduce]]</li>
            <li>[[Using Secondary Indexes]]</li>
            <li>[[Using Search]]</li>
            </ul>

    </td>
        <td>MongoDB has a query interface that has some similarities to relational databases, including secondary indexes that can be derived from the stored documents. MongoDB also has a facilities for performing MapReduce queries and ad-hoc queries on documents. Hadoop support is available, too.
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
        <td> Riak uses a data structure called a vector clock to reason about causality and staleness of stored values. Vector clocks enable clients to always write to the database in exchange for consistency conflicts being resolved at read time by either application or client code. Vector clocks can be configured to store copies of a given datum based on size and age of said datum.   There is also an option to disable vector clocks and fall back to simple time-stamp based "last-write-wins".
            <ul>
              <li>[[Vector Clocks]]</li>
              <li>[[Why Vector Clocks Are Easy|http://basho.com/blog/technical/2010/01/29/why-vector-clocks-are-easy/]]</li>
              <li>[[Why Vector Clocks Are Hard|http://basho.com/blog/technical/2010/04/05/why-vector-clocks-are-hard/]]</li>
            </ul>
         </td>

        <td>MongoDB exhibits strong consistency.  Eventually consistent reads can be accomplished via secondaries.  A MongoDB  cluster (with auto-sharding and replication) has a master server at a given point in time for each shard.
            <ul>
              <li>[[On Distributed Consistency|http://blog.mongodb.org/post/475279604/on-distributed-consistency-part-1]]</li>
            </ul>
     </td>
    </tr>
        <td>Concurrency</td>
        <td> In Riak, any node in the cluster can coordinate a read/write operation for any other node. Riak stresses availability for writes and reads, and puts the burden of resolution on the client at read time.
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
        <td>Riak's replication system is heavily influenced by the Dynamo Paper and Dr. Eric Brewer's CAP Theorem. Riak uses consistent hashing to replicate and distribute N copies of each value around a Riak cluster composed of any number of physical machines. Under the hood, Riak uses virtual nodes to handle the distribution and dynamic rebalancing of data, thus decoupling the data distribution from physical assets.
            <ul>
              <li>[[Replication]]</li>
              <li>[[Clustering|Concepts#Clustering]]</li>
            </ul>

            The Riak APIs expose tunable consistency and availability parameters that let you select which level configuration is best for your use case. Replication is configurable at the bucket level when first storing data in Riak. Subsequent reads and writes to that data can have request-level parameters.
                <ul>
                    <li>[[Reading, Writing, and Updating Data|Concepts#Reading-Writing-and-Updating-Data]]</li>
                </ul>

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
        <td>Riak allows you to elastically grow and shrink your cluster while evenly balancing the load on each machine. No node in Riak is special or has any particular role. In other words, all nodes are masterless. When you add a physical machine to Riak, the cluster is made aware of its membership via gossiping of ring state. Once it's a member of the ring, it's assigned an equal percentage of the partitions and subsequently takes ownership of the data belonging to those partitions. The process for removing a machine is the inverse of this. Riak also ships with a comprehensive suite of command line tools to help make node operations simple and straightforward.

    <ul>
        <li>[[Adding and Removing Nodes]]</li>
        <li>[[Command Line Tools]]</li>
    </ul>
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

        <td>Riak features two distinct types of replication. Users can replicate to any number of nodes in one cluster (which is usually contained within one datacenter over a LAN) using the Apache 2.0 licensed database. Riak Enterprise, Basho's commercial extension to Riak, is required for Multi-Datacenter deployments (meaning the ability to run active Riak clusters in N datacenters).

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
        <td>Riak ships with Riak Control, an open source graphical console for monitoring and managing Riak clusters.
            <ul>
                <li>[[Riak Control]]</li>
                <li>[[Introducing Riak Control|http://basho.com/blog/technical/2012/02/22/Riak-Control/]]
            </ul>
    </td>
        <td>MongoDB does not ship with a graphical monitoring/admin console.  However, several community projects have developed graphical monitoring/admin programs.
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
