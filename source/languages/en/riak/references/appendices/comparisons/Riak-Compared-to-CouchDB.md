---
title: Riak Compared to CouchDB
project: riak
version: 1.1.0+
document: appendix
toc: true
index: true
keywords: [comparisons, couchdb]
---

This is intended to be a brief, objective and technical comparison of Riak and CouchDB.  The CouchDB version described is 1.2.x. The Riak version described is Riak 1.1.x. If you feel this comparison is unfaithful at all for whatever reason, please [fix it](https://github.com/basho/basho_docs/issues/new) or send an email to **docs@basho.com**.

## At A Very High Level

* Riak and CouchDB are both Apache 2.0 licensed
* Riak is written primarily in Erlang with some bits in C; CouchDB is written in Erlang

## Feature/Capability Comparison

The table below gives a high level comparison of Riak and CouchDB features/capabilities.  To keep this page relevant in the face of rapid development on both sides, low level details are found in links to Riak and CouchDB online documentation.

<table>
    <tr>

        <th WIDTH="15%">Feature/Capability</th>
        <th WIDTH="42%">Riak</th>
        <th WIDTH="43%">CouchDB</th>
    </tr>
    <tr>
        <td>Data Model</td>
        <td>Riak stores key/value pairs in a higher level namespace called a bucket. 
			<ul>
			  <li>[[Buckets, Keys, and Values|Concepts#Buckets%2C-Keys%2C-and-Values]] </li>
			</ul>
		</td>
        <td>CouchDB's data format is JSON stored as documents (self-contained records with no intrinsic relationships), grouped into "database" namespaces.
			<ul>
				<li>[[Document API|http://wiki.apache.org/couchdb/HTTP_Document_API]]</li>
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
        <td>CouchDB stores data to disk by "append-only" files. As the files continue to grow, they require occasional compaction.
			<ul>
			 <li>[[Indexes and File|http://guide.couchdb.org/draft/btree.html]]</li>
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
			<li>[[Client-Libraries]]</li>
			<li>[[Community Developed Libraries and Projects|Community-Developed-Libraries-and-Projects]]</li>
			</ul>
			</td>
        <td>CouchDB provides an HTTP API for both data access and administration.

				<ul>
				<li>[[Document API|http://wiki.apache.org/couchdb/HTTP_Document_API]]</li>
				<li>[[View API|http://wiki.apache.org/couchdb/HTTP_view_API]]</a></li>
				<li>[[DB API|http://wiki.apache.org/couchdb/HTTP_database_API]]</a></li>
				</ul>

			The CouchDB community supports many client libraries.
			<ul>	
			  <li>[[Client-Libraries|http://wiki.apache.org/couchdb/Related_Projects/#Libraries]]</li>
			</ul>		
	 </td>
    </tr>
    <tr>
        <td>Query Types and Query-ability</td>
        <td>There are currently four ways to query data in Riak
			<ul>
			<li>Primary key operations (GET, PUT, DELETE, UPDATE)</li>
			<li>[[MapReduce]]</li>
			<li>[[Secondary Indexes]]</li>
			<li>[[Riak Search]]</li>
			<li>[[MapReduce Search 2i Comparison]]</li>
			</ul>
	
	</td>
        <td>CouchDB is generally queried by direct ID lookups, or by creating MapReduce "views" that CouchDB runs to create a queryable index for querying by or computing other attributes. In addition, the ChangesAPI shows documents in the order they were last modified. Finally, there exist some community plugins to expand CouchDB's queryability, such as the CouchDB-Lucene full-text search plugin.

			<ul>
			<li>[[Views|http://wiki.apache.org/couchdb/HTTP_view_API]]</li>	
			<li>[[Changes Notifications|http://guide.couchdb.org/draft/notifications.html]]</li>
			<li>[[Lucene Plugin|https://github.com/rnewson/couchdb-lucene/]]</li>
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
		
        <td>CouchDB replicates newer document versions between nodes, making it an eventually consistent system. CouchDB uses Multi-Version Concurrency Control (MVCC) to avoid locking the database file during writes. Conflicts are left to the application to resolve at write time. Older document versions (called revisions) may be lost when the append-only database file is compacted.
			<ul>
			  <li>[[Eventual Consistency|http://guide.couchdb.org/draft/consistency.html]]</li>
			</ul>			
	 </td>
    </tr>
        <td>Concurrency</td>
        <td> In Riak, any node in the cluster can coordinate a read/write operation for any other node. Riak stresses availability for writes and reads, and puts the burden of resolution on the client at read time.
		 </td>
		
        <td>Because of CouchDB's append-only value mutation, individual instances will not lock. When distributed, CouchDB won't allow updating similarly keyed document without a preceding version number, and conflicts must be manually resolved before concluding a write.
	
			<ul>
				<li>[[No Locking|http://guide.couchdb.org/draft/consistency.html#locking]]</li>
				<li>[[Conflict Management|http://guide.couchdb.org/draft/conflicts.html]]</li>
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
					<li>[[Reading, Writing, and Updating Data|Concepts#Reading%2C-Writing%2C-and-Updating-Data]]</li>
				</ul>	
			
	 </td>
        <td>CouchDB incrementally replicates document changes between nodes. It can be deployed with master/master or master/slave replication. Replication can be finely controlled by way of replication filters.

			<ul>
			<li>[[Replication|http://wiki.apache.org/couchdb/Replication]]</li>
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
        <td>Out of the box, CouchDB is focused on a master-master replication of values (using MVCC to help with conflict resolution). There are external projects that help manage a CouchDB cluster, such as BigCouch (also Apache 2.0 licensed), that shards values across multiple nodes.

			<ul>
			  	<li>[[BigCouch|http://bigcouch.cloudant.com/]]</li>
				<li>[[Sharding (on Wikipedia)|http://en.wikipedia.org/wiki/Sharding]]</li>
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
        <td>CouchDB can be configured to run in multiple datacenters. Robust awareness will generally require a third part solution, or by developing replication filters.
		
			<ul>
			<li>[[Filtered Replication|http://wiki.apache.org/couchdb/Replication#Filtered_Replication]]</li>
			<li>[[The Split Brain|http://guide.couchdb.org/draft/conflicts.html#brain]]</li>
			</ul>
	
	</td>
    </tr>
    <tr>
        <td>Graphical Monitoring/Admin Console</td>
        <td>Starting with Riak 1.1.x, Riak ships with Riak Control, an open source graphical console for monitoring and managing Riak clusters.
			<ul>
				<li>[[Riak Control]]</li>
				<li>[[Introducing Riak Control|http://basho.com/blog/technical/2012/02/22/Riak-Control/]]
			</ul>
	</td>
        <td>CouchDB ships with a graphical interface called Futon.
			
			<ul>
				<li>[[Welcome to Futon|http://guide.couchdb.org/draft/tour.html#welcome]]</li>
			</ul>
	 </td>
    </tr>
</table>
