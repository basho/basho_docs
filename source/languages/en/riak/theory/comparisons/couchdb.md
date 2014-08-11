---
title: Riak Compared to CouchDB
project: riak
version: 1.1.0+
document: appendix
toc: true
index: true
keywords: [comparisons, couchdb]
moved: {
  '1.4.0-': '/references/appendices/comparisons/Riak-Compared-to-CouchDB'
}
---

This is intended to be a brief, objective, and technical comparison of
Riak and CouchDB.  The CouchDB version described is 1.2.x. The Riak
version described is Riak 2.x. If you feel this comparison is unfaithful
for whatever reason, please [submit an issue](https://github.com/basho/basho_docs/issues/new)
or send an email to **docs@basho.com**.

## At A Very High Level

* Riak and CouchDB are both [Apache 2.0](http://www.apache.org/licenses/LICENSE-2.0.html) licensed
* Riak is written primarily in Erlang with some bits in C; CouchDB is written in Erlang

## Feature/Capability Comparison

The table below gives a high level comparison of Riak and CouchDB
features/capabilities. To keep this page relevant in the face of rapid
development on both sides, low level details are found in links to
the online documentation for both [Riak](http://docs.basho.com/) and
[CouchDB](https://couchdb.readthedocs.org/en/latest/).

<table>
    <tr>

        <th WIDTH="15%">Feature/Capability</th>
        <th WIDTH="42%">Riak</th>
        <th WIDTH="43%">CouchDB</th>
    </tr>
    <tr>
        <td><strong>Data Model</strong></td>
        <td>Riak stores key/value pairs under [[keys|Keys and Objects]] in [[buckets]]. [[Using bucket types]] you can set bucket-level configurations for things like [[replication properties]]. In addition to basic [[key/value lookup|Key/Value Modeling]], Riak has a variety of features for discovering objects, including [[Riak Search|Using Search]] and [[secondary indexes|Using Secondary Indexes]].</td>
        <td>CouchDB's data format is JSON stored as documents (self-contained records with no intrinsic relationships), grouped into "database" namespaces.
            <ul>
                <li>[[Document API|http://wiki.apache.org/couchdb/HTTP_Document_API]]</li>
            </ul>
        </td>
    </tr>
    <tr>
        <td><strong>Storage Model</strong></td>
        <td>Riak has a modular, extensible local storage system that lets you plug in a backend store of your choice to suit your use case. The default backend is [[Bitcask]].
            <ul>
              <li>[[Riak Supported Storage Backends|Choosing a Backend]]</li>
            </ul>

        You can also write your own storage backend for Riak using our [[backend API|Backend API]].
        </td>
        <td>CouchDB stores data to disk in "append-only" files. As the files continue to grow, they require occasional compaction.
            <ul>
             <li>[[Indexes and File|http://guide.couchdb.org/draft/btree.html]]</li>
            </ul>
        </td>
    </tr>
    <tr>
        <td><strong>Data Access and APIs</strong></td>
        <td>Riak offers two primary interfaces (in addition to raw Erlang access):
            <ul>
            <li>[[Protocol Buffers|PBC API]] (strongly recommended)</li>
            <li>[[HTTP|HTTP API]]</li>
            </ul>
            Riak [[client libraries]] are wrappers around these APIs, and client support exists for dozens of languages. Basho currently has officially supported clients for [[Java|https://github.com/basho/riak-java-client]], [[Ruby|https://github.com/basho/riak-ruby-client]], [[Python|https://github.com/basho/riak-python-client]], and [[Erlang|https://github.com/basho/riak-erlang-client]].
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
        <td><strong>Query Types and Queryability</strong></td>
        <td>There are currently five ways to query data in Riak:
            <ul>
            <li>Via [[primary key operations|The Basics]] (GET, PUT, DELETE, UPDATE)</li>
            <li>[[Using MapReduce]]</li>
            <li>[[Using secondary indexes]]</li>
            <li>[[Using Search]]</li>
            <li>[[Using Data Types]]</li>
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
        <td><strong>Data Versioning and Consistency</strong></td>
        <td>Riak uses a data structure called a [[vector clock|Vector Clocks]] to reason about causality and staleness of stored values. Vector clocks enable clients to always write to the database in exchange for consistency conflicts being resolved either at read time by application or client code or by Riak's [[active anti-entropy]] subsystem. Vector clocks can be configured to store copies of a given object based on the size and age of that object. There is also an option to disable vector clocks and fall back to simple timestamp-based resolution, known as [[last write wins|Conflict Resolution#Client-and-Server-side-Conflict-Resolution]].

        <ul>
            <li>[[Why Vector Clocks Are Easy|http://basho.com/blog/technical/2010/01/29/why-vector-clocks-are-easy/]]</li>
            <li>[[Why Vector Clocks Are Hard|http://basho.com/blog/technical/2010/04/05/why-vector-clocks-are-hard/]]</li>
        </ul>
        
        In addition, as of version 2.0 you can use Riak in a [[strongly consistent|Strong Consistency]] fashion.
        </td>

        <td>CouchDB replicates newer document versions between nodes, making it an eventually consistent system. CouchDB uses Multi-Version Concurrency Control (MVCC) to avoid locking the database file during writes. Conflicts are left to the application to resolve at write time. Older document versions (called revisions) may be lost when the append-only database file is compacted.
            <ul>
              <li>[[Eventual Consistency|http://guide.couchdb.org/draft/consistency.html]]</li>
            </ul>
     </td>
    </tr>
        <td><strong>Concurrency</strong></td>
        <td>In Riak, any node in the cluster can coordinate a read/write operation for any other node. Riak stresses availability for writes and reads, and puts the burden of resolution on the client at read time.
         </td>

        <td>Because of CouchDB's append-only value mutation, individual instances will not lock. When distributed, CouchDB won't allow updating similarly keyed document without a preceding version number, and conflicts must be manually resolved before concluding a write.

            <ul>
                <li>[[No Locking|http://guide.couchdb.org/draft/consistency.html#locking]]</li>
                <li>[[Conflict Management|http://guide.couchdb.org/draft/conflicts.html]]</li>
            </ul>
     </td>
    </tr>
    <tr>
        <td><strong>Replication</strong></td>
        <td>Riak's replication system is heavily influenced by the Dynamo Paper and Dr. Eric Brewer's CAP Theorem. Riak uses consistent hashing to replicate and distribute N copies of each value around a Riak cluster composed of any number of physical machines. Under the hood, Riak uses virtual nodes to handle the distribution and dynamic rebalancing of data, thus decoupling the data distribution from physical assets.
            <ul>
              <li>[[Replication]]</li>
              <li>[[Clustering|Clusters]]</li>
            </ul>

            The Riak APIs expose tunable consistency and availability parameters that let you select which level of configuration is best for your use case. Replication is configurable at the bucket level when first storing data in Riak. Subsequent reads and writes to that data can have request-level parameters.
                <ul>
                    <li>[[Reading, Writing, and Updating Data|Concepts#Reading, Writing, and Updating Data]]</li>
                </ul>
     </td>
        <td>CouchDB incrementally replicates document changes between nodes. It can be deployed with master/master or master/slave replication. Replication can be finely controlled by way of replication filters.

            <ul>
            <li>[[Replication|http://wiki.apache.org/couchdb/Replication]]</li>
            </ul>
     </td>
    </tr>
    <tr>
        <td><strong>Scaling Out and In</strong></td>
        <td>Riak allows you to [[elastically grow and shrink|Adding and Removing Nodes]] your cluster while evenly balancing the load on each machine. No node in Riak is special or has any particular role. In other words, all nodes are masterless. When you add a physical machine to Riak, the cluster is made aware of its membership via gossiping of [[ring state|Clusters#the-ring]]. Once it's a member of the ring, it's assigned an equal percentage of the partitions and subsequently takes ownership of the data belonging to those partitions. The process for removing a machine is the inverse of this. Riak also ships with a comprehensive suite of [[command line tools|riak-admin Command Line]] to help make node operations simple and straightforward.
        </td>
        <td>Out of the box, CouchDB is focused on a master-master replication of values (using MVCC to help with conflict resolution). There are external projects that help manage a CouchDB cluster, such as BigCouch (also Apache 2.0 licensed), that shards values across multiple nodes.

            <ul>
                <li>[[BigCouch|http://bigcouch.cloudant.com/]]</li>
                <li>[[Sharding (on Wikipedia)|http://en.wikipedia.org/wiki/Sharding]]</li>
            </ul>
    </td>
    </tr>
    <tr>
        <td><strong>Multi-Datacenter Replication and Awareness</strong></td>

        <td>Riak features two distinct types of [[replication]]. Users can replicate to any number of nodes in one cluster (which is usually contained within one datacenter over a LAN) using the Apache 2.0-licensed database. Riak Enterprise, Basho's commercial extension to Riak, is required for Multi-Datacenter deployments (meaning the ability to run active Riak clusters in N datacenters).

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
        <td><strong>Graphical Monitoring/Admin Console</strong></td>
        <td>Riak ships with [[Riak Control]], an open source graphical console for monitoring and managing Riak clusters.</td>
        <td>CouchDB ships with a graphical interface called Futon.

            <ul>
                <li>[[Welcome to Futon|http://guide.couchdb.org/draft/tour.html#welcome]]</li>
            </ul>
     </td>
    </tr>
</table>
