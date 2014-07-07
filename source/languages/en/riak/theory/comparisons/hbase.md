---
title: Riak Compared to HBase
project: riak
version: 1.1.0+
document: appendix
toc: true
index: true
keywords: [comparisons, hbase]
moved: {
  '1.4.0-': '/references/appendices/comparisons/Riak-Compared-to-HBase'
}
---

This is intended to be a brief, objective and technical comparison of
Riak and HBase. The HBase version described is 0.94.x. The Riak version
described is Riak 1.2.x. If you feel this comparison is unfaithful for
whatever reason, please [submit an issue](https://github.com/basho/basho_docs/issues/new)
or send an email to **docs@basho.com**.

## At A Very High Level

* Riak and HBase are both [Apache 2.0](http://www.apache.org/licenses/LICENSE-2.0.html) licensed
* Riak is based on Amazon's [Dynamo paper](http://docs.basho.com/riak/latest/theory/dynamo/); HBase is based on Google's [BigTable](http://research.google.com/archive/bigtable.html)
* Riak is written primarily in Erlang with some C; HBase is written in Java

## Feature/Capability Comparison

The table below gives a high level comparison of Riak and HBase features
and capabilities. To keep this page relevant in the face of rapid
development on both sides, low level details are found in links to the
online documentation for [Riak](http://docs.basho.com/) and
[HBase](http://hbase.apache.org/book.html).

<table>
    <tr>
        <th WIDTH="15%">Feature/Capability</th>
        <th WIDTH="42%">Riak</th>
        <th WIDTH="43%">HBase</th>
    </tr>
    <tr>
        <td><strong>Data Model</strong></td>
        <td>Riak stores key/value pairs under [[keys|Keys and Objects]] in [[buckets]]. [[Using bucket types]] you can set bucket-level configurations for things like [[replication properties]]. In addition to basic [[key/value lookup|Key/Value Modeling]], Riak has a variety of features for discovering objects, including [[Riak Search|Using Search]] and [[secondary indexes|Using Secondary Indexes]].</td>
        <td>HBase stores data in a pre-defined column family format (each grouping of data has a key, and any number of column attributes which may be versioned individually). Data in HBase is sorted, sparse, and physically grouped by column family (rather than by row, as in a relational database). HBase calls their groupings "tables."
            <ul>
                <li>[[HBase Data Model|http://hbase.apache.org/book/datamodel.html]]</li>
                <li>[[Supported Data Types|http://hbase.apache.org/book/supported.datatypes.html]]</li>
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
        <td>Hadoop Distributed File System (HDFS) is the storage system used by HBase. Data is stored in MemStores and StoreFiles, where data is streamed to disk (implemented via HFiles, a format based on BigTable's SSTable). Implementations generally use the native JVM-managed I/O file stream.
            <ul>
             <li>[[HDFS|http://en.wikipedia.org/wiki/Apache_Hadoop#Hadoop_Distributed_File_System]]</li>
             <li>[[Hadoop Uses HDFS|http://hbase.apache.org/book/arch.hdfs.html]]</li>
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
        <td>HBase communicates primarily through code that runs on the JVM ([[Java|http://hbase.apache.org/book/architecture.html]], Jython, Groovy, etc.). Alternatively, HBase provides external protocols for [REST]() or [Thrift](https://thrift.apache.org/) (a cross-language data service format).
        </td>
    </tr>
    <tr>
        <td><strong>Query Types and Queryability</strong></td>
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
        <td>HBase has two query options: looking up values by getting/scanning through ordered keys (optionally filtering out values or using a secondary index), or by using Hadoop to perform MapReduce.
            <ul>
                <li>[[Scanning|http://hbase.apache.org/book/client.filter.html]]</li>
                <li>[[MapReduce|http://hbase.apache.org/book/mapreduce.html]]</li>
                <li>[[Secondary Indexes|http://hbase.apache.org/book/secondary.indexes.html]]</li>
            </ul>
    </td>
    </tr>
    <tr>
        <td><strong>Data Versioning and Consistency</strong></td>
        <td>Riak uses a data structure called a vector clock to reason about causality and staleness of stored values. Vector clocks enable clients to always write to the database in exchange for consistency conflicts being resolved at read time by either application or client code. Vector clocks can be configured to store copies of a given datum based on size and age of said datum. There is also an option to disable vector clocks and fall back to simple time-stamp based "last-write-wins".
            <ul>
              <li>[[Vector Clocks]]</li>
              <li>[[Why Vector Clocks Are Easy|http://basho.com/blog/technical/2010/01/29/why-vector-clocks-are-easy/]]</li>
              <li>[[Why Vector Clocks Are Hard|http://basho.com/blog/technical/2010/04/05/why-vector-clocks-are-hard/]]</li>
            </ul>
         </td>
        <td>HBase has strongly consistent reads/writes. Data may be autosharded across regions and redistributed as data changes.

        Column families may contain an unbounded number of versions, with optional TTL.
            <ul>
                <li>[[Consistent Architecture|http://hbase.apache.org/book/architecture.html#arch.overview.nosql]]</li><li>[[Time to Live|http://hbase.apache.org/book/ttl.html]]</li>
            </ul>
     </td>
    </tr>
        <td><strong>Concurrency</strong></td>
        <td> In Riak, any node in the cluster can coordinate a read/write operation for any other node. Riak stresses availability for writes and reads, and puts the burden of resolution on the client at read time.
         </td>

        <td>HBase guarantees write atomicity and locks per row. HBase has also recently added multi-action and multi-row local transactions (though you cannot mix read/write actions).
            <ul>
                <li>[[Consistency Guarantees|http://hbase.apache.org/acid-semantics.html]]</li>
                <li>[[http://hadoop-hbase.blogspot.com/2012/03/acid-in-hbase.html]]</li>
            </ul>
     </td>
    </tr>
    <tr>
        <td><strong>Replication</strong></td>
        <td>Riak's replication system is heavily influenced by the Dynamo Paper and Dr. Eric Brewer's CAP Theorem. Riak uses consistent hashing to replicate and distribute N copies of each value around a Riak cluster composed of any number of physical machines. Under the hood, Riak uses virtual nodes to handle the distribution and dynamic rebalancing of data, thus decoupling the data distribution from physical assets.
            <ul>
              <li>[[Replication]]</li>
              <li>[[Clustering|Concepts#Clustering]]</li>
            </ul>

            The Riak APIs expose tunable consistency and availability parameters that let you select which level of configuration is best for your use case. Replication is configurable at the bucket level when first storing data in Riak. Subsequent reads and writes to that data can have request-level parameters.
                <ul>
                    <li>[[Reading, Writing, and Updating Data|Concepts#Reading, Writing, and Updating Data]]</li>
                </ul>
     </td>
        <td>HBase supports in-cluster and between-cluster replication. In-cluster replication is handled by HDFS and replicates underlying data files according to Hadoop's settings. Between-cluster replicates by an eventually consistent master/slave push, or more recently added (experimental) master/master and cyclic (where each node plays the role of master and slave) replication.
        <ul>
        <li>[[Replication|http://hbase.apache.org/replication.html]]</li>
        </ul>
     </td>
    </tr>
    <tr>
        <td><strong>Scaling Out and In</strong></td>
        <td>Riak allows you to [[elastically grow and shrink|Adding and Removing Nodes]] your cluster while evenly balancing the load on each machine. No node in Riak is special or has any particular role. In other words, all nodes are masterless. When you add a physical machine to Riak, the cluster is made aware of its membership via gossiping of [[ring state|Clusters#the-ring]]. Once it's a member of the ring, it's assigned an equal percentage of the partitions and subsequently takes ownership of the data belonging to those partitions. The process for removing a machine is the inverse of this. Riak also ships with a comprehensive suite of [[command line tools|riak-admin Command Line]] to help make node operations simple and straightforward.
        </td>
        <td>HBase shards by way or regions, that automatically split and redistribute growing data. A crash on a region requires crash recovery. HBase can be made to scale in with some intervention on the part of the developer or DBA.
            <ul>
                <li>[[Regions|http://hbase.apache.org/book/regions.arch.html]]</li>
                <li>[[Node Management|http://hbase.apache.org/book/node.management.html]]</li>
                <li>[[HBase Architecture|http://hbase.apache.org/book/architecture.html]]</li>
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
        <td>HBase shards by way of regions, that themselves may be replicated across multiple datacenters.
            <ul>
              <li>[[Node Management|http://hbase.apache.org/replication.html]]</li>
            </ul>
    </td>
    </tr>
    <tr>
        <td><strong>Graphical Monitoring/Admin Console</strong></td>
        <td>Riak ships with [[Riak Control]], an open source graphical console for monitoring and managing Riak clusters.</td>
        <td>HBase has a few community supported graphical tools, and a command-line admin console.
        <ul>
        <li>[[Admin Console Tools|http://hbase.apache.org/book/ops_mgt.html#tools]]</li>
        <li>[[Eclipse Dev Plugin|http://wiki.apache.org/hadoop/Hbase/EclipseEnvironment]]</li>
        <li>[[HBase Manager|http://sourceforge.net/projects/hbasemanagergui/]]</li>
        <li>[[GUI Admin|https://github.com/zaharije/hbase-gui-admin]]</li>
        </ul>
     </td>
    </tr>
</table>
