---
title: Riak Compared to Cassandra
project: riak
version: 1.1.0+
document: appendix
toc: true
index: true
keywords: [comparisons, cassandra]
moved: {
  '1.4.0-': '/references/appendices/comparisons/Riak-Compared-to-Cassandra/'
}
---

This is intended to be a brief, objective and technical comparison of Riak and Cassandra.  The Cassandra version described is 1.2.x. The Riak version described is Riak 1.2.x. If you feel this comparison is unfaithful at all for whatever reason, please [fix it](https://github.com/basho/basho_docs/issues/new) or send an email to **docs@basho.com**.


## At A Very High Level

* Both Riak and Cassandra are Apache 2.0 licensed databases based on Amazon’s Dynamo paper.
* Riak is a faithful implementation of Dynamo, with the addition of functionality like links, MapReduce, indexes, full-text Search. Cassandra departs from the Dynamo paper slightly by omitting vector clocks and moving from partition-based consistent hashing to key ranges, while adding functionality like order-preserving partitioners and range queries.
* Riak is written primarily in Erlang with some bits in C. Cassandra is written in Java.

## Feature/Capability Comparison

The table below gives a high level comparison of Riak and Cassandra features/capabilities.  To keep this page relevant in the face of rapid development on both sides, low level details are found in links to Riak and Cassandra online documentation.

<table>
    <tr>
        <th WIDTH="15%">Feature/Capability</th>
        <th WIDTH="42%">Riak</th>
        <th WIDTH="43%">Cassandra</th>
    </tr>
    <tr>
        <td>Data Model</td>
        <td>Riak stores key/value pairs in a higher level namespace called a bucket.
            <ul>
              <li>[[Buckets, Keys, and Values|Concepts#Buckets-Keys-and-Values]] </li>
            </ul>
        </td>
        <td>Cassandra's data model resembles column storage, and consists of Keyspaces, Column Families, and several other parameters.
            <ul>
              <li>[[Cassandra Data Model|http://www.datastax.com/docs/0.7/data_model/index]] </li>
            </ul>
        </td>
    </tr>
    <tr>
        <td>Storage Model</td>
        <td>Riak has a modular, extensible local storage system which lets you plug-in a backend store of your choice to suit your use case. The default backend is Bitcask.
            <ul>
              <li>[[Riak Supported Storage Backends|Choosing a Backend]]</li>
            </ul>

        You can also write you own storage backend for Riak using our [[backend API|Backend API]].
     </td>
        <td> Cassandra's write path starts with a write to a commit log followed by a subsequent write to an in-memory structure called a memtable. Writes are then batched to a persistent table structure called a sorted string table (SST).
            <ul>
              <li><a href="http://wiki.apache.org/cassandra/ArchitectureCommitLog">Commit Log</a></li>
            <li><a href="http://wiki.apache.org/cassandra/MemtableSSTable">Memtable</a></li>
            <li><a href="http://wiki.apache.org/cassandra/ArchitectureSSTable">SSTable Overview</a></li>
            <li><a href="http://www.datastax.com/docs/1.1/dml/about_writes">About Writes</a></li>
            <li><a href="http://www.datastax.com/docs/1.1/dml/about_reads">About Reads</a></li>
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
              <li>[[Community Developed Libraries and Projects|Community-Developed-Libraries-and-Projects]] </li>
            </ul>
            </td>
        <td>Cassandra provides various access methods including a Thrift API, CQL (Cassandra Query Language), and CLI.
            <ul>
              <li><a href="http://www.datastax.com/docs/1.1/dml/about_clients">Cassandra Client APIs</a></li>
            </ul>
     </td>
    </tr>
    <tr>
        <td>Query Types and Query-ability</td>
        <td>There are currently four ways to query data in Riak
            <ul>
            <li>Primary key operations (GET, PUT, DELETE, UPDATE)</li>
            <li>[[MapReduce|Using MapReduce]]</li>
            <li>[[Secondary Indexes]]</li>
            <li>[[Riak Search]]</li>
      <li>[[Comparing MapReduce, Search, and Secondary Indexes|Querying Riak]]</li>
            </ul>

    </td>
        <td>Cassandra offers various ways to query data:
                <ul>
                <li><a href="http://www.datastax.com/docs/0.7/data_model/keyspaces">Keyspaces</a></li>
                <li><a href="http://www.datastax.com/docs/0.7/data_model/cfs_as_indexes">Column Family Operations</a></li>
                <li><a href="http://www.datastax.com/docs/1.0/dml/using_cql">CQL</a></li>
                <li><a href="http://www.datastax.com/docs/0.7/data_model/secondary_indexes">Secondary Indexes</a></li>
                <li><a href="http://wiki.apache.org/cassandra/HadoopSupport#ClusterConfig">Hadoop Support</a></li>
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

        <td>Cassandra uses timestamps at the column family level to determine the most-recent value when doing read requests. There is no built-in way to do versioning of data.
            <ul>
              <li>[[About Read Consistency|http://www.datastax.com/docs/1.1/dml/data_consistency#about-read-consistency]]</li>
            </ul>
     </td>
    </tr>
        </tr>
            <td>Concurrency</td>
            <td>In Riak, any node in the cluster can coordinate a read/write operation for any other node. Riak stresses availability for writes and reads, and puts the burden of resolution on the client at read time.
            </td>

            <td>All nodes in Cassandra are peers. A client read or write request can go to any node in the cluster. When a client connects to a node and issues a read or write request, that node serves as the coordinator for that particular client operation.
                <ul>
                    <li>[[About Client Requests|http://www.datastax.com/docs/1.0/cluster_architecture/about_client_requests]]
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
        <td>Replication in Cassandra starts when a user chooses a partitioner. Partitioners include Random Partitioner (which also relies on consistent hashing for data storage) and various Ordered Partitioner options. Under the hood, physical nodes are assigned tokens which determine a nodes's position on the ring and the range of data for which it's responsible.
            <ul>
              <li>[[Replication|http://www.datastax.com/docs/1.0/cluster_architecture/replication]]</li>
            </ul>

            Like in Riak, Cassandra lets developers configure the consistency and availability requirements at the request level via various APIs.
            <ul>
                <li><a href="http://www.datastax.com/docs/1.1/dml/data_consistency#tunable-consistency">Tunable Consistency</a>
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

        <td>Cassandra allows you to add new nodes dynamically with the exception of manually calculating a node's token (though users can elect to let Cassandra calculate this). It's recommended that you double the size of your cluster to add capacity. If this isn't feasible, you can elect to either add a number of nodes (which requires token recalculation for all existing nodes), or to add one node at a time, which means leaving the initial token blank and "will probably not result in a perfectly balanced ring but it will alleviate hot spots".
            <ul>
              <li>[[Adding Capacity to an Existing Cluster|http://www.datastax.com/docs/1.1/operations/cluster_management#adding-capacity-to-an-existing-cluster]]</li>
            </ul>
    </td>
    </tr>
    <tr>
        <td>Multi-Datacenter Replication</td>

        <td>Riak features two distinct types of replication. Users can replicate to any number of nodes in one cluster (which is usually contained within one datacenter over a LAN) using the Apache 2.0 database. Riak Enterprise, Basho's commercial extension to Riak, is required for Multi-Datacenter deployments (meaning the ability to run active Riak clusters in N datacenters).

        <ul>
            <li><a href="http://basho.com/products/riak-enterprise/">Riak Enterprise</a></li>
        <ul>

        <td>Cassandra has the ability to spread nodes over multiple datacenters via various configuration parameters.
            <ul>
              <li>[[Multiple Datacenters|http://www.datastax.com/docs/1.1/initialize/cluster_init_multi_dc]]</li>
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
        <td>Datastax distributes the DataStax OpsCenter, a graphical user interface for monitoring and administering Cassandra clusters. This includes a free version available for production use, as well as a for-pay version with additional features.
            <ul>
                <li>[[DataStax OpsCenter|http://www.datastax.com/products/opscenter]]</li>
            </ul>
     </td>
    </tr>
</table>
