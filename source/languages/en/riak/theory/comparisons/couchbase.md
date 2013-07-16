---
title: Riak Compared to Couchbase
project: riak
version: 1.1.0+
document: appendix
toc: true
index: true
keywords: [comparisons, couchbase]
moved: {
  '1.4.0-': '/references/appendices/comparisons/Riak-Compared-to-Couchbase'
}
---

This is intended to be a brief, objective and technical comparison of Riak and Couchbase (i.e. Couchbase Server).  The Couchbase version described is 2.0. The Riak version described is Riak 1.2.x. If you feel this comparison is unfaithful at all for whatever reason, please [fix it](https://github.com/basho/basho_docs/issues/new) or send an email to **docs@basho.com**.

## At A Very High Level

* Riak is Apache 2.0 licensed; According to Couchbase, they have two free versions: Couchbase open source is Apache 2.0 licensed; Couchbase Server Community Edition (free version) is licensed under a [community agreement](http://www.couchbase.com/agreement/community)
* Riak is written primarily in Erlang with some bits in C; Couchbase is written in Erlang and C/C++

<div class="note"><div class="title">Couchbase vs CouchDB</div>Keep in mind that Couchbase and CouchDB are two separate database projects.  CouchDB is a document database providing replication, MapReduce and an HTTP API.  Couchbase uses CouchDB as its backend, "wrapping" it with advanced features like caching, and is designed to be clustered.</div>

<div class="note"><div class="title">Couchbase 2.0</div>As of the time of this writing, Couchbase 2.0 is still in developer preview, so some of these points may change between now and the final release. <i>Caveat emptor</i></div>

## Feature/Capability Comparison

The table below gives a high level comparison of Riak and Couchbase features/capabilities.  To keep this page relevant in the face of rapid development on both sides, low level details are found in links to Riak and Couchbase online documentation.

<table>
    <tr>

        <th WIDTH="15%">Feature/Capability</th>
        <th WIDTH="42%">Riak</th>
        <th WIDTH="43%">Couchbase</th>
    </tr>
    <tr>
        <td>Data Model</td>
        <td>Riak stores key/value pairs in a higher level namespace called a bucket.
            <ul>
              <li>[[Buckets, Keys, and Values|Concepts#Buckets-Keys-and-Values]] </li>
            </ul>
        </td>
        <td>Couchbase is a JSON-based document datastore. Like other document datastores, records have no intrinsic relationships, and are stored in buckets. Value size is limited to 20Mbyte.
            <ul>
                <li>[[How Should I Store an Object?|http://www.couchbase.com/docs/couchbase-manual-2.0/couchbase-developing-bestpractices-objectstorage-how.html]]</li>
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
        <td>Couchbase 2.0 is largely memory-based, asynchronously persisting data using a CouchDB fork and C library "couchstore" (prior versions of Couchbase use the SQLite storage engine).
            <ul>
            <li>[[Persistence|http://www.couchbase.com/docs/couchbase-manual-2.0/couchbase-architecture-persistencedesign.html]]</li>
            <li>[[Couchbase File Format|https://github.com/couchbaselabs/couchstore/wiki/Format]]</li>
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
        <td>Couchbase provides drivers in several languages to access data through its binary memcached protocol. Couchbase also provides a REST API to monitor and manage a cluster (though it is not used to directly manage stored data).
            <ul>
                <li>[[Client Interface|http://www.couchbase.com/docs/couchbase-manual-2.0/couchbase-introduction-architecture-clientinterface.html]]</li>
                <li>[[Client-Libraries|http://www.couchbase.com/develop]]</li>
                <li>[[Management REST API|http://www.couchbase.com/docs/couchbase-manual-2.0/couchbase-admin-restapi.html]]</li>
            </ul>
     </td>
    </tr>
    <tr>
        <td>Query Types and Query-ability</td>
        <td>There are currently four ways to query data in Riak
            <ul>
            <li>Primary key operations (GET, PUT, DELETE, UPDATE)</li>
            <li>[[Using MapReduce]]</li>
            <li>[[Using Secondary Indexes]]</li>
            <li>[[Using Search]]</li>
            </ul>
    </td>
        <td>Couchbase also provides four query options
            <ul>
            <li>[[ID lookups|http://www.couchbase.com/docs/couchbase-manual-2.0/couchbase-developing-bestpractices-multiget.html]]</li>
            <li>[[MapReduce Views|http://www.couchbase.com/docs/couchbase-manual-2.0/couchbase-views-basics.html]]</li>
            <li>[[UnQL|http://www.couchbase.com/press-releases/unql-query-language]]</li>
            </ul>
            Hadoop support is also possible through a plugin that streams data to a Hadoop Distributed File System (HDFS) or Hive for processing.
            <ul>
            <li>[[Hadoop Connector|http://www.couchbase.com/develop/connectors/hadoop]]</li>
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

        <td>Couchbase is strongly consistent within a datacenter, replicating data between nodes in a cluster for failover. Inter-datacenter replication follows an eventually consistent CouchDB replication model.

            Via CouchDB, documents are internally revisioned (stored in a "_rev" value). However, prior revisions will be removed on a file compaction operation, making them unreliable.

        <ul>
        <li>[[Couchbase Architecture|http://www.couchbase.com/docs/couchbase-manual-2.0/couchbase-architecture.html]]</li>
        <li>[[Internal Version Field|http://www.couchbase.com/docs/couchbase-manual-2.0/couchbase-views-datastore-fields.html]]</li>
        </ul>
    </tr>
        <td>Concurrency</td>
        <td> In Riak, any node in the cluster can coordinate a read/write operation for any other node. Riak stresses availability for writes and reads, and puts the burden of resolution on the client at read time.
         </td>

        <td>Couchbase claims to be ACID-compliant on a per-item basis, but has no multi-operation transactions. Couchbase clients connect to a server list (or via a proxy) where keys are sharded across the nodes. Couchbase nodes inherit memcached's default (and recommended) connection limit of 10k.

        <ul>
        <li>[[Transaction and concurrency|http://www.couchbase.com/forums/thread/transaction-and-concurency]]</li>
        <li>[[Cluster Design|http://www.couchbase.com/docs/couchbase-manual-2.0/couchbase-architecture-clusterdesign.html]]</li>
        <li>[[Client-side Proxy|http://www.couchbase.com/docs/couchbase-manual-2.0/couchbase-deployment-standaloneproxy.html]]</li>
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
     <td>Couchbase supports two types of replication.  For intra-datacenter clusters, Couchbase uses membase-style replication, which favors immediate consistency in the face of a network partition.  For multi-datacenter deployments, CouchDB's master-master replication is used.

            <ul>
            <li>[[CouchDB Replication|http://wiki.apache.org/couchdb/Replication]]</li>
            <li>[[Memcache Tap|http://code.google.com/p/memcached/wiki/Tap]]</li>
            <li>[[CouchDB, Couchbase, Membase|http://www.infoq.com/news/2012/05/couchdb-vs-couchbase-membase]]</li>

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
        <td>Couchbase scales elastically by auto-sharding. They can be rebalanced to grow or shrink through the administrative interface.

        <ul>
        <li>[[Rebalancing|http://www.couchbase.com/docs/couchbase-manual-2.0/couchbase-admin-tasks-addremove.html]]</li>
        <li>[[Clone to Grow with Auto Sharding|http://www.couchbase.com/couchbase-server/features#clone_to_grow]]</li>
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
        <td>Couchbase 2.0 supports cross-datacenter replication (XDCR).

        <ul>
        <li>[[Stabilizing Couchbase Server 2.0|http://blog.couchbase.com/stabilizing-couchbase-server-2-dot-0]]</li>
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
        <td>Couchbase provides a web-based monitoring/admin console.
            <ul>
                <li>[[Admin Wed Console|http://www.couchbase.com/docs/couchbase-manual-2.0/couchbase-admin-web-console.html]]</li>
                <li>[[Monitoring Couchbase|http://www.couchbase.com/docs/couchbase-manual-2.0/couchbase-monitoring.html]]</li>
            </ul>

     </td>
    </tr>
</table>
