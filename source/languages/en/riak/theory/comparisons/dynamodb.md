---
title: Riak Compared to DynamoDB
project: riak
version: 1.3.0+
document: appendix
toc: true
index: true
keywords: [comparisons, dynamodb]
moved: {
  '1.4.0-': '/references/appendices/comparisons/Riak-Compared-to-DynamoDB'
}
---

This is intended to be a brief, objective, and technical comparison of Riak and Amazon DynamoDB.  The DynamoDB version described is API Version 2011-12-05. The Riak version described is Riak 1.3.x. If you feel this comparison is unfaithful for whatever reason, please [fix it](https://github.com/basho/basho_docs/issues/new) or send an email to **docs@basho.com**.

## At A Very High Level

* Riak is an Apache 2.0 open-source-licensed project. DynamoDB is a fully managed NoSQL database service that is provided by Amazon as part of Amazon Web Services.
* Because DynamoDB is a database service, its implementation details (language, architecture, etc.) cannot be verified.

## Feature/Capability Comparison

The table below gives a high-level comparison of Riak and DynamoDB's features and capabilities. To keep this page relevant in the face of rapid development on both sides, low-level details can be found in links to specific pages in the online documentation for both systems.

<table>
    <tr>
        <th WIDTH="15%">Feature/Capability</th>
        <th WIDTH="42%">Riak</th>
        <th WIDTH="43%">DynamoDB</th>
    </tr>
    <tr>
        <td>Data Model</td>
        <td>Riak stores key/value pairs under [[keys|Keys and Objects]] in [[buckets]]. [[Using bucket types]] you can set bucket-level configurations for things like [[replication properties]]. In addition to basic [[key/value lookup|Key/Value Modeling]], Riak has a variety of features for discovering objects, including [[Riak Search|Using Search]] and [[secondary indexes|Using Secondary Indexes]].</td>
        <td>DynamoDB's data model contains tables, items, and attributes. A database is a collection of tables. A table is a collection of items and each item is a collection of attributes.
            <ul>
              <li>[[DynamoDB Data Model|http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html]]</li>
            </ul>
        </td>
    </tr>
    <tr>
        <td>Storage Model</td>
        <td>Riak has a modular, extensible local storage system that lets you plug in a backend store of your choice to suit your use case. The default backend is [[Bitcask]].
            <ul>
              <li>[[Riak Supported Storage Backends|Choosing a Backend]]</li>
            </ul>

        You can also write you own storage backend for Riak using our [[backend API|Backend API]].
     </td>
        <td>All data items are stored on Solid State Disks (SSDs) and replicated across multiple [[availability zones|http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html]] within a [[region|http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html]].
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
        <td>DynamoDB is a web service that uses HTTP as a transport and JavaScript Object Notation (JSON) as a message serialization format. Alternatively, you can use AWS SDKs that wrap the DynamoDB API calls.
            <ul>
              <li>[[API Reference for DynamoDB|http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API.html]]</li>
        <li>[[Using the AWS SDKs with DynamoDB|http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/UsingAWSSDK.html]]</li>
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
        <td>DynamoDB offers three approaches to quering data:
                <ul>
          <li>Primary key operations (GET, PUT, DELETE, UPDATE)</li>
          <li>[[Query|http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/queryingdynamodb.html]]</li>
          <li>[[Scan|http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/scandynamodb.html]]</li>
          <li>[[Local Secondary Indexes|http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LSI.html]]</li>
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

        <td>DynamoDB data is eventually consistent, meaning that your read request immediately after a write operation might not show the latest change. However, it also offers you the option to request the most up-to-date version of the data.
            <ul>
              <li>[[Data Read and Consistency Considerations|http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/APISummary.html]]</li>
            </ul>
     </td>
    </tr>
        </tr>
            <td>Concurrency</td>
            <td>In Riak, any node in the cluster can coordinate a read/write operation for any other node. Riak stresses availability for writes and reads, and puts the burden of resolution on the client at read time.
            </td>

            <td>Dedicated resources are allocated to your table (tunable via API) to meet performance requirements, and data is automatically partitioned over a number of servers to meet request capacity.
                <ul>
                    <li>[[Provisioned Throughput|http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html]]
                </ul>
        Read and write capacity unit requirements are set at table creation time. When requests such as get, update or delete are issued, capacity units set for the table are consumed.

        <ul>
          <li>[[Capacity Units Calculations|http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithDDTables.html#CapacityUnitCalculations]]</li>
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
        <td>DynamoDB synchronously replicates your data across multiple [[Availability Zones|http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html]] within a [[Region|http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html]] to help protect data against individual machine or facility failures.
     </td>
    </tr>
    <tr>
        <td>Scaling Out and In</td>
        <td>Riak allows you to elastically grow and shrink your cluster while evenly balancing the load on each machine. No node in Riak is special or has any particular role. In other words, all nodes are masterless. When you add a physical machine to Riak, the cluster is made aware of its membership via gossiping of ring state. Once it's a member of the ring, it's assigned an equal percentage of the partitions and subsequently takes ownership of the data belonging to those partitions. The process for removing a machine is the inverse of this. Riak also ships with a comprehensive suite of command line tools to help make node operations simple and straightforward.

         <ul>
          <li>[[Adding and Removing Nodes]]</li>
          <li>[[Command Line Tools]]</li>
        </ul>

        <td>DynamoDB requires that you specify your required read and write throughput values when you create a table – throughput values can be increased and decreased later as access requirements change. This is used to reserve sufficient hardware resources and appropriately partitions your data over multiple servers to meet your throughput requirements.
          <ul>
            <li>[[Provisioned Throughput|http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html]]
            </ul>
          </td>
    </tr>
    <tr>
        <td>Multi-Datacenter Replication</td>

        <td>Riak features two distinct types of replication. Users can replicate to any number of nodes in one cluster (which is usually contained within one datacenter over a LAN) using the Apache 2.0 database. Riak Enterprise, Basho's commercial extension to Riak, is required for Multi-Datacenter deployments (meaning the ability to run active Riak clusters in N datacenters).

        <ul>
            <li><a href="http://basho.com/products/riak-enterprise/">Riak Enterprise</a></li>
        <ul>

        <td>DynamoDB has the ability to spread instances over multiple [[Availability Zones|http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html]] within a Region, but not across multiple [[Regions|http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html]]. Availability Zones are not geographically dispersed.
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
        <td>DynamoDB and [[CloudWatch|http://aws.amazon.com/cloudwatch/]] are integrated, which allows you to monitor a variety of metrics.
            <ul>
                <li>[[Monitoring Amazon DynamoDB|http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/MonitoringDynamoDB.html]]</li>
            </ul>
     </td>
    </tr>
</table>
