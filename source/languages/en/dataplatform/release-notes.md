---
title: Basho Data Platform 1.0.0 Release Notes
project: dataplatform
version: 1.0.0+
document: guide
audience: beginner
toc: true
keywords: [developers]
---

Released August XY, 2015.

This release is the introductory release of Basho Data Platform (BDP), so everything is new!

##Features

###Service Manager
The service manager is the heart of BDP. It provides the means for building a cluster of nodes that can deploy, run, and manage the platform services. Specifically, the service manager provides the following capabilities:

* Create a cluster of platform nodes that can run services.
* Metadata system that can be used to track installed services, running services, configuration information, etc.
* Gossip/broadcast system responsible for propagating metadata across the cluster.
* Command-line interface for installing new services.
* Command-line interface for user administration (eg. starting/stopping services).
* Allow you to assign ports and maintain per-service clustering information.
* Subsystem responsible for running and monitoring services on relevant nodes, restarting services as necessary.

###Cache Proxy + Redis (Enterprise only)
BDP's cache proxy provides caching as a service. Cache proxy builds upon Twemproxy, allowing BDP to spread cache writes across multiple Redis instances using consistent hashing for presharding, reduces connections to Redis and Riak instances, and extends Twemproxy to support Riak KV as a backend, implementing a read-through cache strategy by retrieving values from Riak KV (including sibling resolution) before writing values to the Redis frontend cache.

###Spark-Riak Connector
The Spark-Riak connector provides both development and runtime performance gains while developing Spark analytics jobs that read and write values from Riak KV.  To aid in development, the Spark-Riak connector automatically serializes the Spark RRD format content to a value.  Performance gains are yielded by utilizing the most optimal Riak KV bucket queries for the types of query patterns that are typical in Spark jobs.

###Leader Election Service (Enterprise only)
BDP's leader election service (LES) enables Spark clusters to run without a ZooKeeper instance by using a simple, line-based, ASCII protocol to interact with Spark. This creates a simplified interface to manage many services under BDP.

###Spark Cluster Manager (Enterprise only)
The Spark cluster manager forms a pair with the leader election service. It enables Spark to use the LES rather than ZooKeeper. Spark cluster manager provides all the functionality required for Spark Master high availability without the need to manage yet another software system. 

##Compatibility

Basho Data Platform is compatible with the following operating systems:

* RHEL/CentOS 6
* RHEL/CentOS 7
* Ubuntu 12.04 LTS
* Ubuntu 14.04 LTS
* Amazon Linux

BDP also supports the following operating systems for development:

* OSX 10.10
* Debian 7

##Installing

You can find the install packages for BDP [here](LINK) and the instructions to walk you through the installation process [here](LINK).

##Known Issues

* The default value of the `storage_backend` parameter in riak.conf file is changed from 'bitcask' to 'leveldb' in BDP, because Spark connector uses 2i queries, which are supported for LevelDB but not for Bitcask.
* Only the bundled versions of Spark, Redis, and Solr are officially supported.
* We strongly recommend against running Riak KV and Spark or Riak KV and Redis on the same nodes. This recommendation might be dropped in future versions. 
* We also strongly recommend against joining a previously established Riak KV cluster to BDP. Please create a Riak KV cluster with BDP, instead. 
*  Occasionally BDP Service Manager might start additional, spurious Spark-Master processes - after Spark is already set up and running. It will not interfere with normal Spark work, Spark jobs will run correctly. However, if it happens, users will see error messages in the BDP logs that the spurious spark-master service is failing and additional spark-master service might show up in the process list for short time. Development team is working on solution, hot-fix will be available soon and provided per request, the permamenent fix in the next BDP minor or patch release.
