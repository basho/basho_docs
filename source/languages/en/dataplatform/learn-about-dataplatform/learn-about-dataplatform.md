---
title: Learn About Basho Data Platform Overview
project: dataplatform
version: 1.0.0+
document: tutorial
index: true
audience: beginner
keywords: [data platform]
body_id: learn-about-dataplatform-index
simple: true
versions: true
---

[using bdp index]: LINK
[cache proxy features]: http://docs.basho.com/dataplatform/1.1.0/learn-about-dataplatform/cache-proxy-features/
[service manager features]: http://docs.basho.com/dataplatform/1.1.0/learn-about-dataplatform/service-manager-features/
[leader election features]: http://docs.basho.com/dataplatform/1.1.0/learn-about-dataplatform/leader-election-service/
[spark manager features]: http://docs.basho.com/dataplatform/1.1.0/learn-about-dataplatform/spark-cluster-manager-features/

##In This Section

The **Learn About Basho Data Platform (BDP)**  section provides overviews of the various parts and concepts behind BDP. If you are looking for information on setup or usage, check out [Using Basho Data Platform][using bdp index].

####Service Manager
The [service manager][service manager features] is the foundation of the Basho Data Platform. It provides a means for building a cluster of nodes that can deploy, run, and manage platform services.

####Cache Proxy
The [cache proxy service][cache proxy features] uses Redis and Riak KV to provide pre-sharding and connection aggregation for your data platform cluster.

####Leader Election
The [leader election service][leader election features] enables Spark clusters to run without a ZooKeeper instance.

####Spark Cluster Manager
The [Spark cluster manager][spark manager features] provides all the functionality required for Spark Master high availability without the need to manage yet another software system (Zookeeper).
