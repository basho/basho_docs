---
title: "Learn About Basho Data Platform"
description: ""
project: "dataplatform"
project_version: "1.0.0"
menu:
  dataplatform-1.0.0:
    name: "Learn"
    identifier: "learn"
    weight: 105
    pre: beaker
toc: false
aliases:
  - /dataplatform/1.0.0/learn-about-dataplatform/learn-about-dataplatform/
---

[using bdp index]: /dataplatform/1.0.0/using/
[cache proxy features]: /dataplatform/1.0.0/learn/cache-proxy/
[service manager features]: /dataplatform/1.0.0/learn/service-manager/
[leader election features]: /dataplatform/1.0.0/learn/leader-election-service/
[spark manager features]: /dataplatform/1.0.0/learn/spark-cluster-manager/

## In This Section

The **Learn About Basho Data Platform (BDP)**  section provides overviews of the various parts and concepts behind BDP. If you are looking for information on setup or usage, check out [Using Basho Data Platform][using bdp index].

#### Service Manager

The [service manager][service manager features] is the foundation of the Basho Data Platform. It provides a means for building a cluster of nodes that can deploy, run, and manage platform services.

#### Cache Proxy

The [cache proxy service][cache proxy features] uses Redis and Riak KV to provide pre-sharding and connection aggregation for your data platform cluster.

#### Leader Election

The [leader election service][leader election features] enables Spark clusters to run without a ZooKeeper instance.

#### Spark Cluster Manager

The [Spark cluster manager][spark manager features] provides all the functionality required for Spark Master high availability without the need to manage yet another software system (Zookeeper).
