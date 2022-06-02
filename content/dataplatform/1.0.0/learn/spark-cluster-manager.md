---
title: "Spark Cluster Manager"
description: ""
project: "dataplatform"
project_version: "1.0.0"
menu:
  dataplatform-1.0.0:
    name: "Spark Cluster Manager"
    identifier: "learn_spark_cluster_manager"
    weight: 103
    parent: "learn"
toc: true
aliases:
  - /dataplatform/1.0.0/learn-about-dataplatform/spark-cluster-manager-features/
  - /dataplatform/latest/learn/spark-cluster-manager/
---

[bdp leader election]: {{<baseurl>}}dataplatform/1.0.0/learn/leader-election-service/
[bdp cluster manager]: {{<baseurl>}}dataplatform/1.0.0/configuring/replace-spark-cluster-manager/
[ee]: http://info.basho.com/Wiki_Riak_Enterprise_Request.html

> The Spark cluster manager is available to [Enterprise users only][ee].

The Spark cluster manager provides all the functionality required for Spark Master high availability without the need to manage yet another software system (Zookeeper). This reduces operational complexity of Basho Data Platform (BDP).


> Please note that the Spark cluster manager depends on the [Riak Leader Election Service][bdp leader election]. Check out [Replace Your Previous Spark Cluster Manager with the Basho Data Platform Cluster Manager][bdp cluster manager] for instructions on setting up the Spark cluster manager.


## Zookeeper Replacement

The Spark cluster manager forms a pair with the [leader election service (LES)][bdp leader election]. It enables Spark to use the LES rather than ZooKeeper. Spark cluster manager provides all the functionality required for Spark Master high availability without the need to manage yet another software system.


## Store Spark Cluster Metadata in Riak KV

A consistent Riak bucket with CRDT map is used for reliable storage of the Spark cluster metadata.


## Integrate with BDP Leader Election Service

BDP leader election is built on top of `riak_ensemble`, a strongly consistent group of nodes that are responsible for correctness and fault tolerance.
