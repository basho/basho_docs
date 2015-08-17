---
title: Spark Cluster Manager Features
project: dataplatform
version: 1.0.0+
document: guide
audience: beginner
---

##Overview

The Spark Cluster Manager provides all the functionality required for Spark Master high availability without the need to manage yet another software system (Zookeeper). This reduces operational complexity of Basho Data Platform.

<div class="note">
Please note that the Spark Cluster Manager depends on the [Riak Leader Election Service](LINK). Check out [Replace Your Previous Spark Cluster Manager with the Basho Data Platform Cluster Manager](LINK) for instructions on setting up the Spark Cluster Manager.
</div>


##Zookeeper Replacement

The Spark Cluster manager forms a pair with the [leader election service (LES)](LINK). It enables Spark to use the LES rather than ZooKeeper. Spark Cluster manager provides all the functionality required for Spark Master high availability without the need to manage yet another software system.


##Store Spark cluster metadata in Riak KV

A consistent Riak bucket with CRDT map is used for reliable storage of the Spark cluster metadata.


## Integrate with BDP Leader Election Service

BDP Leader Election is built on top of Riak Ensemble, a strongly consistent group of nodes that are responsible for the correctness and fault tolerance.
