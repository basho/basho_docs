---
title: Spark Cluster Manager Features
project: dataplatform
version: 1.0.0+
document: guide
audience: beginner
---

[ee]: http://info.basho.com/Wiki_Riak_Enterprise_Request.html

<div class="note">
The Spark cluster manager is available to [Enterprise users only][ee].
</div>

The Spark cluster manager provides all the functionality required for Spark Master high availability without the need to manage yet another software system (Zookeeper). This reduces operational complexity of Basho Data Platform (BDP).

<div class="note">
Please note that the Spark cluster manager depends on the [Riak Leader Election Service](LINK). Check out [Replace Your Previous Spark Cluster Manager with the Basho Data Platform Cluster Manager](LINK) for instructions on setting up the Spark cluster manager.
</div>


##Zookeeper Replacement

The Spark cluster manager forms a pair with the [leader election service (LES)](LINK). It enables Spark to use the LES rather than ZooKeeper. Spark cluster manager provides all the functionality required for Spark Master high availability without the need to manage yet another software system.


##Store Spark Cluster Metadata in Riak KV

A consistent Riak bucket with CRDT map is used for reliable storage of the Spark cluster metadata.


## Integrate with BDP Leader Election Service

BDP leader election is built on top of `riak_ensemble`, a strongly consistent group of nodes that are responsible for correctness and fault tolerance.
