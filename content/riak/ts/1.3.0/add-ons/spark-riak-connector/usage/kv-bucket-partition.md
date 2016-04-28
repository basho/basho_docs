---
title_supertext: "Using The Spark-Riak Connector"
title: "Partitioning for KV Buckets"
description: ""
project: "riak_ts"
project_version: "1.3.0"
menu:
  riak_ts-1.3.0:
    name: "Partitioning for KV Buckets"
    identifier: "spark_riak_usage_partition_kv"
    weight: 105
    parent: "spark_riak_usage"
toc: true
canonical_link: "docs.basho.com/riak/ts/latest/add-ons/spark-riak-connector/usage/kv-bucket-partition"
---

## Key Based Partitioning

Querying with the following methods with result in a RDD with single partition:

* query2iRange(index, from, to)
* query2iKeys(index, keys*)
* queryBucketKeys(keys*)

The following methods will split the RDD into multiple partitions:
* partitionBy2iRanges(index, ranges*) will create a partition for each of the  input ranges

```scala
val data = sc.riakBucket[UserTS](DEFAULT_NAMESPACE)
      .partitionBy2iRanges(CREATION_INDEX, 1 -> 3, 4 -> 6, 7 -> 12)
```

* partitionBy2iKeys(index: String, keys*) will create a partition for each of the input keys

```scala
    val data = sc.riakBucket[UserTS](DEFAULT_NAMESPACE)
      .partitionBy2iKeys("category", "neighbor", "visitor", "stranger")
```

## Coverage Plan Based Partitioning

A coverage plan is Riak's description of what keys are stored on what nodes of the cluster.
The coverage plan based partitioner will be used for the following queries:
* queryAll()
* query2iRangeLocal(index, from, to)

First, a query for the coverage plan is made to Riak. Then, the returned coverage entries (one for each VNode) are grouped by host and split into a number of partitions (defined by **spark.riak.input.split.count**) in such a way that each partition reads data from a single host. This means that each partition can processes multiple coverage entries but all of the parition will point to single Riak node (if it's possioble). While processing, the coverage entry partition will iteratively read data in portions. The size of a portion is defined by **spark.riak.input.fetch-size**.
