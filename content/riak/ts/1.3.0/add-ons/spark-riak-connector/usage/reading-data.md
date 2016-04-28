---
title_supertext: "Using The Spark-Riak Connector"
title: "Reading Data"
description: ""
project: "riak_ts"
project_version: "1.3.0"
menu:
  riak_ts-1.3.0:
    name: "Reading Data"
    identifier: "spark_riak_usage_read_data"
    weight: 102
    parent: "spark_riak_usage"
toc: true
canonical_link: "docs.basho.com/riak/ts/latest/add-ons/spark-riak-connector/usage/reading-data"
---

> **Note:**
>
> Currently, if you are using Python, only Riak TS tables, Spark DataFrames and Spark SQL are supported. Reading and writing to Riak KV buckets is not supported yet with Python.

## Reading Data From KV Bucket

Once a SparkContext is created, we can load data stored in Riak KV buckets into Spark as RDDs. To specify which bucket to use:

```scala
val kv_bucket_name = new Namespace("test-data")
```

Let's do a simple but very powerful full bucket read. We're going to read the content of entire Riak KV bucket in one command, and it will happen in an efficient partitioned parallel way and get values as Strings:

```scala
 val data = sc.riakBucket[String](kv_bucket_name).queryAll()
```

When you know your keys by name, you can pass them in directly:

```scala
val rdd = sc.riakBucket[String](kv_bucket_name).queryBucketKeys("Alice", "Bob", "Charlie")
```

You can also specifiy a range of values (say, from 1 to 5000) defined by a numeric 2i index in Riak if your index is `myIndex`:

```scala
val rdd = sc.riakBucket[String](kv_bucket_name).query2iRange("myIndex", 1L, 5000L)
```

You can also specify a set of numeric 2i range values to query by:

```scala
val rdd = sc.riakBucket[String](kv_bucket_name).partitionBy2iRanges("myIndex", 1->3, 4->6, 7->12)
```

You can also query by a 2i string tag or set of 2i string tags:

```scala
val rdd = sc.riakBucket[String](kv_bucket_name).query2iKeys("mon_data", "wed_data", "fri_data")
```

## Reading Data From TS Table

Riak TS tables can be queried using the `sql()` method:

```scala
val ts_table_name = "test-table"
val rdd = sc.riakTSTable(ts_table_name)
      .sql(s"SELECT * FROM $ts_table_name WHERE time >= $from AND time <= $to")
```
