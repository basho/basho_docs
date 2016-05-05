---
title_supertext: "Spark-Riak Connector Add-on (Riak KV)"
title: "Quick Start Guide"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Quick Start Guide"
    identifier: "spark_riak_quick_start"
    weight: 101
    parent: "addons_spark_riak"
toc: true
canonical_link: "docs.basho.com/riak/kv/latest/add-ons/spark-riak-connector/quick-start"
---

This guide will run you through a quick example that uses the Spark-Riak connector to read and write data using Java and Scala. We will assume you are running this guide on Mac OSX. 

## Prerequisites

- Update Homebrew with `brew update`.
- Install Riak KV OSX build. Instruction can be found [here](../../../setup/installing/mac-osx/)
- Set open file limits for Riak by following the guide [here](../../../using/performance/open-files-limit/#mac-os-x).
- Install Spark with `brew install apache-spark`.
- Download the Spark-Riak connector uber jar (containing all dependencies) from here: https://github.com/basho/spark-riak-connector/releases/latest.

Start Riak KV with `riak start`.

## Scala

In this quick start guide we will run you through an example usage of the Spark-Riak connector using the Spark Scala REPL.

Start Spark Scala REPL with: 

```
path/to/spark-shell \
--conf spark.riak.connection.host=127.0.0.1:8087 \
--driver-class-path /path/to/spark-riak-connector-»VERSION«-uber.jar
```

Import the following:

```scala
import com.basho.riak.client.core.query.Namespace
import org.apache.spark.{SparkContext, SparkConf}
import com.basho.riak.spark._
```

Create an RDD with some test data and save to Riak KV bucket:

```scala
val data = Array(1, 2, 3, 4, 5)
val testRDD = sc.parallelize(data)
testRDD.saveToRiak(new Namespace("kv_bucket_a"))
```

When saving RDDs that only have values (i.e. not key-value pairs) to Riak, keys will be automatically generated for each value in the RDD.

Query Riak KV bucket and print results:

```scala
val queryRDD = sc.riakBucket[String](new Namespace("kv_bucket_a")).queryAll()
queryRDD.collect().foreach(println)
```

You should see:

```scala
scala> queryRDD.collect().foreach(println)
5
4
3
1
2
```

You will notice that all values (1,2,3,4,5) are printed.

Create a key-value pair RDD and save it to Riak KV bucket:

```scala
val data = Array((1,2), (2,4), (3,6), (4,8), (5,10))
val testRDD = sc.parallelize(data)
testRDD.saveToRiak(new Namespace("kv_bucket_b"))
```

Query Riak KV bucket and print results:

```scala
val queryRDD = sc.riakBucket[String](new Namespace("kv_bucket_b")).queryAll()
queryRDD.collect().foreach(println)
```

You should see:

```scala
scala> queryRDD.collect().foreach(println)
4
6
10
2
8
```

You will notice only 2, 4, 6, 8, and 10 are printed. When using tuples, the first value is the key and the second value is the value.

Query Riak KV bucket by keys:

```scala
val keyRDD = sc.riakBucket[String](new Namespace("kv_bucket_b")).queryBucketKeys("1", "2", "3")
keyRDD.collect().foreach(println)
```

You should see:

```scala
scala> keyRDD.collect().foreach(println)
4
6
2
```

You will notice that this prints out 2,4,6.

If your data contains secondary indices (2i) you can query by that too:

```scala
val rangeRDD = sc.riakBucket[String](new Namespace("kv_bucket_b")).query2iRange("myIndex", 1L, 5000L)
rangeRDD.collect().foreach(println)
```

Since none of our test data had a secondary index associated with it, our rangeRDD will be empty.

## Java

Coming Soon!
