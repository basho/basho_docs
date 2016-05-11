---
title_supertext: "Spark-Riak Connector Add-on (Riak TS)"
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

## Reading Data From TS Table

### Scala

Riak TS tables can be queried using the `sql()` method:

```scala
val ts_table_name = "test-table"
val rdd = sc.riakTSTable(ts_table_name)
      .sql(s"SELECT * FROM $ts_table_name WHERE time >= $from AND time <= $to")
```

### Java

To use any of the Riak query functions, the initial `RiakTSJavaRDD` must be created by using `SparkContextJavaFunctions.riakTSBucket()` method. The resulting `RiakTSJavaRDD` still needs a SQL query string to perform a range scan:

```java

String test_query = "SELECT * FROM %s WHERE time >= %d AND time <= %d  AND  weather = 'sunny'"

SparkJavaUtil.javaFunctions(jsc)
  .riakTSTable(TABLE_NAME, Row.class)
  .sql(String.format(test_query, TABLE_NAME, from, to));
```

## Reading Data From KV Bucket

> **Note:**
>
> If you are using Python, only Riak TS tables, Spark DataFrames and Spark SQL are supported. Reading and writing to Riak KV buckets is not supported yet with Python.

### Scala

Once a `SparkContext` is created, we can load data stored in Riak KV buckets into Spark as RDDs. To specify which bucket to use:

```scala
val kv_bucket_name = new Namespace("test-data")
```

Next we can read the contents of entire Riak KV bucket in one command and get the values as strings. This command will execute in a partitioned and parallel way:

```scala
 val data = sc.riakBucket[String](kv_bucket_name).queryAll()
```

When you know your keys by name, you can pass them in directly:

```scala
val rdd = sc.riakBucket[String](kv_bucket_name).queryBucketKeys("Alice", "Bob", "Charlie")
```

You can also specify a range of values (say, from 1 to 5000) defined by a numeric secondary index (2i) in Riak if your index is `myIndex`:

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

### Java

To use any of the Riak query functions, an initial `RiakJavaRDD` must be created by using any of the `SparkContextJavaFunctions.riakBucket(...) `methods. The resulting `RiakJavaRDD` still needs query criteria to perform the following operations:

Load all data from a bucket - returns all existing data from the bucket:

```java
SparkJavaUtil.javaFunctions(jsc).riakBucket(NAMESPACE, String.class).queryAll();
```

Load data for a range of index values - returns only data that have index value inside a range (inclusive):

```java
SparkJavaUtil.javaFunctions(jsc).riakBucket(NAMESPACE, String.class).query2iRangeLocal(INDEX_NAME, from, to);
```

Load data for a list of index values - returns only data that have index value inside a list:

```java
SparkJavaUtil.javaFunctions(jsc).riakBucket(NAMESPACE, String.class).query2iKeys(INDEX_NAME, iValue1, iValue2, ...);
```
    
Load data by keys - return only data for listed keys:

```java
SparkJavaUtil.javaFunctions(jsc).riakBucket(NAMESPACE, String.class).queryBucketKeys("key-1", "key-3", "key-6", ...)
```
