---
title: "Using The Spark-Riak Connector"
description: ""
project: "riak_ts"
project_version: "1.3.0"
menu:
  riak_ts-1.3.0:
    name: "Usage"
    identifier: "spark_riak_usage"
    weight: 104
    parent: "addons_spark_riak"
toc: true
canonical_link: "docs.basho.com/riak/ts/latest/add-ons/spark-riak-connector/usage"
---

This section will walk you through setting up your application for development with the Spark-Riak connector.

> **Note:**
>
> Currently, if you are using Python, only Riak TS tables, Spark DataFrames and Spark SQL are supported. Reading and writing to Riak KV buckets is not supported yet with Python.

* [Configuration of Spark Context](config-spark-context)
* [Reading Data From KV Bucket](read-kv-bucket)
* [Writing Data To KV Bucket](write-kv-bucket)
* [Writing Data To KV Bucket With 2i Indices](write-kv-bucket-2i)
* [Reading Data From TS Table](read-ts-table)
* [Writing Data To TS Table](write-ts-table)

### Read From KV Bucket

To use any of the Riak query functions, initial RiakJavaRDD must be created by using any of the SparkContextJavaFunctions.riakBucket(...) methods. The resulting RiakJavaRDD still needs query criteria to perform the following operations:

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

### Write To KV Bucket

An existing JavaRDD<{UserDefinedType}>, `rdd`, can be saved to KV bucket as follows 

```java
SparkJavaUtil.javaFunctions(rdd).saveToRiak(NAMESPACE);
```

{UserDefinedType} must be serializable and can use annotations from com.basho.riak.client.api.annotations package.

```java
class SimpleUDT implements Serializable {
  @RiakIndex(name = "creationNo")
  private long creationNo;
  private String value;
  @RiakKey
  private String key;
...
}
```

### Read From TS Table

To use any of the Riak query functions, the initial RiakTSJavaRDD must be created by using SparkContextJavaFunctions.riakTSBucket() method. The resulting RiakTSJavaRDD still needs a sql query string to perform range scan:

```java

String test_query = "SELECT * FROM %s WHERE time >= %d AND time <= %d  AND  weather = 'sunny'"

SparkJavaUtil.javaFunctions(jsc)
  .riakTSTable(TABLE_NAME, Row.class)
  .sql(String.format(test_query, TABLE_NAME, from, to));
```

### Write To TS Table

An existing JavaRDD<org.apache.spark.sql.Row>, `sqlRdd`, can be saved to Riak TS as follows

```java
SparkJavaUtil.javaFunctions(sqlRdd).saveToRiakTS(TABLE_NAME);
