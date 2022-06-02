---
title: "Using Spark-Riak Connector"
description: ""
project: "dataplatform"
project_version: "1.0.0"
menu:
  dataplatform-1.0.0:
    name: "Spark-Riak Connector"
    identifier: "using_spark_riak_connector"
    weight: 101
    parent: "using"
toc: true
aliases:
  - /dataplatform/1.0.0/using-dataplatform/using-sparkconnector/
  - /dataplatform/latest/using/spark-riak-connector/
---

This is a quick, practical guide on how to use the Spark Riak connector.


## Dependencies

If your Spark project uses Maven, include the following dependency in your application's POM file to enable Spark Riak connector:

```xml
<dependencies>
    <dependency>
        <groupId>com.basho</groupId>
        <artifactId>spark-riak-connector</artifactId>
        <version>1.0.0</version>
    </dependency>
    ...
</dependencies>
```

If your Spark application is going to be written in Java, add the following dependency in addition to the one above:

```xml
<dependency>
    <groupId>com.basho</groupId>
    <artifactId>spark-riak-connector-java</artifactId>
    <version>1.0.0</version>
</dependency>
```


## Creating Spark Context

The following import statements have to be included at the top of your Spark application to enable the connector:

```scala
import com.basho.riak.client.core.query.Namespace
import com.basho.spark.connector.rdd.RiakFunctions
import org.apache.spark.{SparkContext, SparkConf}
import com.basho.spark.connector._
```

Now let's create a Spark context that connects to a Riak KV node at 192.168.1.55:8087.

(In this example we also assume that there is a Spark Master running at 192.168.1.100:7077.) 

```scala
val conf = new SparkConf()
        .set("spark.riak.connection.host", "192.168.1.55:8087")

val sc = new SparkContext("spark://192.168.1.100:7077", "Best Spark App Ever", conf)
```


## Loading Data from Riak KV into Spark

Once the Spark Context is created we can load data stored in Riak buckets into Spark as RDDs.

Let's do a simple but very powerful full bucket read. We are going to read the content of entire Riak bucket in one command, and it will happen in an efficient, partitioned, parallel way, getting values as strings:

```scala
 val data = sc.riakBucket[String](new Namespace("bucket-full-of-data"))
      .queryAll()
```

What if you don't want the content of entire bucket, but instead want something very specific? When you know your keys by name, you can pass them in directly:

```scala
val rdd = sc.riakBucket(new Namespace("FOO"))
      .queryBucketKeys("mister X", "miss Y", "dog Z")
```

What if you want a range of values (say, from 1 to 5000) defined by a numeric 2i index in Riak KV? Your bucket is 'BAR' and your index is 'myIndex'.

```scala
val rdd = sc.riakBucket(new Namespace("BAR"))
      .query2iRange("myIndex", 1L, 5000L)
```

## Saving Data into Riak KV from Spark

To save your calculation results from Spark into a Riak bucket, you need to first add this import line at the top of your application:

```scala
import com.basho.spark.connector.writer.{WriteDataMapper, WriteDataMapperFactory}
```

Let's assume we want the output persisted into the bucket named 'FOO'. Here is how we write data from Spark RDD into the FOO bucket:

```scala
rdd.saveToRiak(new Namespace("FOO"))
```
