---
title_supertext: "Spark-Riak Connector Add-on (Riak KV)"
title: "Working With Spark Dataframes"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Working With Spark Dataframes"
    identifier: "spark_riak_usage_dataframes"
    weight: 104
    parent: "spark_riak_usage"
toc: true
canonical_link: "docs.basho.com/riak/kv/latest/add-ons/spark-riak-connector/usage/dataframes"
---

## Spark Dataframes With KV Bucket

You can use Spark DataFrames on top of an RDD that was created from a KV Bucket. First you need to create a SQLContext from SparkContext:

```scala
val sqlContext = new org.apache.spark.sql.SQLContext(sc)
```

Then import:

```scala    
import sqlContext.implicits._
```

Next, you have to specify a user defined type to allow schema inference using reflection:

```scala
case class UserData(user_id: String, name: String, age: Int, category: String)
```    

Then, you can use the toDF() method on your RDD.

```scala  
val kv_bucket_name = new Namespace("test-data")
val riakRdd = sc.riakBucket[UserData](kv_bucket_name).queryAll()
val df = riakRdd.toDF()
```

Once you have your DataFrame you can use its methods for filtering

```scala
df.where(df("age") >= 50).select("id", "name")
```

or do more complex operations like grouping.


```scala
df.groupBy("category").count
```

Alternatively, you can register a table

```scala
df.registerTempTable("users")
```

and use Spark SQL queries over it.


```scala
sqlContext.sql("select * from users where age >= 50")
```

Another thing you can use are user defined functions (UDFs). First, you have to register a UDF.

```scala
sqlContext.udf.register("stringLength", (s: String) => s.length)
```

After that you can use it in SQL queries   

```scala
sqlContext.sql("select user_id, name, stringLength(name) nameLength from users order by nameLength")
```
When you already have a DataFrame, you can save it into Riak. To do that, make sure you have imported `com.basho.riak.spark._` so that saveToRiak() method is available.

```scala
import com.basho.riak.spark._
```

Then you can use toJSON() method to save data to riak in json format

```
dataFrame.toJSON.map {
  line =>
    val obj = RiakObjectConversionUtil.to(line)
    obj.setContentType("application/json")    
    obj
  }.saveToRiak(namespace)
```

Setting content type to `application/json` will allow automatic conversion to user defined type later when reading from Riak KV.
