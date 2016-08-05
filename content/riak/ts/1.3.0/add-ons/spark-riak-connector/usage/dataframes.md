---
title_supertext: "Spark-Riak Connector Add-on (Riak TS)"
title: "Working With Spark Dataframes"
description: ""
project: "riak_ts"
project_version: "1.3.0"
menu:
  riak_ts-1.3.0:
    name: "Working With Spark Dataframes"
    identifier: "spark_riak_usage_dataframes"
    weight: 104
    parent: "spark_riak_usage"
toc: true
version_history:
  in: "1.3.0+"
canonical_link: "https://docs.basho.com/riak/ts/latest/add-ons/spark-riak-connector/usage/dataframes"
---

## Spark Dataframes With TS Table

To enable DataFrames functionality the first steps are: 

```scala
val sc = new SparkContext()
val sqlContext = new org.apache.spark.sql.SQLContext(sc)
import sqlContext.implicits._
ts_table_name = "test_table"
```

```python
sc = pyspark.SparkContext(conf=conf)
sqlContext = pyspark.SQLContext(sc)
ts_table_name = "test_table"
```

To read data from the existing TS table `test-table` standard `SQLContext` means can be used by providing a `org.apache.spark.sql.riak` data format and using a Riak TS range query: 

```scala
val df = sqlContext.read   
  .option("spark.riak.connection.hosts","riak_host_ip:10017")
    .format("org.apache.spark.sql.riak")
    .load(ts_table_name)
  .select(“time”, “col1”, “col2”)
    .filter(s"time >= CAST($from AS TIMESTAMP) AND time <= CAST($to AS TIMESTAMP) AND  col1= $value1")
```

```python
df = sqlContext.read \
  .option("spark.riak.connection.hosts","riak_host_ip:10017") \
    .format("org.apache.spark.sql.riak") \
    .load(ts_table_name) \
  .select(“time”, “col1”, “col2”) \
    .filter(s"time >= CAST($from AS TIMESTAMP) AND time <= CAST($to AS TIMESTAMP) AND  col1= $value1")
```

Schema may be provided using the `.schema()` method. If not provided, it will be inferred. Any of the connector options can be provided in `.option()` or `.options()`.

Alternatively, `org.apache.spark.sql.riak.RiakSQLContext` can be created and then queried with range query using `sql()` method:

```scala
val riakSqlContext = new RiakSQLContext(sc, ts_table_name)
val alternativeDf = riakSqlContext.sql(s"SELECT time, col1 from $ts_table_name WHERE time >= CAST($from AS TIMESTAMP) AND time <= CAST($to AS TIMESTAMP) AND  col1= $value1")
```

A DataFrame, `inputDF`, that has the same schema as an existing TS table (column order and types) can be saved to Riak TS as follows: 

```scala
inputDF.write
   .option("spark.riak.connection.hosts","riak_host_ip:10017")
   .format("org.apache.spark.sql.riak")
   .mode(SaveMode.Append)
   .save(ts_table_name)
```

```python
inputDF.write \
   .option("spark.riak.connection.hosts","riak_host_ip:10017") \
   .format("org.apache.spark.sql.riak") \
   .mode(SaveMode.Append) \
   .save(ts_table_name)
```

`SaveMode.Append` is the only mode available. Any of the connector options can be provided in `.option()` or `.options()`.

## Spark Dataframes With KV Bucket

You can use Spark DataFrames on top of an RDD that was created from a KV Bucket. First you need to create a `SQLContext` from `SparkContext`:

```scala
val sqlContext = new org.apache.spark.sql.SQLContext(sc)
```

Then import:

```scala    
import sqlContext.implicits._
```

Next, you have to specify a User Defined Type to allow schema inference using reflection:

```scala
case class UserData(user_id: String, name: String, age: Int, category: String)
```    

Then, you can use the `toDF()` method on your RDD:

```scala  
val kv_bucket_name = new Namespace("test-data")
val riakRdd = sc.riakBucket[UserData](kv_bucket_name).queryAll()
val df = riakRdd.toDF()
```

Once you have your DataFrame, you can use its methods for filtering:

```scala
df.where(df("age") >= 50).select("id", "name")
```

or do more complex operations like grouping:


```scala
df.groupBy("category").count
```

Alternatively, you can register a table with:

```scala
df.registerTempTable("users")
```

and use Spark SQL queries over it:


```scala
sqlContext.sql("select * from users where age >= 50")
```

Another thing you can use are User Defined Functions (UDFs). First, you have to register a UDF:

```scala
sqlContext.udf.register("stringLength", (s: String) => s.length)
```

After that you can use it in SQL queries: 

```scala
sqlContext.sql("select user_id, name, stringLength(name) nameLength from users order by nameLength")
```

When you already have a DataFrame, you can save it into Riak. To do that, make sure you have imported `com.basho.riak.spark._` so that the `saveToRiak()` method is available:

```scala
import com.basho.riak.spark._
```

Then you can use the `toJSON()` method to save data to Riak in `json` format:

```
dataFrame.toJSON.map {
  line =>
    val obj = RiakObjectConversionUtil.to(line)
    obj.setContentType("application/json")    
    obj
  }.saveToRiak(namespace)
```

Setting content type to `application/json` will allow automatic conversion to user defined type later when reading from Riak KV.
