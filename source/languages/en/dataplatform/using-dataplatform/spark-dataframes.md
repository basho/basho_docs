---
title: Using Spark DataFrames with Riak
project: dataplatform
version: 1.1.0+
document: guide
toc: true
audience: beginner
---

##Building Spark DataFrame over RiakRDD

RiakRDD is a class that implements Spark RDD in Riak.

To use Spark DataFrames on top of your RiakRDD first create SqlContext from SparkContext:

```scala
  val sqlContext = new org.apache.spark.sql.SQLContext(sc)
```

Then after you import:

```scala
  import sqlContext.implicits._
```
    
Use the `toDf()` method on your RiakRDD:

```scala
  val riakRdd = sc.riakBucket[UserData](namespace).queryAll
  val df = riakRdd.toDF
```

Next specify the user defined type:

```scala
  case class UserData(user_id: String, name: String, age: Int, category: String)
```

This allows schema inference using reflection.

Once you have your `org.apache.spark.sql.DataFrame` object you can use its methods for filtering:

```scala
  df.where(df("age") >= 50).select("id", "name")
```

Or more complex operations like grouping:

```scala
  df.groupBy("category").count
```

Alternatively, you can register a table:

```scala
  df.registerTempTable("users")
```

And use standard SQL queries over it:

```scala
  sqlContext.sql("select * from users where age >= 50")
```

Another thing you can use are user defined functions (UDFs). First, you have to register a UDF:

```scala
  sqlContext.udf.register("stringLength", (s: String) => s.length)
```

After that you can use it in SQL queries:

```scala
  sqlContext.sql("select user_id, name, stringLength(name) nameLength from users order by nameLength")
```
    
## Saving DataFrames into Riak

You can save existing DataFrames into Riak as well. To do this make sure you import the `com.basho.riak.spark` object so that the `saveToRiak()` method is available:

```scala
  import com.basho.riak.spark._
```

Then use `toJSON()` method to save data to Riak in `json` format:

```
  dataFrame.toJSON.map {
    line =>
      val obj = RiakObjectConversionUtil.to(line)
      obj.setContentType("application/json")
      obj
    }.saveToRiak(namespace)
```

Setting the content type to `application/json` will allow automatic conversion to a user defined type later when reading from Riak.
