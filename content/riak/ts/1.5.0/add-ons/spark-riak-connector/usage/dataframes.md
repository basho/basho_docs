---
title_supertext: "Spark-Riak Connector Add-on (Riak TS)"
title: "Working With Spark Dataframes"
description: ""
project: "riak_ts"
project_version: "1.5.0"
menu:
  riak_ts-1.5.0:
    name: "Working With Spark Dataframes"
    identifier: "spark_riak_usage_dataframes"
    weight: 104
    parent: "spark_riak_usage"
toc: true
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
