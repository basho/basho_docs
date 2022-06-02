---
title_supertext: "Spark-Riak Connector Add-on (Riak TS)"
title: "Bulk Write"
description: ""
project: "riak_ts"
project_version: "1.4.0"
menu:
  riak_ts-1.4.0:
    name: "Bulk Write"
    identifier: "spark_riak_usage_ts_bulk_write"
    weight: 107
    parent: "spark_riak_usage"
toc: true
---

To write into a Riak TS table, the Spark-Riak Connector splits the initial set of rows into smaller bulks and processes them in parallel. Bulk size can be configured using `spark.riakts.write.bulk-size` property. The default number is `100`.

As an example, let's say your RDD has 2000 rows and you set `spark.riakts.write.bulk-size` to 200 and `spark.riak.connections.min` to 5. Then, there will be 10 bulks with 200 rows and each bulk will have 5 parallel write connections to Riak. The bulk size option can be configured in SparkConf:

```scala
val conf = new SparkConf().set("spark.riakts.write.bulk-size", "500")
```

```python
conf = new SparkConf().set("spark.riakts.write.bulk-size", "500")
```

Or you can set the `spark.riakts.write.bulk-size` property in the DataFrame's `.option()`:

```scala
val df = sqlContext.write
  .option("spark.riakts.write.bulk-size", "500")
        .format("org.apache.spark.sql.riak")
        .mode(SaveMode.Append)
        .save(bucketName)
```

```python
df = sqlContext.write
  .option("spark.riakts.write.bulk-size", "500")
        .format("org.apache.spark.sql.riak")
        .mode(SaveMode.Append)
        .save(bucketName)
```

Bulks will be written in parallel. The number of parallel writes for each partition is defined with the `spark.riak.connections.min` property (default is `20`):

```scala
val conf = new SparkConf()
  .set("spark.riakts.write.bulk-size", "500")
        .set("spark.riak.connections.min", "50")
```

```python
conf = pyspark.SparkConf()
conf.set("spark.riakts.write.bulk-size", "500")
conf.set("spark.riak.connections.min", "50")
```
