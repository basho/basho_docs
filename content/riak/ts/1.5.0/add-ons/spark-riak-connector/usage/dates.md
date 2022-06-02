---
title_supertext: "Spark-Riak Connector Add-on (Riak TS)"
title: "Working With Riak TS Dates"
description: ""
project: "riak_ts"
project_version: "1.5.0"
menu:
  riak_ts-1.5.0:
    name: "Working With TS Dates"
    identifier: "spark_riak_usage_ts_dates"
    weight: 105
    parent: "spark_riak_usage"
toc: true
---

Riak TS automatically stores all datetimes as a Long integer that represents milliseconds from the [beginning of the epoc](https://en.wikipedia.org/wiki/Unix_time). This is not very human friendly so we have provided a Spark configuration option called `spark.riakts.bindings.timestamp`. This option is for use with Automatic Schema Discovery and allows for conversion from Riak TS datetimes, which are stored as Longs, to Timestamps.  The default value of this option is `useTimestamp` which converts Longs to Timestamps. If you would like to use the original Long value, you can use the option value of `useLong`. All conversion takes place during Automatic Schema Discovery when reading from Riak TS tables.

You can provide the schema manually:

```scala
val schemaWithLong = StructType(List(
      StructField(name = "surrogate_key", dataType = LongType),
      StructField(name = "family", dataType = StringType),
      StructField(name = "time", dataType = LongType),
      StructField(name = "user_id", dataType = StringType),
      StructField(name = "temperature_k", dataType = DoubleType))
    )

    val df = sqlContext.read
      .format("org.apache.spark.sql.riak")
      .schema(newSchema)
      .load(tableName)
      .filter(s"time >= $queryFromMillis AND time <= $queryToMillis AND surrogate_key = 1 AND family = 'f'")
```

You can use `spark.riakts.bindings.timestamp` and Automatic Schema Discovery with `useLong`:

```scala
val df = sqlContext.read
      .format("org.apache.spark.sql.riak")
      .option("spark.riakts.bindings.timestamp", "useLong")
      .load(tableName)
      .filter(s"time > $queryFromMillis AND time < $queryToMillis AND surrogate_key = 1 AND family = 'f'")
```

In the previous example, the query times, `queryFromMillis` and `queryToMillis`, are Long integers since the datetime values in `df` are stored as Long integers.

Or, you can use `spark.riakts.bindings.timestamp` and Automatic Schema Discovery with `useTimestamp`:

```scala
val df = sqlContext.read
      .format("org.apache.spark.sql.riak")
      .option("spark.riakts.bindings.timestamp", "useTimestamp")
      .load(tableName)
      .filter(s"time > CAST('$from' AS TIMESTAMP) AND time < CAST('$to' AS TIMESTAMP) AND surrogate_key = 1 AND family = 'f'")
```

In the previous example, the query times, `CAST('$from' AS TIMESTAMP)` and `CAST('$to' AS TIMESTAMP)`, are Timestamps which are cast from a Long integer since the datetime values in `df` are stored as Timestamps.
