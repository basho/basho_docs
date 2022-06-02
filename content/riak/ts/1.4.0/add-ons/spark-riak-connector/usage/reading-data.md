---
title_supertext: "Spark-Riak Connector Add-on (Riak TS)"
title: "Reading Data"
description: ""
project: "riak_ts"
project_version: "1.4.0"
menu:
  riak_ts-1.4.0:
    name: "Reading Data"
    identifier: "spark_riak_usage_read_data"
    weight: 102
    parent: "spark_riak_usage"
toc: true
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
