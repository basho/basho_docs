---
title_supertext: "Spark-Riak Connector Add-on (Riak TS)"
title: "Table Range Query Partitioning"
description: ""
project: "riak_ts"
project_version: "1.3.0"
menu:
  riak_ts-1.3.0:
    name: "TS Table Range Query Partitioning"
    identifier: "spark_riak_usage_ts_range_query_partition"
    weight: 106
    parent: "spark_riak_usage"
toc: true
version_history:
  in: "1.3.0+"
---

Riak TS range queries are limited to a maximum of 5 quanta (see [Querying Data in Riak TS]({{< baseurl >}}riak/ts/latest/using/querying/)). To work around this limitation or simply achieve higher read performance, large ranges can be split into smaller sub-ranges at partitioning time.

To use this functionality, you must provide the following options:

* `spark.riak.partitioning.ts-range-field-name` to identify quantized field
* `spark.riak.input.split.count` to identify number of partitions/subranges (default value is `10`)

For example:

```scala
   val df = sqlContext.read
      .option("spark.riak.input.split.count", "5")
      .option("spark.riak.partitioning.ts-range-field-name", "time")
      .format("org.apache.spark.sql.riak")
      .schema(schema)
      .load(ts_table_name)
      .filter(s"time >= CAST(111111 AS TIMESTAMP) AND time <= CAST(555555 AS TIMESTAMP) AND col1 = 'val1'")
```

```python
df = sqlContext.read \
      .option("spark.riak.input.split.count", "5") \
      .option("spark.riak.partitioning.ts-range-field-name", "time") \
      .format("org.apache.spark.sql.riak") \
      .schema(schema) \
      .load(ts_table_name) \
      .filter(s"time >= CAST(111111 AS TIMESTAMP) AND time <= CAST(555555 AS TIMESTAMP) AND col1 = 'val1'")

```

The initial range query will be split into 5 subqueries (one per each partition) as follows:

* ```time >= CAST(111111 AS TIMESTAMP) AND time < CAST(222222 AS TIMESTAMP) AND col1 = 'val1'```
* ```time >= CAST(222222 AS TIMESTAMP) AND time < CAST(333333 AS TIMESTAMP) AND col1 = 'val1'```
* ```time >= CAST(333333 AS TIMESTAMP) AND time < CAST(444444 AS TIMESTAMP) AND col1 = 'val1'```
* ```time >= CAST(444444 AS TIMESTAMP) AND time < CAST(555555 AS TIMESTAMP) AND col1 = 'val1'```
* ```time >= CAST(555555 AS TIMESTAMP) AND time < CAST(555556 AS TIMESTAMP) AND col1 = 'val1'```

Not providing the `spark.riak.partitioning.ts-range-field-name` property will default to having a single partition with initial query.
