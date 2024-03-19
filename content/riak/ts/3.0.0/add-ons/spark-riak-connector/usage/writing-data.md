---
title_supertext: "Spark-Riak Connector Add-on (Riak TS)"
title: "Writing Data"
description: ""
project: "riak_ts"
project_version: "3.0.0"
lastmod: 2022-09-20T00:00:00-00:00
sitemap:
  priority: 0.9
menu:
  riak_ts-3.0.0:
    name: "Writing Data"
    identifier: "spark_riak_usage_write_data"
    weight: 103
    parent: "spark_riak_usage"
toc: true
aliases:

---

## Writing Data To TS Table

### Scala

You can save an RDD of type `<org.apache.spark.sql.Row>` to Riak TS as follows:

```scala
val output_ts_table = "test-bucket"
rdd.saveToRiakTS(output_ts_table);
```

### Java

An existing JavaRDD<org.apache.spark.sql.Row>, `sqlRdd`, can be saved to Riak TS as follows:

```java
SparkJavaUtil.javaFunctions(sqlRdd).saveToRiakTS(TABLE_NAME);
```
