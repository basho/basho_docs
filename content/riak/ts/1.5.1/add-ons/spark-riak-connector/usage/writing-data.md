---
title_supertext: "Spark-Riak Connector Add-on (Riak TS)"
title: "Writing Data"
description: ""
project: "riak_ts"
project_version: "1.5.1"
lastmod: 2017-01-21T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_ts-1.5.1:
    name: "Writing Data"
    identifier: "spark_riak_usage_write_data"
    weight: 103
    parent: "spark_riak_usage"
toc: true
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
