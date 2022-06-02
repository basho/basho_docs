---
title_supertext: "Spark-Riak Connector Add-on (Riak TS)"
title: "Configuration of Spark Context"
description: ""
project: "riak_ts"
project_version: "1.5.0"
menu:
  riak_ts-1.5.0:
    name: "Spark Context Configuration"
    identifier: "spark_riak_usage_config_context"
    weight: 101
    parent: "spark_riak_usage"
toc: true
---

The following `import` statements should be included at the top of your Spark application to enable the connector:

```scala
import com.basho.riak.client.core.query.Namespace
import com.basho.riak.spark.rdd.RiakFunctions
import org.apache.spark.{SparkContext, SparkConf}
import com.basho.riak.spark._
```

```python
import pyspark
```

```java
import com.basho.riak.client.core.query.Namespace;
import com.basho.riak.spark.japi.SparkJavaUtil;
import org.apache.spark.SparkConf;
import org.apache.spark.api.java.JavaRDD;
import org.apache.spark.api.java.JavaSparkContext;
```

## Configuration Options

You can control how your Spark application interacts with Riak by configuring different options for your `SparkContext` or `SQLContext`. You can set these options within the `$SPARK_HOME/conf/spark-default.conf`.  If you don't set an option, it will be automatically set to the default values listed below.

You can set the below options for the `SparkConf` object:

Property name                                  | Description                                       | Default value      | Riak Type
-----------------------------------------------|---------------------------------------------------|--------------------|-------------
spark.riak.connection.host                     | IP:port of a Riak node protobuf interface         | 127.0.0.1:8087     | KV/TS
spark.riak.connections.min                     | Minimum number of parallel connections to Riak    | 20                 | KV/TS
spark.riak.connections.max                     | Maximum number of parallel connections to Riak    | 30                 | KV/TS
spark.riak.input.fetch-size                    | Number of keys to fetch in a single round-trip to Riak | 1000          | KV
spark.riak.input.split.count                   | Desired minimum number of Spark partitions to divide the data into | 10| KV
spark.riak.write.replicas                      | Quorum value on write. Integer value or symbolic constant can be used. Possible symbolic constants are: <ul><li>all - All replicas must reply.</li><li>one - This is the same as integer value 1.</li><li>quorum - A majority of the replicas must respond, that is, “half plus one”.</li><li>default - Uses whatever the per-bucket consistency property, which may be any of the above values, or an integer.</li></ul>                                              | default | KV
spark.riak.connections.inactivity.timeout      | Time to keep connection to Riak alive in milliseconds | 1000 | KV/TS
spark.riakts.bindings.timestamp                | To treat/convert Riak TS timestamp columns either as a Long (UNIX milliseconds) or as a Timestamps during the automatic schema discovery. Valid values are: <ul><li>useLong</li><li>useTimestamp</li><ul> | useTimestamp | TS
spark.riak.partitioning.ts-range-field-name    | Name of quantized field for range query       | 1                  | TS
spark.riakts.write.bulk-size                   | Bulk size for parallel TS table writes            | 100                | TS

Example:

```scala
val conf = new SparkConf()
        .setAppName("My Spark Riak App")
        .set("spark.riak.connection.host", "127.0.0.1:8087")
        .set("spark.riak.connections.min", "20")
        .set("spark.riak.connections.max", "50")

val sc = new SparkContext("spark://127.0.0.1:7077", "test", conf)
```

```python
conf = pyspark.SparkConf().setAppName("My Spark Riak App")
conf.set("spark.riak.connection.host", "127.0.0.1:8087")
conf.set("spark.riak.connections.min", "20")
conf.set("spark.riak.connections.max", "50")
sc = pyspark.SparkContext("spark://127.0.0.1:7077", "test", conf)
```

```java
SparkConf sparkConf = new SparkConf().setAppName("My Spark Riak App");

  setSparkOpt(sparkConf, "spark.riak.connection.host", "127.0.0.1:8087");
  setSparkOpt(sparkConf, "spark.riak.connections.min", "20");
  setSparkOpt(sparkConf, "spark.riak.connections.max", "50");

JavaSparkContext jsc = new JavaSparkContext(sparkConf);
```
