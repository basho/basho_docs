---
title_supertext: "Spark-Riak Connector Add-on (Riak TS)"
title: "Spark Streaming TS Tables"
description: ""
project: "riak_ts"
project_version: "1.3.1"
menu:
  riak_ts-1.3.1:
    name: "Spark Streaming Example"
    identifier: "spark_riak_usage_streaming_example"
    weight: 108
    parent: "spark_riak_usage"
toc: true
version_history:
  in: "1.3.0+"
---

> **Note:**
>
> This guide assumes you are using Mac OSX.

The Spark-Riak connector can be used with Spark Streaming. To demonstrate this usage, we will work through a small Scala example. The TS Streaming example is located in the [examples folder](https://github.com/basho/spark-riak-connector/blob/master/examples/src/main/scala/com/basho/riak/spark/examples/streaming/) of the Spark-Riak connector repo.

These examples require the use of Kafka. Please install Kafka and setup a Kafka broker prior to running this example. We will assume that there is a Kafka broker running at `127.0.0.1:9092` with a topic called `streaming`. Instructions on setting up Kafka topics can be found in [this guide](https://kafka.apache.org/documentation.html#quickstart). You can create a broker and topic with the following:

```
path/to/kafka/bin/zookeeper-server-start.sh config/zookeeper.properties
path/to/kafka/bin/kafka-server-start.sh config/server.properties
path/to/kafka/bin/kafka-topics.sh --create --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic streaming
```

We also assume Riak TS is installed and there is a Riak TS node running at `127.0.0.1:8087`. You can find instruction to do so [here]({{<baseurl>}}riak/ts/1.2.0/installing/mac-osx/).

You will need to build the TS example as well. Please follow the instructions on [building the examples](https://github.com/basho/spark-riak-connector/tree/master/examples#building-and-running-examplesdemos).

## Spark Streaming TS Table Example

Let's now look at the TS table example ([found here](https://github.com/basho/spark-riak-connector/blob/master/examples/src/main/scala/com/basho/riak/spark/examples/streaming/StreamingTSExample.scala)):

```scala
 val schema = StructType(List(
      StructField(name = "weather", dataType = StringType),
      StructField(name = "family", dataType = StringType),
      StructField(name = "time", dataType = TimestampType),
      StructField(name = "temperature", dataType = DoubleType),
      StructField(name = "humidity", dataType = DoubleType),
      StructField(name = "pressure", dataType = DoubleType)))

    val sparkConf = new SparkConf(true)
      .setAppName("Simple Spark Streaming to Riak TS Demo")

    setSparkOpt(sparkConf, "spark.master", "local")
    setSparkOpt(sparkConf, "spark.riak.connection.host", "127.0.0.1:8087")
    setSparkOpt(sparkConf, "kafka.broker", "127.0.0.1:9092")

    val sc = new SparkContext(sparkConf)
    val streamCtx = new StreamingContext(sc, Durations.seconds(15))
    val sqlContext = new org.apache.spark.sql.SQLContext(sc)
    import sqlContext.implicits._

    val kafkaProps = Map[String, String](
      "metadata.broker.list" -> sparkConf.get("kafka.broker"),
      "client.id" -> UUID.randomUUID().toString
    )
```

This first section of code just sets up the table schema, a Spark Streaming context, a Spark SQL context, and Kafka properties. Note that we need to set up a TS table that reflect the schema in the previous section of code. We can create this TS table with:

```bash
curl -v -XPUT -H 'Content-Type: application/json' "http://$RIAK_HTTP/admin/explore/clusters/default/bucket_types/ts_weather_demo" -d '{"props":{"n_val":3, "table_def":"CREATE TABLE ts_weather_demo (weather varchar not null,family varchar not null,time timestamp not null,temperature  double,humidity double,pressure double,PRIMARY KEY ((weather, family, quantum(time, 1, 'h')), weather, family, time))"}}'
```

Be sure to substitute the Riak node's IP address and HTTP port in for `$RIAK_HTTP`.

The next section of the code is:

```scala
    KafkaUtils
      .createDirectStream[String, String, StringDecoder, StringDecoder](streamCtx, kafkaProps,
      Set[String]("streaming"))
      .foreachRDD { rdd => rdd.map(println)
        val rows = sqlContext.read.schema(schema).json(rdd.values)
          .withColumn("time", 'time.cast("Timestamp"))
          .select("weather", "family", "time", "temperature", "humidity", "pressure")

        rows.write
          .format("org.apache.spark.sql.riak")
          .mode(SaveMode.Append)
          .save("ts_weather_demo")
      }
```

In this section of code, we are setting up a stream from Kafka topic `streaming` into TS table `ts_weather_demo`. Here we are using our Spark SQL context to read each RDD streamed from the Kafka topic and then write into the TS table.

Now that we have seen the code let's run the example (see [here](https://github.com/basho/spark-riak-connector/tree/master/examples#building-and-running-examplesdemos) if you need to build the example). You can run the `StreamingTSExample.scala` example, after building, with:

```bash
/path/to/spark-riak-connector-examples/bin/run-example streaming.StreamingTSExample
```

Now that the stream is up and running, we need to actually send data to the Kafka topic. Let's start the Kafka console producer. This will allow us to stream messages from the terminal into the Kafka `streaming` topic.

```bash
/path/to/bin/kafka-console-producer.sh --broker-list localhost:9092 --topic streaming
```
 
Now paste the following message into the terminal:

```
{"time": "2016-01-01 08:30:00.000", "weather": "sunny", "temperature": 25.0, "humidity": 67.0, "pressure": 30.20, "family": "f"}
```

You can check that this worked by doing a simple SQL query for the example data.
