---
title_supertext: "Spark-Riak Connector Add-on (Riak KV)"
title: "Spark Streaming KV Buckets"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Spark Streaming Example"
    identifier: "spark_riak_usage_streaming_example"
    weight: 106
    parent: "spark_riak_usage"
toc: true
canonical_link: "docs.basho.com/riak/kv/latest/add-ons/spark-riak-connector/usage/streaming-example"
---

The Spark-Riak Connector can be used with Spark Streaming. To demonstrate this usage, we will work through a small Scala example. The KV Streaming example is located in the [examples folder](https://github.com/basho/spark-riak-connector/blob/master/examples/src/main/scala/com/basho/riak/spark/examples/streaming/) of the Spark-Riak Connector repo.

This example requires the use of Kafka. Please install Kafka and setup a Kafka broker prior to running this example. We will assume that there is a Kafka broker running at `127.0.0.1:9092` with a topic called `streaming`. Instructions on setting up Kafka topics can be found in [this guide](https://kafka.apache.org/documentation.html#quickstart). You can create a broker and topic with the following:

```
path/to/kafka/bin/zookeeper-server-start.sh config/zookeeper.properties
path/to/kafka/bin/kafka-server-start.sh config/server.properties
path/to/kafka/bin/kafka-topics.sh --create --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic streaming
```

You will need to build the KV example as well. Please follow the instructions on [building the examples](https://github.com/basho/spark-riak-connector/tree/master/examples#building-and-running-examplesdemos).

## Spark Streaming KV Buckets Example

Now that we are set up, let's look at the KV bucket example [here](https://github.com/basho/spark-riak-connector/blob/master/examples/src/main/scala/com/basho/riak/spark/examples/streaming/StreamingKVExample.scala). 

In the first chunk of code in the main method, we are just setting up our local Spark Streaming context and setting the name for the KV bucket to `test-data`:

```scala
val sparkConf = new SparkConf(true)
      .setAppName("Simple Spark Streaming to Riak KV Demo")
setSparkOpt(sparkConf, "spark.master", "local")
setSparkOpt(sparkConf, "spark.riak.connection.host", "127.0.0.1:8087")

val sc = new SparkContext(sparkConf)
val streamCtx = new StreamingContext(sc, Durations.seconds(15))
val sqlContext = new org.apache.spark.sql.SQLContext(sc)
import sqlContext.implicits._
val namespace = new Namespace("test-data")
```

Next we are setting up Kafka properties:

```scala
val kafkaProps = Map[String, String](
      "metadata.broker.list" -> "127.0.0.1:9092",
      "client.id" -> UUID.randomUUID().toString
    )
```

Then, we are using `KafkaUtils` to create a stream from the Kafka topic `streaming` into our KV bucket `test-data`:

```scala
    KafkaUtils
      .createDirectStream[String, String, StringDecoder, StringDecoder](streamCtx, kafkaProps, Set[String]("streaming"))
      .foreachRDD { rdd =>
        val rows = sqlContext.read.json(rdd.values).map {
          line => val obj = RiakObjectConversionUtil.to(line)
            obj.setContentType("application/json")
            obj
        }.saveToRiak(namespace)
      }
```
 And finally, we are starting the stream:
 
 ```scala
 streamCtx.start()
 streamCtx.awaitTermination()
 ```

 Now that we understand the code, we can run the `StreamingKVExample.scala` example with:
 
 ```
 /path/to/spark-riak-connector-examples/bin/run-example streaming.StreamingKVExample
 ```
 
 This will start a stream from the Kafka topic `streaming` into the KV bucket `test-data` that we just created. This stream will run until terminated. Whenever a message is produced for Kafka topic `streaming`, the Spark Streaming context that the example creates will automatically stream the message from the topic into the KV bucket. To see this in action, we need to send a message to the Kafka topic `streaming` with the Kafka console producer script, which can be found in the Kafka directory.
 
 ```
 /path/to/kafka/bin/kafka-console-producer.sh --broker-list localhost:9092 --topic streaming
 ```
 
 This script will read messages from the terminal and pass it to the topic. From the topic, the Spark Streaming context will write the message to Riak KV bucket `test-data`.  As an example put the following into the terminal:
 
 ```
 {"time": "2016-01-01 08:30:00.000", "weather": "sunny", "temperature": 25.0, "humidity": 67.0, "pressure": 30.20, "family": "f"}
 ```
 
You should now be able to see this data entry in the KV bucket `test-data`.
