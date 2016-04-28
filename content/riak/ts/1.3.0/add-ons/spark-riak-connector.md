---
title: "Spark-Riak Connector Add-on"
description: "Spark-Riak Connector Add-on for Riak TS"
project: "riak_ts"
project_version: "1.3.0"
menu:
  riak_ts-1.3.0:
    name: "Spark-Riak Connector"
    identifier: "addons_spark_riak"
    weight: 101
    parent: "addons"
toc: true
canonical_link: "docs.basho.com/riak/ts/latest/add-ons/spark-riak-connector"
---

The Spark-Riak Connector enables you to connect Spark applications to Riak KV and Riak TS with the Spark RDD and Spark DataFrames APIs. You can write your app in Scala, Python, and Java. The connector makes it easy to partition the data you get from Riak so multiple Spark workers can process the data in parallel and it has support for failover if a Riak node goes down while your Spark job is running.

## Compatibility

* Riak TS 1.2+
* Apache Spark 1.6+
* Scala 2.10
* Java 8

## Getting Started

* [Quick Start](quick-start)
* [Getting the Spark-Riak Connector](getting)
