---
title: "Spark-Riak Connector Add-on (Riak TS)"
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

The Spark-Riak Connector enables you to connect Spark applications to [Riak KV](/riak/kv/2.1.4/add-ons/spark-riak-connector) and Riak TS with the Spark RDD and Spark DataFrames APIs. You can write your app in Scala, Python, and Java. The connector makes it easy to partition the data you get from Riak so multiple Spark workers can process the data in parallel and it has support for failover if a Riak node goes down while your Spark job is running.

## Compatibility

* Riak TS 1.2+ (or Riak KV 2.1+)
* Apache Spark 1.6+
* Scala 2.10
* Java 8

## Features Overview

* Construct a Spark RDD from a Riak KV bucket with a set of keys
* Construct a Spark RDD from a Riak KV bucket by using a 2i string index or a set of indexes
* Construct a Spark RDD from a Riak KV bucket by using a 2i range query or a set of ranges 
* Map JSON formatted data from Riak KV to user defined types
* Save a Spark RDD into a Riak KV bucket and apply 2i indexes to the contents
* Construct a Spark Dataframe from a Riak TS table using range queries and schema discovery
* Save a Spark Dataframe into a Riak TS table
* Construct a Spark RDD using Riak KV bucket's enhanced 2i query (a.k.a. full bucket read)
* Perform parallel full bucket reads from a Riak KV bucket into multiple partitions

## Getting Started

* [Quick Start](quick-start)
* [Getting the Spark-Riak Connector](getting)
