---
title: "Spark-Riak Connector Add-on (Riak KV)"
description: "Spark-Riak Connector Add-on for Riak KV"
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Spark-Riak Connector"
    identifier: "addons_spark_riak"
    weight: 102
    parent: "add-ons"
toc: true
canonical_link: "docs.basho.com/riak/kv/latest/add-ons/spark-riak-connector"
---

The Spark-Riak connector enables you to connect Spark applications to Riak KV and [Riak TS](/riak/ts/1.3.0/add-ons/spark-riak-connector) with the Spark RDD and Spark DataFrames APIs. You can write your app in Scala, Python, and Java. The connector makes it easy to partition the data you get from Riak so multiple Spark workers can process the data in parallel, and it has support for failover if a Riak node goes down while your Spark job is running.

> **Note:**
>
> Currently, if you are using Python, only Riak TS tables, Spark DataFrames and Spark SQL are supported. Reading and writing to Riak KV buckets is not supported yet with Python.

## Compatibility

* Riak KV 2.1+ (or Riak TS 1.2+)
* Apache Spark 1.6+
* Scala 2.10
* Java 8

## Features Overview

* Construct a Spark RDD from a Riak KV bucket with a set of keys
* Construct a Spark RDD from a Riak KV bucket by using a Secondary Index (2i) string or a set of indexes
* Construct a Spark RDD from a Riak KV bucket by using a 2i range query or a set of ranges 
* Map JSON-formatted data from Riak KV to user-defined types
* Save a Spark RDD into a Riak KV bucket and apply 2i indexes to the contents
* Construct a Spark RDD using Riak KV bucket's enhanced 2i query (a.k.a. full bucket read)
* Perform parallel full bucket reads from a Riak KV bucket into multiple partitions

## Getting Started

* [Quick Start](quick-start)
* [Getting the Spark-Riak connector](getting)
