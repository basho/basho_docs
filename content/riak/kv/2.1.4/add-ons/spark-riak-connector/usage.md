---
title_supertext: "Spark-Riak Connector Add-on (Riak KV)"
title: "Usage"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Usage"
    identifier: "spark_riak_usage"
    weight: 104
    parent: "addons_spark_riak"
toc: true
canonical_link: "docs.basho.com/riak/kv/latest/add-ons/spark-riak-connector/usage"
---

This section will walk you through setting up your application for development with the Spark-Riak connector.

> **Note:**
>
> Currently, if you are using Python, only Riak TS tables, Spark DataFrames and Spark SQL are supported. Reading and writing to Riak KV buckets is not supported yet with Python.

* [Configuration of Spark Context](config-spark-context)
* [Reading Data From KV Bucket](reading-data)
* [Writing Data To KV Bucket](writing-data)
* [Working With Spark Dataframes](dataframes)
* [Partitioning for KV Buckets](bucket-partitions)
* [Spark Streaming KV Buckets Example](streaming-example)
