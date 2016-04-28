---
title_supertext: "Spark-Riak Connector Add-on"
title: "Features"
description: ""
project: "riak_ts"
project_version: "1.3.0"
menu:
  riak_ts-1.3.0:
    name: "Features"
    identifier: "spark_riak_features"
    weight: 500
    parent: "addons_spark_riak"
toc: true
canonical_link: "docs.basho.com/riak/ts/latest/add-ons/spark-riak-connector/features"
---

## Overview

* Construct a Spark RDD from a Riak KV bucket with a set of keys
* Construct a Spark RDD from a Riak KV bucket by using a 2i string index or a set of indexes
* Construct a Spark RDD from a Riak KV bucket by using a 2i range query or a set of ranges 
* Map JSON formatted data from Riak KV to user defined types
* Save a Spark RDD into a Riak KV bucket and apply 2i indexes to the contents
* Construct a Spark Dataframe from a Riak TS table using range queries and schema discovery
* Save a Spark Dataframe into a Riak TS table
* Construct a Spark RDD using Riak KV bucket's enhanced 2i query (a.k.a. full bucket read)
* Perform parallel full bucket reads from a Riak KV bucket into multiple partitions
