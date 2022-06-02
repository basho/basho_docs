---
title_supertext: "Learn About"
title: "SQL for Riak TS"
description: "SQL for Riak TS"
menu:
  riak_ts-1.4.0:
    name: "SQL for Riak TS"
    identifier: "riakts_sql"
    weight: 502
    parent: "about"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/learn-about/sqlriakts
---


[stackoverflow]: http://stackoverflow.com/questions/2578194/what-is-ddl-and-dml

Riak TS tables were designed around SQL tables. This page will go through some SQL basics and more in depth information about how SQL is used within Riak TS.  

This document uses CAPITAL LETTERS for SQL keywords, although you do not need to do so in using the keywords. This document also breaks out SQL queries into multiple lines for readability, but queries can be written as a single line.

## SQL Basics

Riak TS supports a subset of Structured Query Language (SQL): `SELECT` statements for querying called Data Manipulation Language (DML) or Data Query Language (DQL), and `CREATE TABLE` statements for defining Riak TS storage regions, also known as Data Definition Language (DDL). (You can read a nice division of DML and DDL [here][stackoverflow].)

The subset of SQL used by Riak TS supports several kinds of literals: 

* numbers 
* single-quoted strings
* time

It also supports double-quoted column references (a.k.a. field names).


## Schema Design

Let's say you want to store and graph usage metrics on a heterogeneous collection of network servers: disk usage, memory usage, CPU temperatures, load averages, and other metrics. Our graphing is going to be a single metric for a single host at a time, so our primary key should cover time, hostname, and the metric name. Since some metrics won't be on every server (i.e. our Riak servers have more disks than our Rails servers) and we want to be able to add metrics in the future, we're going to have a narrow table with only one metric per row.

Since we've decided to have our compound primary key cover time, hostname, and metric name, the only thing left to decide is the quantization.

A query can span only a certain number of quanta (configurable at the system level, default maximum of 5) to limit the impact of aggressive querying on the database. This may lead developers to use a large quantum size, but that can lead to overloaded servers in the cluster, so benchmarking and careful tuning is recommended.

You can’t use `OR` to select different non-quantized options on a field in the primary key. If you are selecting different options on a non-quantized primary key field, put them in multiple queries.

```sql
CREATE TABLE metrics (
  time TIMESTAMP NOT NULL,
  hostname VARCHAR NOT NULL,
  metric_name VARCHAR NOT NULL,
  metric_value DOUBLE,
  PRIMARY KEY (
    (hostname, metric_name, QUANTUM(time, 15, m)),
    hostname, metric_name, time))
```


### Why is the quantized column last?

The `PRIMARY KEY` declaration is made up of two pieces: the partition key and the local key. The first, `(hostname, metric_name, QUANTUM(time, 15, m))`, declares a partition key. The partition key determines which cluster members should store a given row. The quantized time means that a given partition of a Riak TS table contains rows with the exact same `hostname` and `metric_name`, and a range of time spanning fifteen minutes (in this case).

On-disk, data is stored in sorted order. Since queries must exactly match a `hostname` and `metric_name`, it's most useful to seek to the beginning of a range of those, sweeping to find the lowest timestamp, and reading records until the timestamp leaves the desired range. This allows only records matching the `hostname`, `metric_name`, and timestamp range to be read. The alternatives, where the timestamp is not last, require reading non-matching records and filtering them out after they've been read from the disk.


## How Riak TS Processes SQL Queries

Assume we have way too many computers and we aren’t sure why one of them is being unreliable. We think it’s running hot but we’re not sure, so we’re going to look at our metrics data and see whether or not it is.

In particular, we want to ask about the last forty minutes of CPU temperature data for bunnahabhain.islay with temperatures over 50°C. Our SQL query looks like:

```sql
SELECT * FROM metrics 
WHERE
  hostname='bunnahabhain.islay' AND
  metric_name='core_temp' AND
  time < 1446762821844 AND
  time > 1446760421844 AND
  metric_value > 50.0;
```

And we send the query to Riak TS. Once the query is sent to Riak TS the following happens:

1. The planner receives the query from you and parses it.
2. The parser isolates the table name, and then 
3. the parser constructs a tree out of the `WHERE` conditions, and identifies `WHERE` predicates that are connected with `AND` operations.
3. The planner then loads the table’s DDL.
4. Then the planner plucks out the `WHERE` conditions that are fields in the DDL’s primary key.
5. And then the planner uses the `WHERE` conditions and the DDL’s partition key (a quantized primary key) to calculate coverage; in this case, the three partitions that contain rows matching the query.
6. Finally, the planner creates a filter based on the `metric_value` `WHERE` condition and sends it to the three partitions.
7. Each partition loops over its data, matching rows against the filter.
8. The partitions then send their matched rows back to the planner.
9. The planner then collects the matched rows and returns them to you.
