---
title: SQL for Riak TS
project: riakts
version: 1.0.0+
document: reference
toc: true
index: true
audience: beginner
---

Riak TS tables are closely tied to SQL tables. If you are unfamiliar with SQL or would like to know more about how Riak TS integrates SQL, check out [SQL for Riak TS][sql].


##SQL Basics
Riak TS supports a subset of Structured Query Language: in particular, SELECT statements for querying (called Data Manipulation Language, Data Query Language, DML, or DQL in places), and CREATE TABLE statements for defining Time Series storage regions/bucket types (also known as Data Definition Language, or DDL).

Riak SQL supports several kinds of literals: numbers, single-quoted strings, and time. It also supports double-quoted column references.
This document uses CAPITAL LETTERS for SQL keywords, although they’re not necessary. This document also breaks out SQL queries into multiple lines for readability; you’re welcome to write a query as a single line if you wish.

##Schema Design
Let's say you want to store and graph usage metrics on a heterogeneous collection of network servers: disk usage, memory usage, CPU temperatures, load averages, and other metrics. Our graphing is going to be a single metric for a single host at a time, so our primary key should cover time, hostname, and the metric name. Since some metrics won't be on every server (i.e. our Riak KV servers have more disks than our Puma servers), and we want to be able to add metrics in the future, we're going to have a "narrow" table, with only one metric per row. 

Since we've decided to have our compound primary key cover time, hostname, and metric name, the only thing left to decide is the quantization. Right now, a query can only cover three quanta, so quantization comes down to how much time we want in a single graph. 

You can’t use “OR” to select different non-quantized options on a field in the primary key. If you are selecting different options on a non-quantized primary key field, put them in multiple queries.

CREATE TABLE metrics (
  time TIMESTAMP NOT NULL, 
  hostname VARCHAR NOT NULL, 
  metric_name VARCHAR NOT NULL, 
  metric_value DOUBLE, 
  PRIMARY KEY (
    (hostname, metric_name, QUANTUM(time, 15, m)),
    hostname, metric_name, time))
### Why the quantized column is last
The “PRIMARY KEY” declaration is made up of two pieces: the first, (hostname, metric_name, QUANTUM(time, 15, m)), declares a partition key. A partition key determines which cluster members should store a given row. The quantized time means that a given partition of a time series table contains rows with the exact same hostname and metric_name, and a range of time spanning fifteen minutes (in this case).
##How Riak TS Processes Queries
Assume we have way too many computers, and we aren’t sure why one of them is being unreliable. We think it’s running hot, but we’re not sure, so we’re going to look at our metrics data and see if it is or not.
In particular, we want to ask about “the last forty minutes of CPU temperature data for bunnahabhain.islay with temperatures over 50°C.” In SQL:
```sql
SELECT * FROM metrics 
WHERE
  hostname=’bunnahabhain.islay’ AND
  metric_name=’core_temp’ AND
  time < 1446762821844 AND
  time > 1446760421844 AND
  metric_value > 50;
```
We send this to Riak TS.

1. The planner receives the query from you, and parses it
2. The parser isolates the table name and ANDed together WHERE conditions
3. The planner loads the table’s DDL
4. The planner plucks out the WHERE conditions that are fields in the DDL’s primary key
5. The planner uses the WHERE conditions and the DDL’s partition key (a quantized primary key) to calculate coverage; in this case, the three partitions that contain rows matching the query
6. The planner creates a filter based on the metric_value WHERE condition and sends it to the three partitions
7. Each partition loops over its data, matching rows against the filter
8. The partitions send their matched rows back to the planner
9. The planner collects the matched rows and returns them to you, the user


##Query Requirements
Riak TS 1.0 has somewhat strict requirements for a query:

* All elements of the compound primary key must be present
* Queries can only cover five partitions
* All clauses must be in “ColumnName Comparison Literal” order
* Non-quantized primary key elements must be compared for equality
* Timestamp values are expressed as epoch milliseconds


##Querying
Must cover all components of compound key
Must hit five or fewer quanta 
Must check equality of non-quantized key components
For a normalized table, not worth not getting all columns
SELECT * FROM metrics
WHERE
  hostname = 'pancake' AND
  metric_name = 'temp_cpu' AND
  time > 1445865879000 AND
  time < 1445866000000

Due to parser limitations in 1.0, all WHERE comparisons must be ColumnReference ComparisonOperator Literal format, or Comparison BooleanOperator Comparison format. Comparing column values to column values is not yet supported.

Timestamp values should be expressed as epoch milliseconds.

##Key requirements

##What's not supported
Grouping and aggregations
Big integer/big decimal types

