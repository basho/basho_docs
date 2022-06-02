---
title: "EXPLAIN in Riak TS"
description: "Using the EXPLAIN statement in Riak TS"
menu:
  riak_ts-1.5.0:
    name: "EXPLAIN"
    identifier: "explain_riakts"
    weight: 400
    parent: "querying_data_riakts"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/using/querying/explain
---

[creating-activating]: {{<baseurl>}}riak/ts/1.5.0/using/creating-activating
[develop]: {{<baseurl>}}riak/ts/1.5.0/developing
[planning]: {{<baseurl>}}riak/ts/1.5.0/using/planning
[riak shell]: {{<baseurl>}}riak/ts/1.5.0/using/riakshell

You can use the EXPLAIN statement to better understand how a query you would like to run will be executed. This document will show you how to use `EXPLAIN` in Riak TS.

## EXPLAIN Guidelines

`EXPLAIN` takes a SELECT statement as an argument and shows information about each subquery. The constraints placed upon the WHERE clause in the SELECT statement determine the subquery values. 

The details about each subquery include:

* A subquery number which is a unique integer,
* The range scan start key which shows the value of the fields at the beginning of that quantum,
* A flag indicating if that flag is inclusive (less than or greater than or equal),
* The range scan end key,
* Another inclusive flag for the end key, and
* The value of the filter, i.e. constrained columns which are not a part of the partition key

To use `EXPLAIN`, the table in question must first be [defined][planning] and [activated][creating-activating]. Only metadata about the desired query is returned.

## EXPLAIN Example

Assuming our standard example table, GeoCheckin, has been created:

```sql
CREATE TABLE GeoCheckin
(
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (region, state, QUANTUM(time, 15, 'm')),
     region, state, time
   )
)
```

We can run `EXPLAIN` in riak shell as follows:

```
riak-shell>EXPLAIN SELECT * FROM GeoCheckin WHERE myfamily = 'family1' AND myseries = 'series1' AND time >= 2 AND time <= 7000000 AND weather='fair';
+--------+----------------------------------------------------------+-------------------+----------------------------------------------------------+-----------------+------------------+
|Subquery|                   Range Scan Start Key                   |Is Start Inclusive?|                    Range Scan End Key                    |Is End Inclusive?|      Filter      |
+--------+----------------------------------------------------------+-------------------+----------------------------------------------------------+-----------------+------------------+
|   1    |   myfamily = 'family1', myseries = 'series1', time = 2   |       true        |myfamily = 'family1', myseries = 'series1', time = 900000 |      false      |(weather = 'fair')|
|   2    |myfamily = 'family1', myseries = 'series1', time = 900000 |       true        |myfamily = 'family1', myseries = 'series1', time = 1800000|      false      |(weather = 'fair')|
|   3    |myfamily = 'family1', myseries = 'series1', time = 1800000|       true        |myfamily = 'family1', myseries = 'series1', time = 2700000|      false      |(weather = 'fair')|
|   4    |myfamily = 'family1', myseries = 'series1', time = 2700000|       true        |myfamily = 'family1', myseries = 'series1', time = 3600000|      false      |(weather = 'fair')|
|   5    |myfamily = 'family1', myseries = 'series1', time = 3600000|       true        |myfamily = 'family1', myseries = 'series1', time = 4500000|      false      |(weather = 'fair')|
|   6    |myfamily = 'family1', myseries = 'series1', time = 4500000|       true        |myfamily = 'family1', myseries = 'series1', time = 5400000|      false      |(weather = 'fair')|
|   7    |myfamily = 'family1', myseries = 'series1', time = 5400000|       true        |myfamily = 'family1', myseries = 'series1', time = 6300000|      false      |(weather = 'fair')|
|   8    |myfamily = 'family1', myseries = 'series1', time = 6300000|       true        |myfamily = 'family1', myseries = 'series1', time = 7000000|      true       |(weather = 'fair')|
+--------+----------------------------------------------------------+-------------------+----------------------------------------------------------+-----------------+------------------+
```

Note that riak shell fits output into the current window so the results might be truncated.

Using `EXPLAIN` in the [TS-supported clients][develop] works exactly as described above and returns the same results.
