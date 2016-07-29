---
title: "Basic Querying in Riak TS"
description: "The basics of querying data in Riak TS"
menu:
  riak_ts-1.4.0:
    name: "Basic Querying"
    identifier: "basic_querying_riakts"
    weight: 110
    parent: "querying"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/querying/basic-querying
canonical_link: "https://docs.basho.com/riak/ts/latest/using/querying/basic-querying"
---

[table arch]: ../../learn-about/tablearchitecture
[activating]: ../creating-activating/
[writing]: ../writingdata/
[planning]: ../planning#column-definitions


Now that you have [created][activating] a Riak TS table and [written][writing] data to it, you can query your data.
**NEEDS AN INTRO BETTER THAN THAT ^**


## Basic Querying

You query data via columns. There are three categories of column, each with a different set of rules for valid queries. Query columns are based on rows in your TS table.

```
CREATE TABLE tab2(
a SINT64 NOT NULL,
b TIMESTAMP NOT NULL,
c BOOLEAN NOT NULL,
PRIMARY KEY  ((a, QUANTUM(b, 1, 's'))<-Partition Key, a,b,c)<-Local Key)
```


### Partition Key

All queries must cover the partition key fields and must use greater than and less than (>, >=, <, <=).

All unquantized fields in your partition key must be included in the query as exact matches.

Any quantized field in your partition key must be included in the query as either an exact match or a bounded range.

* Valid: `time > 1449864277000 and time < 1449864290000`
* Invalid: `time > 1449864277000`
* Invalid: `time > 1449864277000 or time < 1449864290000`


### Local Key

For any field in the local key that is not in the partition key, your
query can include any operator supported for that field's type and
bounded ranges are not required.

```
PRIMARY KEY ((a,b),a,b,c)
```

Here 'c' is in the local key only so does not have to be in the query.

Column names from the local key must be compared using strict equality against literal values. No ranges are permitted, `!=` must not be used, and `or` will not work.

* Valid: `country_code = 'uk'`
* Invalid: `(country_code = 'uk' or country_code = 'de')`
* Invalid: `country_code != 'se'`
* Invalid: `temperature < 85.0`


### Column Definitions

[Column definitions][planning] may be queried with unbounded ranges, `!=`, and `or` comparisons.


### General Guidelines

Before you begin querying, there are some guidelines to keep in mind.

* Columns may not be compared against other columns in the query.
* When using `or`, you must surround the expression with parentheses or your query will return an error.

Basic queries return the full range of values between two given times for an instance within a class or type of data. To demonstrate, we'll use the same example table, in which the `state` is an instance within the `region`:

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

Your query must include all components of the partition key. If any part of the partition key is missing, you will get an error.

