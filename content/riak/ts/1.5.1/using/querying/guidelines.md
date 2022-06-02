---
title: "Guidelines for Querying in Riak TS"
description: "Limitations and rules for querying in Riak TS"
menu:
  riak_ts-1.5.1:
    name: "Querying Guidelines"
    identifier: "querying_guidelines_riakts"
    weight: 500
    parent: "querying_data_riakts"
project: "riak_ts"
project_version: "1.5.1"
toc: true
aliases:
    - /riakts/1.5.1/using/querying/basic-querying
    - /riakts/1.5.1/using/querying/guidelines
---

[table arch]: ../../../learn-about/tablearchitecture/#data-modeling
[activating]: ../../creating-activating/
[writing]: ../../writingdata/
[planning]: ../../planning#column-definitions
[iso8601]: ../../../timerepresentations/
[SELECT]: {{<baseurl>}}riak/ts/1.5.1/using/querying/SELECT#iso_8601
[configuring]: ../../../configuring/riakconf/


Riak TS supports several kinds of queries of your TS data. To create the most successful queries possible, there are some guidelines and limitations you should know. 

This document will cover the basic rules of querying in Riak TS, general guidelines to help you create the best queries possible, and all current limitations impacting queries in TS.


## The Basic Rules of Querying

You query data via columns. There are three categories of column, each with a different set of rules for valid queries. Query columns are based on rows in your TS table.

```
CREATE TABLE tab2(
a SINT64 NOT NULL,
b TIMESTAMP NOT NULL,
c BOOLEAN NOT NULL,
PRIMARY KEY  ((a, QUANTUM(b, 1, 's'))<-Partition Key, a,b,c)<-Local Key)
```


### Partition Key

```
PRIMARY KEY  ((a,<-Unquantized QUANTUM(b, 1, 's')<-Quantized)...
```

All queries must cover the partition key fields and must use greater than and less than (>, >=, <, <=).

All unquantized fields in your partition key must be included in the query as exact matches.

Any quantized field in your partition key must be included in the query as a bounded range, or with equals.

* Valid: `time > 1449864277000 and time < 1449864290000`
* Invalid: `time > 1449864277000`
* Invalid: `time > 1449864277000 or time < 1449864290000`

{{% note title="A Note About `SELECT`" %}}
It is possible to use ISO 8601-compliant date/time strings rather than integer timestamps in SELECT statements. Please see [SELECT]({{<baseurl>}}riak/ts/1.5.1/using/querying/select/#iso-8601) for an example or [Time Representations]({{<baseurl>}}riak/ts/1.5.1/using/timerepresentations/) for more information.
{{% /note %}}


### Local Key


Any field in the local key but not in the partition key can be queried with any operator supported for that field's type. Bounded ranges are not required. Any filter is allowed, including `OR` and `!=`

```
PRIMARY KEY ((a,b),a,b,c)
```

Here 'c' is in the local key only so does not have to be in the query.

[Column names][planning] from the local key must be compared using strict equality against literal values. No ranges are permitted, `!=` must not be used, and `OR` will not work.

* Valid: `country_code = 'uk'`
* Invalid: `(country_code = 'uk' or country_code = 'de')`
* Invalid: `country_code != 'se'`
* Invalid: `temperature < 85.0`


### Column Definitions

[Column definitions][planning] may be queried with unbounded ranges, `!=`, and `or` comparisons.


### General Guidelines

Before you begin querying, there are some guidelines to keep in mind.

* Columns may not be compared against other columns in the query.
* When using `OR`, you must surround the expression with parentheses or your query will return an error.

Basic queries return the full range of values between two given times for an instance within a class or type of data. In the example table, `state` is an instance within the `region`:

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

## SQL Support

A small subset of SQL is supported. All columns are of the format:

```sql
Field    Operator   Constant
```

The following operators are supported for each data type:

|           |=  |!= |>  |<  |<= |>=|
|-----------|---|---|---|---|---|---|
| varchar   | X | X |   |   |   |   |
| blob      | X | X |   |   |   |   |
| boolean   | X | X |   |   |   |   |
| sint64    | X | X | X | X | X | X |
| double    | X | X | X | X | X | X |
| timestamp | X | X | X | X | X | X |


## Limitations

* Column to column comparisons are not currently supported.
* Secondary indexing (2i) will not work with Riak TS.
* Riak search will not work with Riak TS.
* Queries are limited by the number of quanta they can span when specifying the time limits.

### Blob data in queries and primary keys

Blob data should be queried using integers in base 16 (hex) notation, preceded by `0x` using riak shell or by providing any block of data (e.g. binary, text or JSON) through a Riak client library.

However, we do not recommend using blob columns in primary keys yet, due to limitations in the Riak TS 1.5 `list_keys` API.


### Query parameters

You can set [configurations in your riak.conf][configuring] to restrict or broaden your queries. There are three parameters, in particular, that will impact your queries.

The query size is limited by the `max_returned_data_size` parameter. This parameter specifies the limit of data a query may fetch. When a query is made, the size of the data it will fetch is estimated and then checked against the `max_returned_data_size` parameter. If the estimation exceeds the limit, the query is cancelled.

The `max_running_fsms` will impact how long your query will take. This parameter specifies the maximum number of subqueries that can be processed in parallel. For more information on this parameter, check out [Configuring Riak TS][configuring].

The `max_quanta_span` parameter will limit the number of quanta a query can cover.

A query covering more than than the configured quanta span will generate an error like 'Error (1025): Query spans too many quanta' and the query system will refuse to run it.

In the example DDL below we set a quantum of 1 minute in the primary key:

```sql
CREATE TABLE GeoCheckin
 (geohash VARCHAR NOT NULL,
  location VARCHAR NOT NULL,
  user VARCHAR NOT NULL,
  time TIMESTAMP NOT NULL,
  weather VARCHAR NOT NULL,
  temperature VARCHAR,
    PRIMARY KEY((location, user, QUANTUM(time, 1, 'm')),
                location, user, time))
```

With the above quantum and with the default `max_quanta_span` of 5000,  the maximum timeframe we can query at a time is going to be 5000 minutes provided that the data returned from the query wouldn’t exceed the limits set in `max_returned_data_size`.

See the Data Modeling section in [Table Architecture][table arch] for more information on selecting your quanta and setting parameters.