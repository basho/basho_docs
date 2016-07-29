---
title: "SQL in Riak TS"
description: "Using SQL in Riak TS"
menu:
  riak_ts-1.4.0:
    name: "SQL in Riak TS"
    identifier: "sql_riakts"
    weight: 120
    parent: "querying"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/querying/sql
canonical_link: "https://docs.basho.com/riak/ts/latest/using/querying/sql"
---

**NEEDS AN INTRO**


## SQL Support

{% note title="SQL Injection" %}

When querying with user-supplied data, it is essential that you protect against SQL injection. Please verify the user-supplied data before constructing queries.

{{% /note %}}

A small subset of SQL is supported. All columns are of the format:

```sql
Field    Operator   Constant
```

The following operators are supported for each data type:

|           |=  |!= |>  |<  |<= |>=|
|-----------|---|---|---|---|---|---|
| varchar   | X | X |   |   |   |   |
| boolean   | X | X |   |   |   |   |
| sint64    | X | X | X | X | X | X |
| double    | X | X | X | X | X | X |
| timestamp | X | X | X | X | X | X |


### Limitations

* Column to column comparisons are not currently supported.
* Secondary indexing (2i) will not work with Riak TS.
* Riak Search will not work with Riak TS.
* Queries are limited by the number of quanta they can span when specifying the time limits.


#### Quanta query range

A query covering more than a certain number of quanta (5 by default) will generate the error `too_many_subqueries` and the query system will refuse to run it. Assuming a default quantum of 15 minutes, the maximum query time range is 75 minutes.

In the below example we set a quantum of 15s:

```sql
CREATE TABLE GeoCheckin
 (geohash VARCHAR NOT NULL,
  location VARCHAR NOT NULL,
  user VARCHAR NOT NULL,
  time TIMESTAMP NOT NULL,
  weather VARCHAR NOT NULL,
  temperature VARCHAR,
    PRIMARY KEY((location, user, QUANTUM(time, 15, 's')),
                location, user, time))
```

The maximum time range we can query is 60s, anything beyond will fail.

See the Data Modeling section in [Table Architecture][table arch] for more information.


#### Leap seconds and quantum boundaries

Periodically [leap seconds](https://en.wikipedia.org/wiki/Leap_second)
are announced. These are inserted at the end of one day (in UTC).

UNIX treats them as one double-length second. For example, at the end of 1998 a second was added:

```
Date         Time of day   UNIX time
1998-12-31   23:59:58      915148798
1998-12-31   23:59:59      915148799
1998-12-31   23:59:60      915148800     <== Artificial leap second
1999-01-01   00:00:00      915148800
```

Effectively, there is no way in the UNIX time scheme to differentiate an event that occurred during the extra second at the end of 1998 to something that occurred the first second of 1999.

Similarly, Riak TS would treat `915148800` as the start of a new time quantum, and any data points which a client added for that second would be considered to be in the first time quantum in 1999.

The data is not lost, but a query against 1998 time quanta will not produce those data points despite the fact that some of the events flagged as `915148800` technically occurred in 1998.
