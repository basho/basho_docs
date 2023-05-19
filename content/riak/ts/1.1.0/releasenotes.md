---
title: "Riak TS Release Notes"
description: "Riak TS 1.1.0 Release Notes"
menu:
  riak_ts-1.1.0:
    name: "Release Notes"
    identifier: "release_notes"
    weight: 101
    parent: "introduction"
project: "riak_ts"
project_version: "1.1.0"
toc: true
aliases:
    - /riakts/1.1.0/releasenotes/
---

Released January 14, 2016.

This release builds on Riak TS 1.0.0 to enable further analysis of time series data with aggregates and arithmetic functionality.

## New Features

### Aggregations

In Riak TS 1.0.0 you could run a` WHERE` clause that returned a particular row set of time series data. In 1.1.0 you can apply a function (such as `COUNT`) in the `SELECT` clause that operates on those responses in aggregate.

For instance,

```
SELECT fun(X) FROM tablename WHERE
```

Where `fun` is one of:

* `SUM`
* `COUNT`
* `AVG` / `MEAN`
* `MIN`
* `MAX`
* `STDDEV`

And where is (X) is either a column name, or a multi-column expression avg(temperature/pressure).

### Arithmetic

Riak TS now also supports arithmetic operations in the `SELECT` list. The arithmetic operations available in this release are: Numeric Literals, Addition, Subtraction, Multiplication, Division, and Negation.

* +
* -
* /
* *
* (
* )

For example,

```
SELECT 555, 1.1, 1e1, 1.123e-2 from GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

>**Important:** Arithmetic operations and aggregate functions cannot be mixed in a single value expression.

### Dynamic Schema Discovery

You can now query a table definition with the `DESCRIBE` table query which returns the table's rows and columns.

For example:

```sql
DESCRIBE GeoCheckin
```

Returns (Rows and Columns):

```
Column      | Type      | Is Null | Partition Key | Local Key
--------------------------------------------------------
myfamily    | varchar   | false   | 1             | 1
myseries    | varchar   | false   | 2             | 2
time        | timestamp | false   | 3             | 3
weather     | varchar   | false   | <null>        | <null>
temperature | double    | false   | <null>        | <null>
```

### Create Tables via Clients

You can create tables using `CREATE TABLE`. Simply execute your `CREATE TABLE` command in a query, and it will be created.

```sql
CREATE TABLE GeoCheckin
(
   myfamily    varchar   not null,
   myseries    varchar   not null,
   time        timestamp not null,
   weather     varchar   not null,
   temperature double,
   PRIMARY KEY (
     (myfamily, myseries, quantum(time, 15, 'm')),
     myfamily, myseries, time
   )
)
```

A successful table creation will return nothing, and an exception response will be returned if the attempt was unsuccessful.

## Compatibility

Riak TS is compatible with the following operating systems:

* RHEL/CentOS 6
* RHEL/CentOS 7
* Ubuntu 12.04 LTS
* Ubuntu 14.04 LTS
* Debian 7 (development only)
* OSX 10.8+ (development only)

## Known Issues

* AAE must be turned off.
* Riak Search is not supported.
* Multi-Datacenter Replication is not supported.
* Arithmetic operations and aggregates cannot currently be combined.
