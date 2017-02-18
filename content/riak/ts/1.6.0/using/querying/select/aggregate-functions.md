---
title: "Riak TS Aggregate Functions"
description: "Riak TS Aggregate Functions"
menu:
  riak_ts-1.6.0:
    name: "Aggregate Functions"
    identifier: "aggregate_functions_riakts"
    weight: 305
    parent: "select_riakts"
project: "riak_ts"
project_version: "1.6.0"
toc: true
canonical_link: "https://docs.basho.com/riak/ts/latest/using/querying/select/aggregate-functions"
version_history:
  present_from: "1.4.0+"
  moved:
    - ["1.1.0+", "using/aggregate-functions"]
aliases:
    - /riakts/1.6.0/using/aggregate-functions/
    - /riak/ts/1.6.0/using/aggregate-functions/
    - /riakts/1.6.0/using/querying/select/aggregate-functions/
---


[arithmetic]: ../arithmetic-operations


You can turn a set of rows in your Riak TS table into a value with the aggregate feature. This document will walk you through the functions that make up aggregation in Riak TS.


## Aggregate Functions

* `COUNT` - Returns the number of entries that match specified criteria.
* `SUM` - Returns the sum of entries that match specified criteria.
* `MEAN` & `AVG` - Returns the average of entries that match specified criteria.
* `MIN` - Returns the smallest value of entries that match specified criteria.
* `MAX` - Returns the largest value of entries that match specified criteria.
* `STDDEV`/`STDDEV_SAMP` - Returns the statistical standard deviation of all entries that match specified criteria using Sample Standard Deviation.
* `STDDEV_POP` - Returns the statistical standard deviation of all entries that match specified criteria using Population Standard Deviation.
* `PERCENTILE_DISC`/`PERCENTILE_CONT` - Return a given percentile value in the observations represented by entries in the selection, assuming discrete/continuous distrinution model.
* `MEDIAN` - equivalent to `PERCENTILE_DISC` called with 0.5 as a parameter.
* `MODE` - Returns the mode of the population represented by entries in the selection.

{{% note title="A Note On Negation" %}}
You cannot negate an aggregate function. If you attempt something like: `select -count(temperature)`, you will receive an error. Instead, you can achieve negation with `-1*`; for instance: `-1*COUNT(...)`.
{{% /note %}}


### `AVG` & `MEAN`

Calculate the mean average over the specified `sint64` or `double` column.

```sql
SELECT AVG(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND region = 'South Atlantic' AND state = 'South Carolina'
```

Returns `NULL` if no values were returned or all values were `NULL`.

| Column Input Type | Return Type |
|-------------------|-------------|
| sint64            | sint64 |
| double            | double |


### `COUNT`

Count the number of returned values.

```sql
SELECT COUNT(*) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND region = 'South Atlantic' AND state = 'South Carolina'
```

If a single column is used as an input then `NULL` values are ignored. If all values were `NULL` or no rows were returned by the query then `NULL` is returned.

| Column Input Type | Return Type |
|-------------------|-------------|
| Any               | sint64 |
| `*`               | sint64 |


### `MAX`

The largest value from the set of values returned by the query.

```sql
SELECT MAX(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND region = 'South Atlantic' AND state = 'South Carolina'
```

Returns `NULL` if no values were returned or all values were `NULL`.

| Column Input Type | Return Type |
|-------------------|-------------|
| sint64            | sint64 |
| double            | double |


### `MIN`

The smallest value from the set of values returned by the query.

```sql
SELECT MIN(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND region = 'South Atlantic' AND state = 'South Carolina'
```

Returns `NULL` if no values were returned or all values were `NULL`.

| Column Input Type | Return Type |
|-------------------|-------------|
| sint64            | sint64 |
| double            | double |


### `STDDEV`, `STDDEV_SAMP` & `STDDEV_POP`

Calculate the standard deviation for the set of values returned by the query.

```sql
SELECT STDDEV(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND region = 'South Atlantic' AND state = 'South Carolina'
```

Returns `NULL` if less than two non-null values were returned.

| Column Input Type | Return Type |
|-------------------|-------------|
| sint64            | double |
| double            | double |


### `SUM`

The sum of all the values of one `sint64` or `double` column returned by the query.

```sql
SELECT SUM(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND region = 'South Atlantic' AND state = 'South Carolina'
```

Returns `NULL` if no values were returned or all values were `NULL`.

| Column Input Type | Return Type |
|-------------------|-------------|
| sint64            | sint64 |
| double            | double |

### `PERCENTILE_DISC`, `PERCENTILE_CONT`

Calculate a percentile, given as a value in the range [0..1], in a population of entries in the selection, with null values discarded.  The column argument must be of numeric type (`sint64`, `double` or `timestamp`).  Return type is the type of the column argument for `PERCENTILE_DISC`, and `double` for `PERCENTILE_CONT`.

The `_DISC`/`_CONT` variants differ in the discrete/continuous distribution model assumed for the population.  For the former, the value returned is the largest observation that is less than or equal to the percentile computed.  For the latter, the result is the linear interpolation between two observations surrounding the percentile.

```sql
SELECT PERCENTILE_DISC(x, 0.3), PERCENTILE_CONT(x, 0.3) FROM Table WHERE ...
```

### MEDIAN

Equivalent to `PERCENTILE_DISC(x, 0.5)`.

### MODE

Calculate the mode (i.e., the value occurring with the highest frequency) of observations in the selection, with nulls discarded.  If there are more than one modes, the lowest one is returned.  The column argument must be of numeric type (`sint64`, `double` or `timestamp`).  Return type is the type of the column argument.

{{% note title="Notes on inverse distribution functions" %}}
1. These functions cannot be used in conjunction with `ORDER BY` or `GROUP BY` clauses, or with any other column specifiers.

2. Multiple inverse distrinution function calls are permitted as long as they all have the same column argument.

3. Inverse distrinution functions use query buffers.  Queries with large selection size may incur increased latency depending on the value of `riak_kv.query.timeseries.qbuf_inmem_max` in your riak.conf and the I/O throughput of storage backing up query buffers (`riak_kv.query.timeseries.qbuf_root_path`).
{{% /note %}}
