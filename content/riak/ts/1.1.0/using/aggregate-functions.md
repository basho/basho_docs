---
title: "Riak TS Aggregate Functions"
description: "Riak TS Aggregate Functions"
menu:
  riak_ts-1.1.0:
    name: "Aggregate Functions"
    identifier: "aggregate_functions_riakts"
    weight: 306
    parent: "using"
project: "riak_ts"
project_version: "1.1.0"
toc: true
version_history:
  in: "1.1.0+"
aliases:
    - /riakts/1.1.0/using/aggregate-functions/
---

[arithmetic]: ../arithmetic-operations

You can turn a set of rows in your Riak TS table into a value with the aggregate feature. This document will walk you through the functions that make up aggregation in Riak TS.

>**Note:** At the moment, you cannot combine [arithmetic functions][arithmetic] with aggregation functions.

## Aggregate Functions

* `COUNT()` - Returns the number of entries that match a specified criteria.
* `SUM()` - Returns the sum of entries that match a specified criteria.
* `MEAN()` & `AVG()` - Returns the average of entries that match a specified criteria.
* `MIN()` - Returns the smallest value of entries that match a specified criteria.
* `MAX()` - Returns the largest value of entries that match a specified criteria.
* `STDDEV()` - Returns the statistical standard deviation of all entries that match a specified criteria using Population Standard Deviation.

### `AVG` & `MEAN`

Mean average over the specified `sint64` or `double` column.

```sql
SELECT AVG(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

Returns `NULL` if no values were returned or all values were `NULL`.

| Column Input Type | Return Type |
|------------|-------------|
| sint64 | sint64 |
| double | double |

### `COUNT`

Count the number of returned values.

```sql
SELECT COUNT(*) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

If a single column is used as an input then `NULL` values are ignored. If all values were `NULL` or no rows were returned by the query then `NULL` is returned.

| Column Input Type | Return Type |
|------------|-------------|
| Any | sint64 |
| `*` | sint64 |

### `MAX`

The largest value from the set of values returned by the query.

```sql
SELECT MAX(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

Returns `NULL` if no values were returned or all values were `NULL`.

| Column Input Type | Return Type |
|------------|-------------|
| sint64 | sint64 |
| double | double |

### `MIN`

The smallest value from the set of values returned by the query.

```sql
SELECT MIN(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

Returns `NULL` if no values were returned or all values were `NULL`.

| Column Input Type | Return Type |
|------------|-------------|
| sint64 | sint64 |
| double | double |

### `STDDEV`

Calculate the standard deviation for a set of values returned by the query.

>**Note:** We use population standard deviation for our calculations.

```sql
SELECT STDDEV(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

Returns `NULL` if less than two non-null values were returned.

| Column Input Type | Return Type |
|------------|-------------|
| sint64 | double |
| double | double |

### `SUM`

The sum of all the values of one `sint64` or `double` column returned by the query.

```sql
SELECT SUM(temperature) FROM GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

Returns `NULL` if no values were returned or all values were `NULL`.

| Column Input Type | Return Type |
|------------|-------------|
| sint64 | sint64 |
| double | double |
