---
title: "ORDER BY in Riak TS"
description: "Using the ORDER BY statement in Riak TS."
menu:
  riak_ts-1.5.0:
    name: "ORDER BY"
    identifier: "order_by_riakts"
    weight: 150
    parent: "select_riakts"
project: "riak_ts"
project_version: "1.5.0"
toc: true
version_history:
  in: "1.5.0+"
canonical_link: "https://docs.basho.com/riak/ts/latest/using/querying/select/order-by"
---

[select]: /riak/ts/1.5.0/using/querying/select
[query guidelines]: /riak/ts/1.5.0/using/querying/guidelines/

The ORDER BY statement is used with [`SELECT`][select] to sort results by one or more columns in ascending or descending order. `ORDER BY` is useful for operations such as returning the most recent results in a set.

This document shows how to run various queries using `ORDER BY`. See the [guidelines][query guidelines] for more information on limitations and rules for queries in Riak TS.

{{% note title="Warning" %}}
The initial implementation of `ORDER BY` in SELECT queries uses on-disk query buffer. It adds some overhead which increases the query latency, sometimes significantly.
{{% /note %}}

## Overview

The ORDER BY statement sorts results according to the specified column(s) and any optional keywords or clauses used.

`ORDER BY` has the following syntax:

```sql
ORDER BY column_name [ ASC | DESC ] [ NULLS { FIRST | LAST } ] [, ...]
```

During an `ORDER BY` sort if two rows are equal according to the leftmost column, they are compared according to the next column, and so on.

### Options

The following keywords can be appended to `ORDER BY` to further sort results: 

#### `ASC`

Sort results in ascending order. This is the default if no order is specified.

`NULLS LAST` is the default when `ASC` is specified or implied.

{{% note title="Relationship of ASC/DESC qualifiers in `ORDER BY` vs DDL" %}}
A DESC qualifier (express or implied) used in the DDL has no effect on the sorting order applied during `ORDER BY`. That is, given a DDL where column X appears in the primary key definition with a DESC qualifier, a `SELECT .. ORDER BY _X_` (where `ASC` is implied) will still have X ASC-ordered.
 {{% /note %}}
 
[Example](#ascending)

#### `DESC`

Sort results in descending order.

`NULLS FIRST` is the default when `DESC` is specified.

[Example](#descending)

#### `NULLS FIRST`

Null values will be sorted before all non-null values.

[Example](#null-values-first)

#### `NULLS LAST`

Null values will be sorted after all non-null values.

[Example](#null-values-last)

#### `LIMIT`

Only return a specified number of results.

[Example](#limit-results)

#### `OFFSET`

Skip a specified number of results first and then return remaining results.

[Example](#offset-results)


{{% note title="WARNING" %}}
Before you run `SELECT` you must ensure the node issuing the query has adequate memory to receive the response. If the returning rows do not fit into the memory of the requesting node, the node is likely to fail.
{{% /note %}}


## Examples

The following table defines a schema for sensor data.

```sql
CREATE TABLE SensorData
(
   id     SINT64    NOT NULL,
   time   TIMESTAMP NOT NULL,
   value  DOUBLE,
   PRIMARY KEY (
     (id, QUANTUM(time, 15, 'm')),
      id, time
   )
)
```

### Ascending

Sort results between given times, by time, in ascending order:

```sql
SELECT id, time, value FROM SensorData WHERE id = 2 AND time > '2016-11-28 06:00:00' AND time < '2016-11-28 06:10:10' ORDER BY time ASC;
```

Alternatively, because `ORDER BY` sorts in ascending by default:

```sql
SELECT id, time, value FROM SensorData WHERE id = 2 AND time > '2016-11-28 06:00:00' AND time < '2016-11-28 06:10:10' ORDER BY time;
```

### Descending

Sort results between given times, by time, in descending order:

```sql
SELECT id, time, value FROM SensorData WHERE id = 2 AND time > '2016-11-28 06:00:00' AND time < '2016-11-28 06:10:10' ORDER BY time DESC;
```

### Multiple Columns

Sort results by multiple columns:

```sql
SELECT id, time, value FROM SensorData WHERE id = 2 AND time > '2016-11-28 06:00:00' AND time < '2016-11-28 06:10:10' ORDER BY value DESC, time ASC LIMIT 5;
```

### Null Values First

Sort results and return null values first:

```sql
SELECT id, time, value FROM SensorData WHERE id = 2 AND time > '2016-11-28 06:00:00' AND time < '2016-11-28 06:10:10' ORDER BY time DESC, id ASC, value NULLS FIRST;
```

### Null Values Last

Sort results and return null values last:

```sql
SELECT id, time, value FROM SensorData WHERE id = 2 AND time > '2016-11-28 06:00:00' AND time < '2016-11-28 06:10:10' ORDER BY time DESC, id ASC, value NULLS LAST;
```

### Limit Results

Sort results between given times, by time, in ascending order and only return 5 results:

```sql
SELECT id, time, value FROM SensorData WHERE id = 2 AND time > '2016-11-28 06:00:00' AND time < '2016-11-28 06:10:10' ORDER BY time ASC LIMIT 5;
```

### Offset Results

Skip first 2 results and return the results after:

```sql
SELECT id, time, value FROM SensorData WHERE id = 2 AND time > '2016-11-28 06:00:00' AND time < '2016-11-28 06:10:10' ORDER BY time ASC LIMIT 5 OFFSET 2;
```
