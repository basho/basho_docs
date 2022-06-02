---
title: "SQL Reference"
description: "Available SQL statements in Riak TS"
menu:
  riak_ts-1.5.1:
    name: "SQL Reference"
    identifier: "query_ref"
    weight: 90
    parent: "querying_data_riakts"
project: "riak_ts"
project_version: "1.5.1"
toc: true
version_history:
  in: "1.5.1+"
aliases:
    - /riakts/1.5.1/using/querying/reference
---

[select]: {{<baseurl>}}riak/ts/1.5.1/using/querying/select/
[describe]: {{<baseurl>}}riak/ts/1.5.1/using/querying/describe/
[delete]: {{<baseurl>}}riak/ts/1.5.1/using/querying/delete/
[explain]: {{<baseurl>}}riak/ts/1.5.1/using/querying/explain/
[show tables]: {{<baseurl>}}riak/ts/1.5.1/using/querying/show-tables/
[create table]: {{<baseurl>}}riak/ts/1.5.1/using/creating-activating/
[group by]: {{<baseurl>}}riak/ts/1.5.1/using/querying/select/group-by/
[order by]: {{<baseurl>}}riak/ts/1.5.1/using/querying/select/order-by/
[limit]: {{<baseurl>}}riak/ts/1.5.1/using/querying/select/limit/
[offset]: {{<baseurl>}}riak/ts/1.5.1/using/querying/select/
[arithmetic]: {{<baseurl>}}riak/ts/1.5.1/using/querying/select/arithmetic-operations/
[aggregate]: {{<baseurl>}}riak/ts/1.5.1/using/querying/select/aggregate-functions/

This document lists each SQL statement available in Riak TS.

## DESCRIBE

The DESCRIBE statement returns the table’s information in rows and columns.

`DESCRIBE` has the following syntax:

```sql
DESCRIBE «table_name»
```

See the [DESCRIBE in Riak TS][describe] page for more information and usage examples.

## SELECT

The SELECT statement is used to query Riak TS data sets. All queries using `SELECT` must include a WHERE clause with all components.

`SELECT` has the following syntax:

```sql
SELECT «column_name» FROM «table_name» WHERE «thing» [, ...]
```

See the [SELECT in Riak TS][select] page for more information and usage examples.

## DELETE

The DELETE statement is used to delete records.

`DELETE` has the following syntax:

```sql
DELETE FROM «table_name» WHERE «column_name = value»
```

See the [DELETE in Riak TS][delete] page for more information and usage examples.

## EXPLAIN

The EXPLAIN statement is used to better understand how a query will be executed.

`EXPLAIN` has the following syntax:

```sql
EXPLAIN «query»
```

For example:

```sql
EXPLAIN SELECT * FROM «table_name» WHERE «column_name» = «value»
```

See the [EXPLAIN in Riak TS][explain] page for more information and usage examples.

## SHOW TABLES

The SHOW TABLES statement returns a list of tables you’ve created in a single column with one row per table name.

`SHOW TABLES` has the following syntax:

```sql
SHOW TABLES
```

See the [SHOW TABLES in Riak TS][show tables] page for more information and usage examples.

## CREATE TABLE

The CREATE TABLE statement creates a table for storing records.

`CREATE TABLE` has the following syntax:

```sql
CREATE TABLE «table_name» ()
```

For example:

```sql
CREATE TABLE ExampleTable
(
   id           SINT64    NOT NULL,
   value        VARCHAR   NOT NULL,
   PRIMARY KEY (
     (id, QUANTUM(time, 15, 'm')),
      id, time
   )
)
```

See the [Creating and Activating Tables][create table] page for more information and usage examples.

## GROUP BY

The GROUP BY statement is used with `SELECT` to pick out and condense rows sharing the same value, then return a single row.

`GROUP BY` has the following syntax:

```sql
GROUP BY «column_name»
```

For example:

```sql
SELECT «column_name» FROM «table_name» WHERE «column_name» = «value» GROUP BY «column_name»;
```

See the [GROUP BY in Riak TS][group by] page for more information and usage examples.

## ORDER BY

The ORDER BY clause is used with `SELECT` to sort results by one or more columns in ascending or descending order.

`ORDER BY` has the following syntax:

```sql
ORDER BY «column_name» [ ASC | DESC ] [ NULLS { FIRST | LAST } ] [, ...]
```

For example:

```sql
SELECT «column_name» FROM «table_name» WHERE «column_name» = «value» ORDER BY «column_name»;
```

See the [ORDER BY in Riak TS][order by] page for more information and usage examples.

## LIMIT

The LIMIT clause is used with `SELECT` to return a limited number of results.

`LIMIT` has the following syntax:

```sql
LIMIT «number_rows» [ OFFSET «offset_rows» ]
```

For example:

```sql
SELECT «column_name» FROM «table_name» WHERE «column_name» = «value» LIMIT 5;
```

See the [LIMIT in Riak TS][limit] page for more information and usage examples.

## OFFSET

The OFFSET clause is used with `SELECT` to skip a specified number of results then return remaining results.

`OFFSET` has the following syntax:

```sql
OFFSET «integer»
```

For example:

```sql
SELECT «column_name» FROM «table_name» WHERE «column_name» = «value» LIMIT 5 OFFSET 2;
```

See the [OFFSET in Riak TS][offset] page for more information and usage examples.

## NULLS FIRST

The NULLS FIRST modifier is used with `SELECT` and `ORDER BY` to sort null values before all non-null values.

```sql
SELECT «column_name» FROM «table_name» WHERE «column_name» = «value» ORDER BY «column_name» DESC, «column_name» NULLS FIRST;
```

## NULLS LAST

The NULLS LAST modifier is used with `SELECT` and `ORDER BY` to sort null values after all non-null values.

```sql
SELECT «column_name» FROM «table_name» WHERE «column_name» = «value» ORDER BY «column_name» DESC, «column_name» NULLS LAST;
```

## Arithmetic Operations

Riak TS supports arithmetic operations in SELECT statements; such as addition (`+`), subtraction (`-`), multiplication (`*`), division (`/`). See the [Riak TS Arithmetic Operations][arithmetic] page for more information and usgae examples.

## Aggregate Functions

Riak TS also supports aggregate functions in SELECT statements:

- `COUNT` - Returns the number of entries that match a specified criteria.
- `SUM` - Returns the sum of entries that match a specified criteria.
- `MEAN` / `AVG` - Returns the average of entries that match a specified criteria.
- `MIN` - Returns the smallest value of entries that match a specified criteria.
- `MAX` - Returns the largest value of entries that match a specified criteria.
- `STDDEV` / `STDDEV_SAMP` - Returns the statistical standard deviation of all entries that match a specified criteria using Sample Standard Deviation.
- `STDDEV_POP` - Returns the statistical standard deviation of all entries that match a specified criteria using Population Standard Deviation.

See the [Aggregate Functions][aggregate] page for more information and usage examples.
