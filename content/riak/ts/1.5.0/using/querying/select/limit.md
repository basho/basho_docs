---
title: "LIMIT in Riak TS"
description: "Using the LIMIT clause in Riak TS."
menu:
  riak_ts-1.5.0:
    name: "LIMIT"
    identifier: "limit_riakts"
    weight: 160
    parent: "select_riakts"
project: "riak_ts"
project_version: "1.5.0"
toc: true
version_history:
  in: "1.5.0+"
canonical_link: "https://docs.basho.com/riak/ts/latest/using/querying/limit/order-by"
---

[select]: /riak/ts/1.5.0/using/querying/select
[query guidelines]: /riak/ts/1.5.0/using/querying/guidelines/

The LIMIT clause is used with [`SELECT`][select] to return a limited number of results.

This document shows how to run various queries using `LIMIT`. See the [guidelines][query guidelines] for more information on limitations and rules for queries in Riak TS.


## Overview

The LIMIT clause returns a limited number of results from a SELECT statement.

`LIMIT` has the following syntax:

```sql
LIMIT «number_rows» [ OFFSET «offset_rows» ]
```

### Options

The following can be appended to `LIMIT`:

#### `OFFSET`

Skip a specified number of results first and then return remaining results.

[Example](#offset-results)


{{% note title="WARNING" %}}
Before you run `SELECT` you must ensure the node issuing the query has adequate memory to receive the response. If the returning rows do not fit into the memory of the requesting node, the node is likely to fail.
{{% /note %}}


## Examples

The following table defines a schema for «TODO_DATA_MODEL»

```sql
CREATE TABLE «TODO_DATA_MODEL»
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

### Basic

Return only five results between «TODO_TIME» and «TODO_TIME»:

```sql
SELECT id, time, value FROM «TODO_DATA_MODEL» WHERE id = 2 AND time > «TODO_TIME» AND time < «TODO_TIME» LIMIT 5;
```

### Sort and Limit

Sort results between «TODO_TIME» and «TODO_TIME» by time in ascending order and only return 5 results:

```sql
SELECT id, time, value FROM «TODO_DATA_MODEL» WHERE id = 2 AND time > «TODO_TIME» AND time < «TODO_TIME» ORDER BY time ASC LIMIT 5;
```

### Offset Results

Skip the first two results of the query, return five:

```sql
SELECT id, time, value FROM «TODO_DATA_MODEL» WHERE id = 2 AND time > «TODO_TIME» AND time < «TODO_TIME» LIMIT 5 OFFSET 2;
```
