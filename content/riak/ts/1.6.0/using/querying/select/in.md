---
title: "IN in Riak TS"
description: "Using the IN keyword in Riak TS."
menu:
  riak_ts-1.6.0:
    name: "IN"
    identifier: "in_riakts"
    weight: 170
    parent: "select_riakts"
project: "riak_ts"
project_version: "1.6.0"
toc: true
version_history:
  in: "1.6.0+"
canonical_link: "https://docs.basho.com/riak/ts/latest/using/querying/select/in/"
---

[select]: /riak/ts/1.6.0/using/querying/select/
[query guidelines]: /riak/ts/1.6.0/using/querying/guidelines/

The IN keyword is used with [`SELECT`][select] to return results where a specified column matches one or more given values.

This document shows how to run various queries using `IN`. See the [guidelines][query guidelines] for more information on limitations and rules for queries in Riak TS.

## Overview

The IN keyword returns results from a SELECT statement which match one or more literal values.

`IN` has the following syntax:

```sql
SELECT * FROM «table_name» WHERE «column_name» IN («values»)
```

{{% note title="WARNING" %}}
Before you run `SELECT` you must ensure the node issuing the query has adequate memory to receive the response. If the returning rows do not fit into the memory of the requesting node, the node is likely to fail.
{{% /note %}}

## Example

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

### Basic

Return only results where the value column matches the given values:

```sql
SELECT * FROM SensorData WHERE value IN (1.2, 3.4, 5.6);
```
