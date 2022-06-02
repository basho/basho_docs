---
title: "DELETE in Riak TS"
description: "Using the DELETE statement in Riak TS"
menu:
  riak_ts-1.5.1:
    name: "DELETE"
    identifier: "delete_riakts"
    weight: 110
    parent: "querying_data_riakts"
project: "riak_ts"
project_version: "1.5.1"
toc: true
version_history:
  in: "1.5.1+"
aliases:
    - /riakts/1.5.1/using/querying/delete
---

[query guidelines]: {{<baseurl>}}riak/ts/1.5.1/using/querying/guidelines/
[time rep]: {{<baseurl>}}riak/ts/1.5.1/using/timerepresentations/
[http delete]: {{<baseurl>}}riak/ts/1.4.0/using/writingdata/#deleting-data

# DELETE

The DELETE statement is used to delete records.

This document shows how to delete records using `DELETE`. See the [guidelines][query guidelines] for more information on limitations and rules for queries in Riak TS.


## Overview

The DELETE statement removes whole records matching a WHERE clause and a given timestamp from a specified table. It is not possible to remove separate columns or indices from a record.

`DELETE` has the following syntax:

```sql
DELETE FROM «table_name» WHERE column1 = value1 [AND column2 = value ...] AND { time = t | time op t1 AND time op t2 }, where op = { >, <, >=, <= }
```

The WHERE clause in `DELETE` should include all columns comprising `PRIMARY KEY` in the table definition.

Timestamp values can be provided as milliseconds or in [supported ISO 8601 formats][time rep]. `DELETE` acts as a single-key delete, like the [HTTP DELETE command][http delete].


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

### Single Record

Delete a single record:

```sql
DELETE FROM SensorData WHERE id = 3 AND time = '2016-11-28 06:10:03';
```
