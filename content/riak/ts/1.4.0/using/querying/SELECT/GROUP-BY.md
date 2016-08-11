---
title: "GROUP BY in Riak TS"
description: "Using the GROUP BY statement in Riak TS"
menu:
  riak_ts-1.4.0:
    name: "GROUP BY"
    identifier: "group_by_riakts"
    weight: 100
    parent: "SELECT"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/querying/select/group-by
canonical_link: "https://docs.basho.com/riak/ts/latest/using/querying/select/group-by"
---

[aggregate function]: ../aggregate-functions
[guidelines]: riak/ts/1.4.0/using/querying/guidelines

The GROUP BY statement is used with `SELECT` to pick out and condense rows sharing the same value and return a single row. `GROUP BY` is useful for aggregating an attribute of a device over a time period; for instance, you could use it to pull average values for every 30 minute period over the last 24 hours.

This document will show you how to run various queries using `GROUP BY`. See the [guidelines] for more information on limitations and rules for queries in TS.

 
## GROUP BY Basics

`GROUP BY` returns a single row for each unique combination of values for columns specified in the GROUP BY statement. There is no guaranteed order for the returned rows. 

The SELECT statement must contain only the columns specified in `GROUP BY`. Columns not used as groups can appear as function parameters.

The [aggregate function] may be used with the GROUP BY statement. If used, `SELECT` may contain the columns specified in either `GROUP BY` or the [aggregate function].

{{% note title="WARNING" %}}
Before you run `GROUP BY` you must ensure the node issuing the query has adequate memory to receive the response. If the returning rows do not fit into the memory of the requesting node, the node is likely to fail. 
{{% /note %}}


## GROUP BY Examples 

The following table defines a schema for tasks, including which project they are part of and when they were completed.

```sql
CREATE TABLE tasks (
    name VARCHAR NOT NULL,
    project VARCHAR NOT NULL,
    completed TIMESTAMP NOT NULL,
    subtask VARCHAR NOT NULL,
    duration SINT64 NOT NULL,
    PRIMARY KEY((name,project,quantum(completed,1,'m')),name,project,completed)
);
```

### Basic GROUP BY

The query below returns one column per unique project and counts how many rows have the same project.

```sql
SELECT project, COUNT(name)
FROM tasks
WHERE completed > 1 AND completed < 1000 AND name = 'wheels' AND project = 'Mars Rover'
GROUP BY project
```

### More than one group

You can group as many columns as you choose, and the order of the grouping has no effect. 

The query below returns one column per unique project, name combination, and counts how many rows have the same project, name combination.
 
```sql
SELECT project, COUNT(name)
FROM tasks
WHERE completed > 1 AND completed < 1000 AND name = 'wheels' AND project = 'Mars Rover'
GROUP BY project, name
```

### Timestamps in a group

{{% note title="Warning" %}}
Using a timestamp in the group can produce large number of groups due to the likelihood of the timestamps being unique.
{{% /note %}}

You can include the timestamp (or quantized column) in the group.

The query below returns one column per unique project, name, timestamp combination.

```sql
SELECT project, name, completed
FROM tasks
WHERE completed > 1 AND completed < 1000 AND name = 'wheels' AND project = 'Mars Rover'
GROUP BY project, name, completed
```

### Using combination aggregate functions

If you use a combination [aggregate function] (`COUNT()`, `SUM()`, `MEAN()`, `AVG()`, `STDDEV()`, `STDDEV_SAMP()`, and `STDDEV_POP()`) with the SELECT statement, you can specify the column in the aggregate function alone, or in both the `SELECT` and the aggregate function.

This is allowed because the values returned can be grouped into one value: the result of the COUNT function.

The query below returns one column per unique project, name combination and counts how many rows have the same name.

```sql
SELECT COUNT(name)
FROM tasks
WHERE completed > 1 AND completed < 1000 AND name = 'wheels' AND project = 'Mars Rover'
GROUP BY project, name
```

The query below returns one column per unique project and counts how many rows have the same timestamp.

```sql
SELECT project, COUNT(completed)
FROM tasks
WHERE completed > 1 AND completed < 1000 AND name = 'wheels' AND project = 'Mars Rover'
GROUP BY project
```

### 

/* the grouping column doesn't have to be in the key */
SELECT AVG(duration)
FROM tasks
WHERE completed > 1 AND completed < 1000 AND name = 'wheels' AND project = 'Mars Rover'
GROUP BY subtask
```