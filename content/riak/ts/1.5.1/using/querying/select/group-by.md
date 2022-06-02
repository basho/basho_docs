---
title: "GROUP BY in Riak TS"
description: "Using the GROUP BY statement in Riak TS"
menu:
  riak_ts-1.5.1:
    name: "GROUP BY"
    identifier: "group_by_riakts"
    weight: 100
    parent: "select_riakts"
project: "riak_ts"
project_version: "1.5.1"
toc: true
aliases:
    - /riakts/1.5.1/using/querying/select/group-by
---

[aggregate function]: ../aggregate-functions
[guidelines]: {{<baseurl>}}riak/ts/1.5.1/using/querying/guidelines

The GROUP BY statement is used with `SELECT` to pick out and condense rows sharing the same value and return a single row. `GROUP BY` is useful for aggregating an attribute of a device over a time period; for instance, you could use it to pull average values for every 30 minute period over the last 24 hours.

This document will show you how to run various queries using `GROUP BY`. See the [guidelines] for more information on limitations and rules for queries in TS.

 
## GROUP BY Basics

`GROUP BY` returns a single row for each unique combination of values for columns specified in the GROUP BY statement. There is no guaranteed order for the returned rows. 

The SELECT statement must contain only the columns specified in `GROUP BY`. Columns not used as groups can appear as function parameters. The GROUP BY statement works on all rows, not just the values in the partition key, so all columns are available.

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
GROUP BY project;
```

### More than one group

You can group as many columns as you choose, and the order of the grouping has no effect. 

The query below returns one column per unique project, name combination, and counts how many rows have the same project, name combination.
 
```sql
SELECT project, COUNT(name)
FROM tasks
WHERE completed > 1 AND completed < 1000 AND name = 'wheels' AND project = 'Mars Rover'
GROUP BY project, name;
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
GROUP BY project, name, completed;
```

### Using combination aggregate functions

If you use a combination [aggregate function][aggregate function] (`COUNT()`, `SUM()`, `MEAN()`, `AVG()`, `STDDEV()`, `STDDEV_SAMP()`, and `STDDEV_POP()`) with the SELECT statement, you can specify the column in the aggregate function alone, or in both the `SELECT` and the aggregate function.

This is allowed because the values returned can be grouped into one value: the result of the COUNT function.

The query below returns one column per unique project, name combination and counts how many rows have the same name.

```sql
SELECT COUNT(name)
FROM tasks
WHERE completed > 1 AND completed < 1000 AND name = 'wheels' AND project = 'Mars Rover'
GROUP BY project, name;
```

The query below returns one column per unique project and counts how many rows have the same timestamp.

```sql
SELECT project, COUNT(completed)
FROM tasks
WHERE completed > 1 AND completed < 1000 AND name = 'wheels' AND project = 'Mars Rover'
GROUP BY project;
```

### GROUP BY columns not in the partition key

Since the GROUP BY statement works on all rows, all columns are available, which means that using `GROUP BY` on the partition key values is not very useful because the partition key limits the result set.

If we create the following table:

```sql
CREATE TABLE tasks2 (
userid VARCHAR NOT NULL, 
visits SINT64, 
a_time TIMESTAMP NOT NULL, PRIMARY KEY(userid, a_time));
```

And and try to run the GROUP BY statement including `userid` in the SELECT statement:

```sql
SELECT userid, SUM(visits)
FROM tasks2
WHERE userid = 'roddy' AND a_time > 1 AND a_time < 10000
GROUP BY userid;
```

The result set would only have the group 'roddy' because it is required by the WHERE clause.

If, however, we combine two column names from the partition key in the group using `SUM` without specifying `userid`, `GROUP BY` will return multiple result rows for the `userid` 'roddy' with one column per visit.