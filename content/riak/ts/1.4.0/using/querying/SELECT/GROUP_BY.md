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
    - /riakts/1.4.0/using/querying/select/group_by
canonical_link: "https://docs.basho.com/riak/ts/latest/using/querying/select/group_by"
---

[aggregate]: ../../../aggregate-functions
[guidelines]: riak/ts/1.4.0/using/querying/guidelines

The GROUP BY statement is used with `SELECT` to pick out and condense rows sharing the same value and return a single row. `GROUP BY` is useful for aggregating an attribute of a device over a time period; for instance, you could use it to pull average values for every 30 minute period over the last 24 hours.

This document will show you how to run various queries using `GROUP BY`. See the [guidelines] for more information on limitations and rules for queries in TS.

 
## GROUP BY Basics

`GROUP BY` returns a single row for each unique combination of values for columns specified in the GROUP BY statement. 

The SELECT statement must contain only the columns specified in `GROUP BY`. Columns not used as groups can appear as function parameters.

The [aggregate] function may be used with the GROUP BY statement. If used, `SELECT` may contain the columns specified in either `GROUP BY` or the [aggregate] function.

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

### 

/* Columns do not have to be specified, any combination aggregate functions
   can be used */
SELECT COUNT(name)
FROM tasks
WHERE completed > 1 AND completed < 1000 AND name = 'wheels' AND project = 'Mars Rover'
GROUP BY project, name

###

/* The column "completed" is not in the group clause but can be used
   as a function argument */
SELECT project, COUNT(completed)
FROM tasks
WHERE completed > 1 AND completed < 1000 AND name = 'wheels' AND project = 'Mars Rover'
GROUP BY project


/* the grouping column doesn't have to be in the key */
SELECT AVG(duration)
FROM tasks
WHERE completed > 1 AND completed < 1000 AND name = 'wheels' AND project = 'Mars Rover'
GROUP BY subtask
```


There is no guaranteed order for rows returned by `GROUP BY`, `ORDER BY` provides this.

The `SELECT` statement must contain only the columns specified in the `GROUP BY` and [aggregate] functions. 

### Technical Documentation

`GROUP BY` can be described as a dictionary of aggregates. When no `GROUP BY` is specified but an aggregate function is, only one row is returned. For example:

```sql
SELECT COUNT(project)
FROM table1
WHERE completed > 1 AND completed < 1000 AND name = 'datacentre' AND project = 'Mars Rover'
GROUP BY project
```

When a `GROUP BY` is specified, one aggregate is returned per group. Internally the coordinator maintains a dictionary, the key is the group by values and the value is the aggregate.



The `GROUP BY` key is a list of cell values, Given the the row and query, for the `tasks` table.

|   name  | project | completed |
|---------|---------|-----------|
| groupby | TS      |      5000 |

```sql
SELECT name, project
FROM tasks
GROUP BY name, project
```

The `GROUP BY` key for the row would be `[<<"groupby">>, <<"TS">>]`.

### Overload Protection

There is no special overload protection for queries using GROUP BY. It is not possible to put them into temporary tables because the accumulated groups all need to be in memory to be group on when processing new rows.