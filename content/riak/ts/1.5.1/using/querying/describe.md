---
title: "DESCRIBE in Riak TS"
description: "Using the DESCRIBE statement in Riak TS"
menu:
  riak_ts-1.5.1:
    name: "DESCRIBE"
    identifier: "describe_riakts"
    weight: 200
    parent: "querying_data_riakts"
project: "riak_ts"
project_version: "1.5.1"
toc: true
aliases:
    - /riakts/1.5.1/using/querying/describe
---

[riak shell]: {{<baseurl>}}riak/ts/1.5.1/using/riakshell

You can use the DESCRIBE statement to obtain the definition of your Riak TS table. This document will show you how to execute `DESCRIBE` in TS.

The DESCRIBE statement returns the table's information in rows and columns.

For example:

```sql
DESCRIBE GeoCheckin
```

Returns:

```
Column      | Type      | Nullable | Partition Key | Local Key | Interval | Unit | Sort Order
---------------------------------------------------------------------------------|-----------
region      | varchar   | false    | 1             | 1         |          |      |
state       | varchar   | false    | 2             | 2         |          |      |
time        | timestamp | false    | 3             | 3         | 15       | m    |
weather     | varchar   | false    | <null>        | <null>    |          |      |
temperature | double    | true     | <null>        | <null>    |          |      |
```


You can use `DESCRIBE` in [riak shell]:

```
riak-shell>describe GeoCheckin;
+-----------+---------+--------+-------------+---------+--------+----+----------+
|  Column   |  Type   |Nullable|Partition Key|Local Key|Interval|Unit|Sort Order|
+-----------+---------+--------+-------------+---------+--------+----+----------+
|  region   | varchar | false  |     1       |    1    |        |    |           |
|   state   | varchar | false  |     2       |    2    |        |    |           |
|   time    |timestamp| false  |     3       |    3    |   15   | m  |           |
|  weather  | varchar | false  |             |         |        |    |           |
|temperature| double  | true   |             |         |        |    |           |
+-----------+---------+--------+-------------+---------+--------+----+----------+
```


Using TS's supported clients, a successful `DESCRIBE` will return a language-specific representation of the table.

* **Java** - Use a `Query` command to execute a DESCRIBE statement.
* **Ruby** - Use the `Riak::TimeSeries::Query` object to execute the DESCRIBE statement. The returned results will have a collection of rows as well as a `columns` property corresponding to the above table.
* **Python** - either the `ts_query` or `ts_describe` methods of the client object can be used to executed a DESCRIBE statement. In both cases, the response object will have `columns` and `rows` properties corresponding to the above table.
* **C#** - Use a `Query` command to execute a DESCRIBE statement.
* **Node.js** - you may use the `TS.Query` command to execute a DESCRIBE statement, or use the purpose-built `TS.Describe` command. In both cases, the response object will have `columns` and `rows` properties corresponding to the above table.
