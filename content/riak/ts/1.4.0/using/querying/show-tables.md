---
title: "SHOW TABLES in Riak TS"
description: "Using the SHOW TABLES statement in Riak TS"
menu:
  riak_ts-1.4.0:
    name: "SHOW TABLES"
    identifier: "show_tables_riakts"
    weight: 300
    parent: "querying_data_riakts"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/querying/show-tables
---

[riak shell]: {{<baseurl>}}riak/ts/1.4.0/using/riakshell

You can use the SHOW TABLES statement to enumerate the Riak TS tables you have set up. This document will show you how to execute `SHOW TABLES` in TS.

The SHOW TABLES statement returns a list of tables you've created in a single column with one row per table name.

For example:

```sql
SHOW TABLES
```

Returns:

```sql
+------------+
|   Table    |
+------------+
| RandomTable|
| GeoCheckin |
|AnotherTable|
| FinalTable |
+------------+
```

You can use `SHOW TABLES` in [riak shell]:

```
riak-shell>show tables;
+------------+
|   Table    |
+------------+
| RandomTable|
| GeoCheckin |
|AnotherTable|
| FinalTable |
+------------+
```

Using TS's supported clients, a successful `SHOW TABLE` will return a regular successful query result:

```
+------------+
|   Table    |
+------------+
| RandomTable|
| GeoCheckin |
|AnotherTable|
| FinalTable |
+------------+
```
