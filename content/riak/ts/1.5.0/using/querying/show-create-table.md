---
title: "SHOW CREATE TABLE in Riak TS"
description: "Using the SHOW CREATE TABLE statement in Riak TS"
menu:
  riak_ts-1.5.0:
    name: "SHOW CREATE TABLE"
    identifier: "show_create_table_riakts"
    weight: 310
    parent: "querying_data_riakts"
project: "riak_ts"
project_version: "1.5.0"
toc: true
version_history:
  in: "1.5.0+"
aliases:
    - /riakts/1.5.0/using/querying/show-create-table
---

[riak shell]: {{<baseurl>}}riak/ts/1.5.0/using/riakshell

You can use the SHOW CREATE TABLE statement to obtain the SQL used to create your Riak TS table. This document will show you how to execute `SHOW CREATE TABLE` in TS.

The SHOW CREATE TABLE statement returns the table's information as well as many bucket properties as a SQL string.

For example:

```sql
SHOW CREATE TABLE ExampleTable
```

Returns: 

```sql
-------------------------------------------------------------------------------
CREATE TABLE ExampleTable (
somechars VARCHAR NOT NULL,
somebool BOOLEAN NOT NULL,
sometime TIMESTAMP NOT NULL,
somefloat DOUBLE,
PRIMARY KEY ((somechars, somebool, QUANTUM(sometime, 1, 'h')),
somechars, somebool, sometime))
WITH (active = true,
allow_mult = true,
dvv_enabled = true,
dw = quorum,
last_write_wins = false,
n_val = 2,
notfound_ok = true,
postcommit = '',
pr = 0,
pw = 0,
r = quorum,
rw = quorum,
w = quorum)
```


You can use `SHOW CREATE TABLE` in [riak shell]:

```
riak-shell>show create table ExampleTable;
CREATE TABLE ExampleTable (
somechars VARCHAR NOT NULL,
somebool BOOLEAN NOT NULL,
sometime TIMESTAMP NOT NULL,
somefloat DOUBLE,
PRIMARY KEY ((somechars, somebool, QUANTUM(sometime, 1, 'h')),
somechars, somebool, sometime))
WITH (active = true,
allow_mult = true,
dvv_enabled = true,
dw = quorum,
last_write_wins = false,
n_val = 2,
notfound_ok = true,
postcommit = '',
pr = 0,
pw = 0,
r = quorum,
rw = quorum,
w = quorum)
```


Using TS's supported clients, a successful `SHOW CREATE TABLE` will return a language-specific representation of the table's SQL.

* **Java** - Use a `Query` command to execute a SHOW CREATE TABLE statement.
* **Ruby** - Use the `Riak::TimeSeries::Query` object to execute the SHOW CREATE TABLE statement. The returned results will have a collection of a single `rows` as well as a `columns` property corresponding to the creation SQL.
* **Python** - The `ts_query` method of the client object can be used to executed a SHOW CREATE TABLE statement. In both cases, the response object will have a single `columns` and `rows` property corresponding to the creation SQL.
* **C#** - Use a `Query` command to execute a SHOW CREATE TABLE statement.
* **Node.js** - You may use the `TS.Query` command to execute a SHOW CREATE TABLE. In both cases, the response object will have a single `columns` and `rows` property corresponding to the create SQL.
