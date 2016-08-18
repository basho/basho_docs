---
title: "Planning Your Riak TS Table"
description: "Planning Your Riak TS Table"
menu:
  riak_ts-1.4.0:
    name: "Plan Your Table"
    identifier: "planning_riakts_table"
    weight: 301
    parent: "using"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/planning/
canonical_link: "https://docs.basho.com/riak/ts/latest/using/planning"
---


[activating]: ../creating-activating/
[table arch]: ../../learn-about/tablearchitecture/
[bestpractices]: ../../learn-about/bestpractices/
[epoch]: https://en.wikipedia.org/wiki/Unix_time
[installing]: ../../setup/installing/
[sql]: ../../learn-about/sqlriakts/


Now that you've [installed][installing] Riak TS, you're almost ready to create a TS table. Before you can create your table, you'll need to plan it out.

This page provides a basic overview of what you'll need and some guidelines/limitations. For a deeper dive into planning and designing Riak TS tables, check out [Table Architecture][table arch].

Riak TS tables are closely tied to SQL tables. If you are unfamiliar with SQL or would like to know more about how Riak TS integrates SQL, check out [SQL for Riak TS][sql].


## Anatomy of a Schema

In order to create a working Riak TS table, you'll need to plan your table out. Once created, your table cannot be changed. Here is an example Riak TS `CREATE TABLE` statement (broken across many lines for clarity):

```sql
CREATE TABLE GeoCheckin
(
   id           SINT64    NOT NULL,
   time         TIMESTAMP NOT NULL,
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   weather      VARCHAR   NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (id, QUANTUM(time, 15, 'm')),
      id, time
   )
)
```

While the keywords appear in all uppercase letters here, they can be specified using lowercase or uppercase letters as they are not case sensitive.

{{% note title="Table Limitations" %}}
You cannot create a table with more than 511 total [column definitions](#column-definitions) and [column names](#primary-key). If you try to create a table with more than 511 columns, you will receive an error.
{{% /note %}}


#### Column Definitions

Column definitions are the lines preceding the `PRIMARY KEY` in the example. Column definitions define the structure of the data. They are comprised of three parts: a column name, a data type, and (optionally) an inline constraint. 

```sql
column_name data_type [NOT NULL],
```

Column names (`region`, `state`, etc) must be ASCII strings, in addition to having the correct case. If column names need to contain spaces or punctuation they can be double quoted.

Any column names specified as part of the primary key must be defined as `NOT NULL`.

The column definitions for the keys can be specified in any order in the `CREATE TABLE` statement. For instance both are correct:

**A.**
```sql
CREATE TABLE GeoCheckin
(
   id           SINT64    NOT NULL,
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR   NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (id, QUANTUM(time, 15, 'm')),
      id, time
   )
)
```

**B.**
```sql
CREATE TABLE GeoCheckin
(
   time         TIMESTAMP NOT NULL,
   id           SINT64    NOT NULL,
   state        VARCHAR   NOT NULL,
   weather      VARCHAR   NOT NULL,
   region       VARCHAR   NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (id, QUANTUM(time, 15, 'm')),
      id, time
   )
)
```

The data types in column definitions are limited. Valid types are:

* `VARCHAR` - Any string content is valid, including Unicode. Can only be compared using strict equality, and will not be typecast (e.g., to an integer) for comparison purposes. Use single quotes to delimit varchar strings.
* `BOOLEAN` - `true` or `false` (any case)
* `TIMESTAMP` - Timestamps are integer values expressing [UNIX epoch time in UTC][epoch] in milliseconds. Zero is not a valid timestamp.
* `SINT64` - Signed 64-bit integer
* `DOUBLE` - This type does not comply with its IEEE specification: `NaN` (not a number) and `INF` (infinity) cannot be used.


### Primary Key

The `PRIMARY KEY` describes both the partition key and local key. The partition key is a prefix of the local key, consisting of one or more column names. The local key must begin with the same column names as the partition key, but may also contain additional column names.

```sql
CREATE TABLE GeoCheckin
(
   id           SINT64    NOT NULL,
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR   NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (id, QUANTUM(time, 15, 'm')),  <-- PARTITION KEY
      id, time                      <-- LOCAL KEY
   )
)
```

The field definitions for the `PRIMARY KEY` can be specified in any order in the `CREATE TABLE` statement. For instance both are correct:

**A.**
```sql
CREATE TABLE GeoCheckin
(
   id           SINT64    NOT NULL,
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR   NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (id, time,
      id, time, state
   )
)
```

**B.**
```sql
CREATE TABLE GeoCheckin
(
   id           SINT64    NOT NULL,
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR   NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (id, time,
      id, time, region
   )
)
```


#### Partition Key

The partition key is the first element of the primary key, and is defined as a list of  column names in parentheses. The partition key must have at least one column name.

You may specify a quantum, which is used to colocate data on one of the partition key's timestamp fields:

```sql
PRIMARY KEY  ((id, QUANTUM(time, 1, 's')), ...)
```

Only one quantum function may be specified and it must be the last element of the partition key.

The quantum function takes 3 parameters:

* the name of a field in the table definition of type `TIMESTAMP`
* a quantity as a positive integer, greater than zero.
* a unit of time:
  * `'d'` - days
  * `'h'` - hours
  * `'m'` - minutes
  * `'s'` - seconds

A general guideline to get you started if you are not sure how best to structure your partition key is to first choose a column name that represents a class or type of data, and then choose a  second column name represents is a more specific instance(s) of the class/type.

#### Local Key

The local key comes after the partition key. It must first contain the same column names in the same order as the partition key. This ensures that the same column names determining your data's partition also dictate the sorting of the data within that partition.

The local key may also contain additional column names so long as they come after the column names present in the partition key. For example:

```sql
   PRIMARY KEY (
     (id, QUANTUM(time, 15, 'm')),
      id, time, weather, temperature
   )
```


## Schema Discovery

After creating a table, its schema can be discovered with the `DESCRIBE` statement:

```sql
DESCRIBE GeoCheckin
```

The `DESCRIBE` statement will return the following:

* **Column**, column name;
* **Type**, data type;
* **Is Null**, _true_ if the field is optional, _false_ otherwise;
* **Primary Key**, position of this field in the primary key, or blank if it does not appear in the key;
* **Local Key**, position of this field in the local key, or blank if it does not appear in the key.


## More information

Still unsure how best to structure your Riak TS table? Check out our [best practice recommendations][bestpractices].

Confused about column definition, primary key, etc? Check out [Table Architecture][table arch] for an in-depth explanation of TS tables.


## Next Steps

Now that you know how your Riak TS table will be structured, you can move on to [creating and activating your table][activating].
