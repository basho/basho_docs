---
title: "Planning Your Riak TS Table"
description: "Planning Your Riak TS Table"
menu:
  riak_ts-1.4.0:
    name: "Plan Your Table"
    identifier: "planning_riakts_table"
    weight: 300
    parent: "using"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/planning/
---


[activating]: ../creating-activating/
[table arch]: ../../learn-about/tablearchitecture/
[bestpractices]: ../../learn-about/bestpractices/
[describe]: ../querying/describe/
[epoch]: https://en.wikipedia.org/wiki/Unix_time
[installing]: ../../setup/installing/
[sql]: ../../learn-about/sqlriakts/


Now that you've [installed][installing] Riak TS, you're almost ready to create a TS table. Before you can create your table, you'll need to plan it out.

This page provides a basic overview of what you'll need and some guidelines/limitations. For more information about what the components of a Riak TS table do, check out [Table Architecture][table arch].

Riak TS tables are closely tied to SQL tables. If would like to know more about how Riak TS integrates SQL, check out [SQL for Riak TS][sql].


## TS Table Schema

In order to create a working Riak TS table, you'll need to plan your table out. Once created, your table cannot be changed. Here is an example Riak TS CREATE TABLE statement (broken across many lines for clarity):

```sql
CREATE TABLE GeoCheckin
(
  region       VARCHAR   NOT NULL,
  state        VARCHAR   NOT NULL,
  time         TIMESTAMP NOT NULL,
  weather      VARCHAR   NOT NULL,
  temperature  DOUBLE,
  PRIMARY KEY (
    (region, state, QUANTUM(time, 15, 'm')),
    region, state, time
  )
)
```

A TS table is made up of:

* a table name, GeoCheckin in this example,
* [column definitions](#column-definitions), the first 6 lines after the table name in this example, and
* [the primary key](#primary-key), which is everything following `PRIMARY KEY` in this example.

The column definitions will determine the columns of your TS table, while the primary key determines where data is stored in your TS cluster.

Keywords are case sensitive, so be sure to capitalize appropriately.

{{% note title="Table Limitations" %}}
You cannot create a table with more than 511 total columns. If you try to create a table with more than 511 columns, you will receive an error.
{{% /note %}}


### Column Definitions

Column definitions define the structure of the data and are comprised of three parts: a column name, a data type, and (optionally) an inline constraint.

```sql
column_name data_type [NOT NULL],
```

Column names (`region`, `state`, etc) must be ASCII strings, in addition to having the correct case. If column names need to contain spaces or punctuation they can be double quoted.

Any column names specified as part of the [primary key](#primary-key) must be defined as `NOT NULL`.

The column definitions for the keys can be specified in any order in the CREATE TABLE statement. For instance both are correct:

**A.**
```sql
CREATE TABLE GeoCheckin
CREATE TABLE GeoCheckin
(
  region       VARCHAR   NOT NULL,
  state        VARCHAR   NOT NULL,
  time         TIMESTAMP NOT NULL,
  weather      VARCHAR   NOT NULL,
  temperature  DOUBLE,
  PRIMARY KEY (
    (region, state, QUANTUM(time, 15, 'm')),
    region, state, time
  )
)
```

**B.**
```sql
CREATE TABLE GeoCheckin
(
   time         TIMESTAMP NOT NULL,
   state        VARCHAR   NOT NULL,
   weather      VARCHAR   NOT NULL,
   region       VARCHAR   NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (region, state, QUANTUM(time, 15, 'm')),
     region, state, time
   )
)
```

The data types in column definitions are limited. Valid types are:

* `VARCHAR` - Any string content is valid, including Unicode. Can only be compared using strict equality, and will not be typecast (e.g., to an integer) for comparison purposes. Use single quotes to delimit varchar strings.
* `BOOLEAN` - `true` or `false` (any case).
* `TIMESTAMP` - Integer values expressing [UNIX epoch time in UTC][epoch] in milliseconds. Zero is not a valid timestamp.
* `SINT64` - Signed 64-bit integer.
* `DOUBLE` - This type does not comply with its IEEE specification: `NaN` (not a number) and `INF` (infinity) cannot be used.


### Primary Key

The `PRIMARY KEY` describes both the partition key and local key. The partition key is a prefix of the local key, consisting of one or more column names in parentheses. The local key must begin with the same column names as the partition key, but may also contain additional column names.

```sql
CREATE TABLE GeoCheckin
(
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR   NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (region, state, QUANTUM(time, 15, 'm')),  <-- PARTITION KEY
     region, state, time                       <-- LOCAL KEY
   )
)
```

The field definitions for the `PRIMARY KEY` can be specified in any order in the CREATE TABLE statement. For instance both are correct:

**A.**
```sql
CREATE TABLE GeoCheckin
(
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR   NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (region, state, QUANTUM(time, 15, 'm')),
      region, state, time
   )
)
```

**B.**
```sql
CREATE TABLE GeoCheckin
(
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR   NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (state, region, time,
      state, region, time, weather
   )
)
```


#### Partition Key

The partition key is the first element of the primary key, and must have at least one column name.

A general guideline is to first choose a column name that represents a class or type of data, and then choose a second column name that is a more specific instance(s) of the class/type.

You may specify a quantum, which is used to colocate data on one of the partition key's timestamp fields:

```sql
PRIMARY KEY  ((region, state, QUANTUM(time, 1, 's')), ...)
```

If you choose to specify a quantum, you may specify only one and it must be the last element of the partition key.

The quantum function takes 3 parameters:

* the name of a field in the table definition of type `TIMESTAMP`
* a quantity as a positive integer, greater than zero
* a unit of time:
  * `'d'` - days
  * `'h'` - hours
  * `'m'` - minutes
  * `'s'` - seconds


#### Local Key

The local key comes after the partition key. It must first contain the same column names in the same order as the partition key. This ensures that the same column names determining your data's partition also dictate the sorting of the data within that partition.

The local key may also contain additional column names so long as they come after the column names present in the partition key. For example:

```sql
   PRIMARY KEY (
     (region, state, QUANTUM(time, 15, 'm')),
      region, state, time, weather, temperature
   )
```

{{% note title="On Key Uniqueness" %}}

The Local Key in a Riak TS row is what makes that row's key/address unique from other rows.
In the examples on this page and others we've used a composite key of
`region, state, time` because it can model different devices and groupings.  
If you have another identifier, such as a device ID, that you can guarantee to be unique when combined with a timestamp, you can have a shorter key definition.

The table definition for such a schema would be as follows:

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

The omission of `region` and `state` from the key definition makes it shorter, and will also make any SQL queries shorter because we'll only need a minimum of id/time in our queries `WHERE` clauses (see [Table Architecture](../../learn-about/tablearchitecture/) and [Querying Guidelines](../querying/guidelines/) for all the specifics about how different partition + local key layouts change the way you query data).  

The downside to this schema is that you'll likely need to do one query per device, instead of being able to group multiple devices together based on their other defining characteristics such as region & state.

Please take care in defining how you address your unique data, as it will affect how you will query it in the future.
{{% /note %}}

## More information

After creating a table, its schema can be discovered with the [DESCRIBE statement][describe].

Still unsure how best to structure your Riak TS table? Check out our [best practice recommendations][bestpractices].

Confused about what column definitions, primary key, etc. do? Check out [Table Architecture][table arch] for an in-depth explanation of TS tables.


## Next Steps

Now that you know how your Riak TS table will be structured, you can move on to [creating and activating your table][activating].
