---
title: "Planning Your Riak TS Table"
description: "Planning Your Riak TS Table"
menu:
  riak_ts-1.3.0:
    name: "Plan Your Table"
    identifier: "planning_riakts_table"
    weight: 301
    parent: "using"
project: "riak_ts"
project_version: "1.3.0"
toc: true
aliases:
    - /riakts/1.3.0/using/planning/
canonical_link: "docs.basho.com/riak/ts/latest/using/planning"
---


[activating]: ../creating-activating/
[table arch]: ../../learn-about/tablearchitecture/
[bestpractices]: ../../learn-about/bestpractices/
[epoch]: https://en.wikipedia.org/wiki/Unix_time
[installing]: ../../installing/
[sql]: ../../learn-about/sqlriakts/


Now that you've [installed][installing] Riak TS, you're almost ready to create a TS table. Before you can create your table, you'll need to plan it out.

This page provides a basic overview of what you'll need and some guidelines/limitations. For a deeper dive into planning and designing Riak TS tables, check out [Table Architecture][table arch].

Riak TS tables are closely tied to SQL tables. If you are unfamiliar with SQL or would like to know more about how Riak TS integrates SQL, check out [SQL for Riak TS][sql].


## Anatomy of a Schema

In order to create a working Riak TS table, you'll need to plan your table out. Once created, your table cannot be changed. Here is an example Riak TS `CREATE TABLE` statement (broken across many lines for clarity):

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

In the SQL statements shown throughout this document, keywords appear in all uppercase letters to distinguish them from other entities of the statement such as table and column names.  However, keywords can be specified using lowercase or uppercase letters as they are not case sensitive.


#### Fields

Fields, also called columns, refer to the items preceding the `PRIMARY KEY`. Field names (`region`, `state`, etc) must be ASCII strings, in addition to having the correct case. If field names need to contain spaces or punctuation they can be double quoted.

Field names define the structure of the data, taking the format:

```sql
name type [NOT NULL],
```

Fields specified as part of the primary key must be defined as `NOT NULL`.

The field definitions for the keys can be specified in any order in the `CREATE TABLE` statement. For instance both are correct:

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
   time         TIMESTAMP NOT NULL,
   state        VARCHAR   NOT NULL,
   weather      VARCHAR   NOT NULL,
   region       VARCHAR   NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (QUANTUM(time, 15, 'm'), state, region),
      time, state, region
   )
)
```

The types associated with fields are limited. Valid types are:

* `VARCHAR` - Any string content is valid, including Unicode. Can only be compared using strict equality, and will not be typecast (e.g., to an integer) for comparison purposes. Use single quotes to delimit varchar strings.
* `BOOLEAN` - `true` or `false` (any case)
* `TIMESTAMP` - Timestamps are integer values expressing [UNIX epoch time in UTC][epoch] in **milliseconds**. Zero is not a valid timestamp.
* `SINT64` - Signed 64-bit integer
* `DOUBLE` - This type does not comply with its IEEE specification: `NaN` (not a number) and `INF` (infinity) cannot be used.


### Primary Key

The `PRIMARY KEY` describes both the partition and local keys. The partition key is a prefix of the local key, consisting of one or more fields. The local key must begin with the same fields as the partition key, but may have more fields.

```sql
CREATE TABLE GeoCheckin
(
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR   NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (region, state, QUANTUM(time, 15, 'm')), /* <-- PARTITION KEY */
      region, state, time /* <-- LOCAL KEY */
   )
)
```

The field definitions for the `PRIMARY KEY` can be specified in any order in the `CREATE TABLE` statement. For instance both are correct:

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
     (state, region, QUANTUM(time, 15, 'm')),
      state, region, time
   )
)
```


#### Partition Key

The partition key is the first key, and is defined as the named fields in parentheses. The partition key must have **at least one** field.

You can use a quantum to colocate data on one of the partition key's timestamp fields:

```sql
  PRIMARY KEY  ((region, state, QUANTUM(time, 1, 's')), ...)
```

The timestamp field can occur at any point in the partition key. For example, this is snippet is also valid:

```sql
  PRIMARY KEY  ((QUANTUM(time, 1, 's'), region, state), ...)
```

The quantum function takes 3 parameters:

* the name of a field in the table definition of type `TIMESTAMP`
* a quantity
* a unit of time:
  * 'd'  - days
  * 'h' - hours
  * 'm' - minutes
  * 's' - seconds

There may only be one quantum in the partition key. The quantum must be a positive integer over '0' or there will be errors.


#### Local Key

The local key comes after the partition key. It **must first contain the same fields in the same order** as the partition key. This ensures that the same fields determining your data's partition also dictate the sorting of the data within that partition.

The local key may also contain additional fields so long as they come after the fields present in the partition key.

```sql
   PRIMARY KEY (
     (region, state, QUANTUM(time, 15, 'm')),
      region, state, time, weather, temperature
   )
```


## Schema Discovery

After creating a table, its schema can be discovered with the `describe` statement, which will return, for each column, the following items:

* *Column*, field name;
* *Type*, field type;
* *Is Null*, _true_ is the field is optional, _false_ otherwise;
* *Primary Key*, position of this field in the primary key, or blank if it does not appear in the key;
* *Local Key*, position of this field in the local key, or blank if it does not appear in the key.

```sql
describe GeoCheckin
```


## More information

Still unsure how best to structure your Riak TS table? Check out our [best practice recommendations][bestpractices].

Confused about columns, primary key, etc? Check out [Table Architecture][table arch] for full definitions.


## Next Steps

Now that you know how your Riak TS table will be structured, you can move on to [creating and activating your table][activating].