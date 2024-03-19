---
title: "Planning Your Riak TS Table"
description: "Planning Your Riak TS Table"
menu:
  riak_ts-1.0.0:
    name: "Plan Your Table"
    identifier: "planning_riakts_table"
    weight: 301
    parent: "using"
project: "riak_ts"
project_version: "1.0.0"
lastmod: 2015-12-15T00:00:00-00:00
sitemap:
  priority: 0.1
toc: true
aliases:
    - /riakts/1.0.0/using/planning/
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

### Anatomy of a Schema

In order to create a working Riak TS table, you'll need plan your table out. Once created, your table cannot be changed. Here is an example Riak TS `CREATE TABLE` statement (broken across many lines for clarity):

```sql
CREATE TABLE GeoCheckin
(
   myfamily    varchar   not null,
   myseries    varchar   not null,
   time        timestamp not null,
   weather     varchar   not null,
   temperature double,
   PRIMARY KEY (
     (myfamily, myseries, quantum(time, 15, 'm')),
     myfamily, myseries, time
   )
)
```

#### Fields

Fields, also called columns, refer to the items before the `PRIMARY KEY`. Field names (`myfamily`, `myseries`, etc) must be ASCII strings, in addition to having the correct case. If field names need to contain spaces or punctuation they can be double quoted.

Field names define the structure of the data, taking the format:

```sql
name type [not null],
```

Fields specified as part of the primary key must be defined as `not null`.

The types associated with fields are limited. Valid types are:

* `varchar`
  * Any string content is valid, including Unicode. Can only be compared using strict equality, and will not be typecast (e.g., to an integer) for comparison purposes. Use single quotes to delimit varchar strings.
* `boolean`
  * `true` or `false` (any case)
* `timestamp`
  * Timestamps are integer values expressing [UNIX epoch time in UTC][epoch] in **milliseconds**. Zero is not a valid timestamp.
* `sint64`
  * Signed 64-bit integer
* `double`
  * This type does not comply with its IEEE specification: `NaN` (not a number) and `INF` (infinity) cannot be used.

### Primary Key

The `PRIMARY KEY` describes both the partition and local keys. The partition key and the local key are nearly identical, differing only by the definition of the `quantum` used to colocate data.

#### Partition Key

The partition key is defined as the three named fields in parentheses:

```sql
(myfamily, myseries, (quantum(time, 15, 'm')),
```

The partition key MUST have exactly three fields in the following order:

1. The first field (*family*) is a class or type of data.
2. The second field (*series*) identifies the specific instances of the class/type, such as username or device ID.
3. The third field (*quantum*) sets the time intervals to group data by.

The quantum function takes 3 parameters:

* the name of a field in the table definition of type `timestamp`
* a quantity
* a unit of time:
  * 'd'  - days
  * 'h' - hours
  * 'm' - minutes
  * 's' - seconds

#### Local Key

The second key (local key) MUST contain the same 3 fields in the same order as the partition key. This ensures that the same fields determining your data's partition also dictate the sorting of the data within that partition.

## More information

Still unsure how best to structure your Riak TS table? Check out our [best practice recommendations][bestpractices].

Confused about columns, primary key, etc? Check out [Table Architecture][table arch] for full definitions.

## Next Steps

Now that you know how your Riak TS table will be structured, you can move on to [creating and activating your table][activating].
