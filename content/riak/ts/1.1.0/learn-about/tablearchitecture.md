---
title: "Architecture of Riak TS Tables"
description: "Advanced Planning Riak TS Tables"
menu:
  riak_ts-1.1.0:
    name: "Table Architecture "
    identifier: "advanced_planning_riakts_tables"
    weight: 503
    parent: "about"
project: "riak_ts"
project_version: "1.1.0"
lastmod: 2016-01-14T00:00:00-00:00
sitemap:
  priority: 0.1
toc: true
aliases:
    - /riakts/1.1.0/learn-about/advancedplanning/
---

[activating]: ../../using/creating-activating/
[configuring]: ../../using/configuring/
[planning]: ../../using/planning
[sql]: ../sqlriakts/
[bestpractices]: ../bestpractices/

This page provides more in-depth information about how Riak TS tables are structured.

If you just want to get started creating a table in Riak TS, check out our quick guide to [planning your Riak TS table][planning]. You may also be interested in [more information about SQL in Riak TS][sql].

## Riak TS Tables

With Riak TS, you no longer have to build your own time series database on Riak KV. Instead, Riak TS integrates SQL structure and functionality with Riak KV key/value storage. It does this through Riak TS tables, that you customize to fit your time series data and the needs of your workflow.

## Basic Structure of a Riak TS Table

Riak TS enables querying large amounts of related data, so keys behave differently than in Riak KV.

Riak TS has two types of keys:

* *partition keys*, which determine where the data is placed on the cluster, and
* *local keys*, which determine where the data is written in the partition.

Partition keys use *time quantization* to group data that will be queried together in the same physical part of the cluster. Time quantization says “group data by 15 minute clumps, or 10 second clumps, or 60 day clumps” depending on how quickly your time series data come in and how you need to analyze them. The quantization is configurable on a table level.

In order to query TS data, data is structured using a specific schema. The schema defines what data can be stored in a TS table and what type it has. Queries can then be written against that schema and the TS query system can validate and execute them.

We have combined the definition of the various keys and the data schema into a single SQL-like statement. The query language is a subset of SQL, so you will use the field names and the table name in those queries; SQL conventions such as case sensitivity hold.

Riak TS tables have a one-to-one mapping with Riak KV buckets.

### Example

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

### Fields

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
  * Timestamps are integer values expressing [UNIX epoch time in UTC](https://en.wikipedia.org/wiki/Unix_time) in **milliseconds**. Zero is not a valid timestamp.
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

## Riak TS Tables in Command Line

When you [verify that your table was properly created][activating], you'll see a response that shows your table's schema on the command line. It will look something like this:

```sh
$ riak-admin bucket-type status GeoCheckin
GeoCheckin is active
...
ddl: {ddl_v1,<<"GeoCheckin">>,
             [{riak_field_v1,<<"myfamily">>,1,binary,false},
              {riak_field_v1,<<"myseries">>,2,binary,false},
              {riak_field_v1,<<"time">>,3,timestamp,false},
              {riak_field_v1,<<"weather">>,4,binary,false},
              {riak_field_v1,<<"temperature">>,5,double,true}],
             {key_v1,[{hash_fn_v1,riak_ql_quanta,quantum,
                                  {param_v1,[<<"myfamily">>]},
                      {param_v1,[<<"myseries">>]}]},
                      [{param_v1,[<<"time">>]},15,m],
                                  timestamp},
             {key_v1,[{param_v1,[<<"time">>]},
                      {param_v1,[<<"myfamily">>]},
                      {param_v1,[<<"myseries">>]}]}}
```

The format of the response is:

```sh
ddl:  { ddl_v1, TABLE_NAME,
[ ARRAY OF COLUMNS],
[ KEY INFO ]}}
```

The columns are each:

```sh
{riak_field_v1,<<"FIELD_NAME">>,COLUMN_INDEX,COLUMN_TYPE,NULLABLE}
```

The two `key_v1` entries correspond to the partion key and the local key. The first key contains the columns used for the partition key, which defines how the data set is chunked and where the chunk data is co-located, and the second key contains the local key which uniquely identifies the data within a chunk. These two sets of columns will mostly be the same, but the partition key will have an additional quantum definition for the timestamp column:

```sh
{key_v1,[
   {hash_fn_v1,riak_ql_quanta,quantum,
               {param_v1,[<<"myfamily">>]},               <- Partition Key Part 1
               {param_v1,[<<"myseries">>]},               <- Partition Key Part 2
               [{param_v1,[<<"time">>]},15,m],timestamp}  <- Partition Key Part 3

]},
{key_v1,[
   {param_v1,[<<"myfamily">>]},  <- Local Key part 1
   {param_v1,[<<"myseries">>]},  <- Local Key part 2
   {param_v1,[<<"time">>]}       <- Local Key part 3
]}
```

## Data Modeling

```sql
CREATE TABLE GeoCheckin
(
   myfamily    varchar   not null,                     -
   myseries    varchar   not null,                      |
   time        timestamp not null,                       --> Columns
   weather     varchar   not null,                      |
   temperature double,                                 _
   PRIMARY KEY (
     (myfamily, myseries, quantum(time, 15, 'm')),     <-- Partition key
     myfamily, myseries, time                          <-- Local key
   )
)
```

The values in the partition key determine which vnodes handle its writes and queries. If the `family` and `series` fields are the same for large numbers of writes, and the quanta that is specified has a large duration, only n_val vnodes will process the incoming writes. In these instances, Riak will not be able to parallelize the workload across CPUs.

Riak TS queries work best when the data being queried is in the same family, series, and quanta because all keys and values are written contiguously on disk. For each co-located block of data a sub-query is created. Currently there is a default maximum of 5 sub-queries per single query to prevent overload; see [Configure Riak TS][configuring] for details on changing that value.

Choosing a quanta size involves tradeoffs. Small quanta are best for writes while large quanta are best for queries. You should choose your quanta according to your data needs.

It is difficult to make any recommendations on quanta size, because the size of your quanta is heavily dependent on both the kind of data you are gathering and what you need to do with it.

See also [Riak TS Best Practices][bestpractices].

## Editing Your Table

Once created, you cannot edit your Riak TS table. If you discover something wrong with the setup of your Riak TS table, you will need to create it again. You will also need to decide whether to scrap the data in the existing table or move it from the old table to the new one.
