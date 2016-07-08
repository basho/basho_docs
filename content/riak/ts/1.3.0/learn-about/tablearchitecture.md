---
title: "Architecture of Riak TS Tables"
description: "TS Tables Architecture"
menu:
  riak_ts-1.3.0:
    name: "Table Architecture "
    identifier: "advanced_planning_riakts_tables"
    weight: 503
    parent: "about"
project: "riak_ts"
project_version: "1.3.0"
toc: true
aliases:
    - /riakts/1.3.0/learn-about/advancedplanning/
canonical_link: "https://docs.basho.com/riak/ts/latest/learn-about/tablearchitecture"
---


[activating]: ../../using/creating-activating/
[configuring]: ../../using/configuring/
[planning]: ../../using/planning
[sql]: ../sqlriakts/
[bestpractices]: ../bestpractices/


This page provides more in-depth information about how Riak TS tables are structured.

If you just want to get started creating a table in Riak TS, check out our quick guide to [planning your Riak TS table][planning]. 

You may also be interested in [more information about SQL in Riak TS][sql].


## Riak TS Tables

With Riak TS, you no longer have to build your own time series database on Riak KV. Instead, Riak TS integrates SQL structure and functionality with Riak KV key/value storage. It does this through Riak TS tables that you customize to fit your time series data and the needs of your workflow.


## Basic Structure of a Riak TS Table

Riak TS enables querying large amounts of related data, so keys behave differently than in Riak KV.

Riak TS has two types of keys:

* *partition keys*, which determine where the data is placed on the cluster, and
* *local keys*, which determine where the data is written in the partition.

Partition keys can use *time quantization* to group data that will be queried together in the same physical part of the cluster. Time quantization says “group data by 15 minute clumps, or 10 second clumps, or 60 day clumps” depending on how quickly your time series data come in and how you need to analyze them. The quantization is configurable on a table level.

In order to query TS data, data is structured using a specific schema. The schema defines what data can be stored in a TS table and what type it has. Queries can then be written against that schema and the TS query system can validate and execute them.

We have combined the definition of the various keys and the data schema into a single SQL-like statement. The query language is a subset of SQL, so you will use the column names and the table name in those queries; SQL conventions such as case sensitivity hold.

Riak TS tables have a one-to-one mapping with Riak KV bucket types.


### Example

```sql
CREATE TABLE GeoCheckin
(
   region      VARCHAR   NOT NULL,                   -
   state       VARCHAR   NOT NULL,                    |
   time        TIMESTAMP NOT NULL,                    |  --> Column Definitions
   weather     VARCHAR   NOT NULL,                    |
   temperature DOUBLE,                               _
   PRIMARY KEY (
     (region, state, QUANTUM(time, 15, 'm')),        <-- Partition Key
     region, state, time                             <-- Local Key
   )
)
```


### Column Definitions

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
* `BOOLEAN` - `true` or `false` (any case)
* `TIMESTAMP` - Timestamps are integer values expressing [UNIX epoch time in UTC][epoch] in milliseconds. Zero is not a valid timestamp.
* `SINT64` - Signed 64-bit integer
* `DOUBLE` - This type does not comply with its IEEE specification: `NaN` (not a number) and `INF` (infinity) cannot be used.


### Primary Key

The `PRIMARY KEY` describes both the partition key and local key. The partition key is a prefix of the local key, consisting of one or more column names. The local key must begin with the same column names as the partition key, but may also contain additional column names.


#### Partition Key


The partition key is the first element of the primary key, and is defined as a list of  column names in parentheses. The partition key must have at least one column name.

If you choose to include a quantum, it will be used to colocate data on one of the partition key's timestamp fields:

```sql
PRIMARY KEY  ((region, state, QUANTUM(time, 1, 's')), ...)
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

The second key (local key) MUST have the same fields in the same order as the partition key. This ensures that the keys are unique for that partition.

It can also have additional fields AFTER the fields in the partition key.

```sql
((region, state, QUANTUM(time, 15, 'm')), region, state, time, weather)
```

Note that weather is in the local key but not in the partition key.  Fields in the partition key must be covered by a queries where clause, additional fields in the local key do **not** have to be covered.


## Riak TS Tables in Command Line

When you [verify that your table was properly created][activating], you'll see a response that shows your table's schema on the command line. It will look something like this:

```sh
$ riak-admin bucket-type status GeoCheckin
GeoCheckin is active
...
ddl: {ddl_v1,<<"GeoCheckin">>,
             [{riak_field_v1,<<"region">>,1,binary,false},
              {riak_field_v1,<<"state">>,2,binary,false},
              {riak_field_v1,<<"time">>,3,timestamp,false},
              {riak_field_v1,<<"weather">>,4,binary,false},
              {riak_field_v1,<<"temperature">>,5,double,true}],
             {key_v1,[{hash_fn_v1,riak_ql_quanta,quantum,
                                  {param_v1,[<<"region">>]},
                      {param_v1,[<<"state">>]}]},
                      [{param_v1,[<<"time">>]},15,m],
                                  timestamp},
             {key_v1,[{param_v1,[<<"time">>]},
                      {param_v1,[<<"region">>]},
                      {param_v1,[<<"state">>]}]}}
```

The format of the response is:

```sh
ddl:  { ddl_v1, TABLE_NAME,
[ ARRAY OF COLUMN DEFINITIONS],
[ KEY INFO ]}}
```

The columns definitions are each:

```sh
{riak_field_v1,<<"COLUMN_NAME">>,COLUMN_INDEX,COLUMN_TYPE,NULLABLE}
```

The two `key_v1` entries correspond to the partion key and the local key. The first key contains the column names used for the partition key, which defines how the data set is chunked and where the chunk data is co-located, and the second key contains the local key which uniquely identifies the data within a chunk. These two sets of column names will mostly be the same, but the partition key will have an additional quantum definition for the timestamp and the local key may have additional column names:

```sh
{key_v1,[
   {hash_fn_v1,riak_ql_quanta,quantum,
               {param_v1,[<<"region">>]},                 <- Partition Key Part 1
               {param_v1,[<<"state">>]},                  <- Partition Key Part 2
               [{param_v1,[<<"time">>]},15,m],timestamp}  <- Partition Key Part 3

]},
{key_v1,[
   {param_v1,[<<"region">>]},     <- Local Key part 1
   {param_v1,[<<"state">>]},      <- Local Key part 2
   {param_v1,[<<"time">>]}        <- Local Key part 3
   {param_v1,[<<"temperature">>]} <- Local Key part 4
]}
```


## Data Modeling

```sql
CREATE TABLE GeoCheckin
(
   region      VARCHAR   NOT NULL,                -
   state       VARCHAR   NOT NULL,                 |
   time        TIMESTAMP NOT NULL,                 |--> Column Definitions
   weather     VARCHAR   NOT NULL,                 |
   temperature DOUBLE,                            _
   PRIMARY KEY (
     (region, state, QUANTUM(time, 15, 'm')),     <-- Partition key
     region, state, time                          <-- Local key
   )
)
```

The column names in the partition key are used to determine which vnodes handle the writes and queries, which is why we suggest using a column definition that captures a class or type of data and another that captures an instance of that class or type. For instance, `region` represents a class of data, whereas `state` is an instance of that class.

If both column names represent the same class or type of data for large numbers of writes, and the quanta that is specified has a large duration, only n_val vnodes will process the incoming writes. In these instances, Riak will not be able to parallelize the workload across CPUs.

All keys and values are written contiguously on disk. For each co-located block of data a sub-query is created. Currently there is a default maximum of 5 sub-queries per single query to prevent overload; see [Configure Riak TS][configuring] for details on changing that value.

Choosing a quanta size involves tradeoffs. Small quanta are best for writes while large quanta are best for queries. You should choose your quanta according to your data needs.

It is difficult to make any recommendations on quanta size, because the size of your quanta is heavily dependent on both the kind of data you are gathering and what you need to do with it.

Also see [Riak TS Best Practices][bestpractices] for more guidance on choosing an appropriate quantum size.


## Editing Your Table

Once created, you cannot edit your Riak TS table. If you discover something wrong with the setup of your Riak TS table, you will need to create it again. You will also need to decide whether to scrap the data in the existing table or move it from the old table to the new one.