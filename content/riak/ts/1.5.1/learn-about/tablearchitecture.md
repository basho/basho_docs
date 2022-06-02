---
title_supertext: "Learn About"
title: "Architecture of Riak TS Tables"
description: "TS Tables Architecture"
menu:
  riak_ts-1.5.1:
    name: "Table Architecture "
    identifier: "advanced_planning_riakts_tables"
    weight: 503
    parent: "about"
project: "riak_ts"
project_version: "1.5.1"
toc: true
aliases:
    - /riakts/1.5.1/learn-about/advancedplanning/
---


[activating verify]: ../../using/creating-activating/#verify-creation-and-activation
[configuring]: ../../configuring/
[planning]: ../../using/planning
[sql]: ../sqlriakts/
[bestpractices]: ../bestpractices/


This page provides more in-depth information about how Riak TS tables are structured.

If you just want to get started creating a table in Riak TS, check out our quick guide to [planning your Riak TS table][planning]. 

You may also be interested in [more information about SQL in Riak TS][sql].


## Riak TS Tables

With Riak TS, you no longer have to build your own time series database on Riak KV. Instead, Riak TS integrates SQL structure and functionality with Riak KV key/value storage. It does this through Riak TS tables that you customize to fit your time series data and the needs of your workflow.


## Basic Structure of a Riak TS Table

In order to query TS data, data is structured using a specific schema. In particular, each row of data in a TS table consists of a set of columns. The definition of those columns happens when a table is created, and determines what data can be stored in the table. Queries can then be written against that schema and the TS query system can validate and execute them.

The schema of a TS table is comprised of column definitions and a primary key. The column definitions define the structure and type of the data in your table. The primary key contains two keys which determine how your data is stored and, therefore, how it is queried.

Riak TS enables querying large amounts of related data, so keys behave differently than in Riak KV.

Riak TS has two types of keys:

* *partition keys*, which determine where the data is placed on the cluster, and
* *local keys*, which determine where the data is written in the partition.

Partition keys can use *time quantization* to group data that will be queried together in the same physical part of the cluster. Time quantization says "group data by 15 minute clumps, or 10 second clumps, or 60 day clumps" depending on how quickly your time series data come in and how you need to analyze them. The quantization is configurable on a table level.

Local keys group similar kinds of data together in the partition, impacting your query performance.

We have combined the definition of the various keys and the data schema into a single SQL-like statement. The query language is a subset of SQL, so you will use the column names and the table name in those queries; SQL conventions such as case sensitivity hold.

Riak TS tables have a one-to-one mapping with Riak KV bucket types.


### Example

```sql
CREATE TABLE GeoCheckin
(
   id          SINT64    NOT NULL,                   -
   region      VARCHAR   NOT NULL,                    |
   state       VARCHAR   NOT NULL,                    |
   time        TIMESTAMP NOT NULL,                    |  --> Column Definitions
   weather     VARCHAR   NOT NULL,                    |
   temperature DOUBLE,                               _
   PRIMARY KEY (
     (id, QUANTUM(time, 15, 'm')),        <-- Partition Key
     id, time                             <-- Local Key
   )
)
```


## Riak TS Tables in Command Line

When you [verify that your table was properly created][activating verify], you'll see a response that shows your table's schema on the command line. It will look something like this:

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

The two `key_v1` entries correspond to the partition key and the local key. The first key contains the column names used for the partition key, which defines how the data set is chunked and where the chunk data is co-located, and the second key contains the local key which uniquely identifies the data within a chunk. These two sets of column names will mostly be the same, but the partition key will have an additional quantum definition for the timestamp and the local key may have additional column names:

```sh
{key_v1,[
   {hash_fn_v1,riak_ql_quanta,quantum,
               {param_v1,[<<"id">>]},                     <- Partition Key Part 1
               [{param_v1,[<<"time">>]},15,m],timestamp}  <- Partition Key Part 2

]},
{key_v1,[
   {param_v1,[<<"id">>]},        <- Local Key part 1
   {param_v1,[<<"time">>]},      <- Local Key part 2
   {param_v1,[<<"state">>]}      <- Local Key part 3
   {param_v1,[<<"weather">>]}    <- Local Key part 4
]}
```


## Data Modeling

```sql
CREATE TABLE GeoCheckin
(
   id           SINT64    NOT NULL,               -
   region      VARCHAR   NOT NULL,                 |
   state       VARCHAR   NOT NULL,                 |
   time        TIMESTAMP NOT NULL,                 |--> Column Definitions
   weather     VARCHAR   NOT NULL,                 |
   temperature DOUBLE,                            _
   PRIMARY KEY (
     (id, QUANTUM(time, 15, 'm')),     <-- Partition key
     id, time                          <-- Local key
   )
)
```

The column names in the partition key are used to determine which vnodes handle the writes and queries, which is why we suggest using a column definition that captures a class or type of data and another that captures an instance of that class or type. For instance, `region` represents a class of data, whereas `state` is an instance of that class.

If both column names represent the same class or type of data for large numbers of writes, and the quanta that is specified has a large duration, only n_val vnodes will process the incoming writes. In these instances, Riak will not be able to parallelize the workload across CPUs.

All keys and values are written contiguously on disk. For each co-located block of data a sub-query is created. Currently there is a default maximum of 20 sub-queries per single query to prevent overload; see [Configure Riak TS][configuring] for details on changing that value.

Choosing a quanta size involves tradeoffs. Small quanta are best for writes while large quanta are best for queries. You should choose your quanta according to your data needs.

It is difficult to make any recommendations on quanta size, because the size of your quanta is heavily dependent on both the kind of data you are gathering and what you need to do with it.

Also see [Riak TS Best Practices][bestpractices] for more guidance on choosing an appropriate quantum size.
