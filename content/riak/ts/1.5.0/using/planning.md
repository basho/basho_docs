---
title: "Planning Your Riak TS Table"
description: "Planning Your Riak TS Table"
menu:
  riak_ts-1.5.0:
    name: "Plan Your Table"
    identifier: "planning_riakts_table"
    weight: 300
    parent: "using"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/using/planning/
canonical_link: "https://docs.basho.com/riak/ts/latest/using/planning"
---


[activating]: ../creating-activating/
[table arch]: ../../learn-about/tablearchitecture/
[bestpractices]: ../../learn-about/bestpractices/
[describe]: ../querying/describe/
[epoch]: https://en.wikipedia.org/wiki/Unix_time
[installing]: ../../setup/installing/
[sql]: ../../learn-about/sqlriakts/


You've [installed][installing] Riak TS, and you're ready to create a table. 

* If you're just looking to test out Riak TS, you can jump ahead to [Create a Table][activating], and test out our sample table. 
* If you're looking to set up your production environment, keep reading for guidelines on how best to structure your table.
* If you're looking for more information about what the components of a Riak TS table do, check out [Table Architecture][table arch].
* Riak TS tables are closely tied to SQL tables. If would like to know more about how Riak TS integrates SQL, check out [SQL for Riak TS][sql].

{{% note title="Important" %}}
The structure of your TS table will have a major impact on your Riak TS performance. Once created, your table cannot be changed. Please read through these sections carefully to determine the best options for your use-case.
{{% /note %}}


## TS Table Schema

Here is an example Riak TS CREATE TABLE statement, broken across many lines for clarity. (Remember, once created, your table cannot be changed):

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


## Column Definitions

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

* `VARCHAR` - Any [Latin 1](https://en.wikipedia.org/wiki/ISO/IEC_8859-1) string content is valid. Can only be compared using strict equality, and will not be typecast (e.g., to an integer) for comparison purposes. Use single quotes to delimit varchar strings.
* `BOOLEAN` - `true` or `false` (any case).
* `TIMESTAMP` - Integer values expressing [UNIX epoch time in UTC][epoch] in milliseconds. Zero is not a valid timestamp.
* `SINT64` - Signed 64-bit integer.
* `DOUBLE` - This type does not comply with its IEEE specification: `NaN` (not a number) and `INF` (infinity) cannot be used.
* `BLOB` - A new type as of TS 1.5.0 for binary objects. Behaves like a `VARCHAR` but is displayed as a hex value (and can be input as hex) via `riak-shell`.

## Primary Key

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
      region, state, time                      <-- LOCAL KEY
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
     (region, state, time,
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


### Partition Key

The partition key is the first element of the primary key, and must have at least one column name.

A general guideline is to first choose a column name that represents a class or type of data, and then choose a second column name that is a more specific instance(s) of the class/type.

You may specify a [quantum](#quantum), which is used to colocate data on one of the partition key's timestamp fields:

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

If you choose to specify a quantum, the quantum you pick will greatly impact your query performance. Read more about [quanta](#quantum) below.


### Local Key

The local key comes after the partition key. It must first contain the same column names in the same order as the partition key. This ensures that the same column names determining your data's partition also dictate the sorting of the data within that partition.

The local key may also contain additional column names so long as they come after the column names present in the partition key. For example:

```sql
   PRIMARY KEY (
     (region, QUANTUM(time, 15, 'm')),
      region, time, weather, temperature
   )
```

{{% note title="On Key Uniqueness" %}}

The local key in a TS row is what makes that row's key/address unique from other rows.
In the examples on this page and others we've used a composite key of
`region, state, time` because it can model different devices and groupings.  
If you have another integer identifier, such as a device ID, you can guarantee to be unique when combined with a timestamp, then you can have a shorter key definition.

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


## Quantum

The quantum is part of the [partition key](#partition-key) which is in the [primary key](#primary-key), as discussed above.

```sql
   PRIMARY KEY (
     (region, QUANTUM(time, 15, 'm')),
      region, time, weather, temperature
   )
```

Quanta give you the ability to control how your data are stored around your cluster, which impacts query performance. Iterating over a large number of keys localized on a small number of nodes can be significantly faster than iterating over a small number of keys on a large number of nodes. The option that is best for you will depend on how you expect to be querying your data, and whether you care more about [throughput](#throughput) or [latency](#latency).

If you have a cluster that isn't busy, a large query run on a single node will take longer to complete than the same query split into subqueries across multiple nodes. But if you were to add up the iteration times of those subqueries, you would find that the total is larger than your single-node iteration time.  

Multi-node queries take more added-up time overall because there’s extra overhead in setting up the sub-query iterations. When you run a whole query on a single node, you incur that overhead only once.  

This means that if you have a busy cluster, 5 large queries run against one cluster each (spanning 5 quanta) will complete quicker than 5 large queries broken into subqueries (spanning a quanta each) and run across all the nodes in your cluster.

1) if you know you care most about query throughput, then do as recommended above (ie, localize your data as much as possible), 2) if you know you care most about individual query latency, then do the opposite, and 3) if you don’t really know, then do as in 2), because the fractional latency gains of less data localization are much higher than the throughput losses.

### Throughput

If you need to optimize your Riak TS performance to handle many queries (throughput) more than you need those queries to be run quickly (latency), then you will want your data to be more localized.

#### How small should my quantum be?  

To optimize for throughput, we suggest that your minimum quantum size be set by the typical individual query length. For the best performance, the typical query should span as few quanta as possible.  

For example, if your typical query is an hour’s worth of data, then your quantum should be large enough that most of your queries will only hit a single node, meaning a quantum between 1 and 2 hours. 


#### How large should my quantum be? 

For the best throughput, we suggest that your maximum quantum size be chosen to minimize the number of concurrent queries hitting the same node. For the best performance, your quantum should be bounded by the total time spanned by your instantaneous volume of concurrent queries.  

For example, if you are typically issuing thousands of concurrent queries for different hours of the same week's worth of data, then you want your quantum to be significantly less than a week, or all your queries will end up hitting the same node and will execute serially, rather than benefit from parallelization in a distributed ring.


### Latency

If you need to optimize your Riak TS performance to ensure that individual queries are run quickly (latency), rather than for many queries to be handled at once (throughput), then you will need to take advantage of the parallel resources in your cluster by splitting queries across multiple nodes.


#### The best quantum for latency

To optimize query latency, your quantum should be small enough that your normal-sized query is spread across all nodes. If your quantum is too small it can result in so many subqueries being generated per-node that the parallel-processing capacity of your nodes is overwhelmed.

If you're up for a little math, you can use the following formula to identify the optimal quantum:

```
Q = t/(N*n)
```

`t` is the time spanned by your typical query, `N` is the number of nodes in your cluster, and `n` is the number of subqueries each node can execute simultaneously.

For example, if you have a 5-node cluster and your typical query is for 40 hours of data, your quantum should be no larger than 8 hours: 

```
8 = 40/(5*n)
```

If your quantum is larger than 8 hours, subqueries will execute on fewer than 5 nodes.  

As another example, if you have a 5-node cluster, your typical query is for 40 hours of data, and  you know that each of your nodes can process 4 subqueries simultaneously, then a quantum of 2 hours would further minimize your query latency. 

```
2 = 40/(5*4)
```

A 1-hour quantum would be too small, because it would cause you to have 8 subqueries per node. These 8 subqueries would not execute in parallel; instead, they would effectively execute as two sets of 4 subqueries, one after the other, which is less efficient than a single set of 4 subqueries that are twice as long.


## More information

After [creating a table][activating], its schema can be discovered with the [DESCRIBE statement][describe].

Still unsure how best to structure your Riak TS table? Check out our [best practice recommendations][bestpractices].

Confused about what column definitions, primary key, etc. do? Check out [Table Architecture][table arch] for an in-depth explanation of TS tables.


## Next Steps

Now that you know how your Riak TS table will be structured, you can move on to [creating your table][activating].