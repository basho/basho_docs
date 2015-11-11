---
title: Configuring a Riak TS Table
project: riak-ts
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner
---

[installing]: https://www.docs.basho.com/riakts/1.0.0/installing/

Now that you've [installed][installing] Riak TS, you can configure a table.

## Basic Structure of a Riak TS Table
Riak TS enables querying large amounts of related data, so keys behave differently than in Riak KV.

Riak TS has two types of keys:

* *partition keys*, which determine where the data is placed on the cluster, and
* *local keys*, which determine how the data is actually stashed on the disk.

Partition keys use *time quantization* to group data that will be queried together in the same physical part of the cluster. Time quantization says “group data by 15 minute clumps, or 10 second clumps, or 60 day clumps” depending on how quickly your time series data come in and how you need to analyze them. The quantization is configurable on a table level.

In order to query TS data, data is structured using a specific schema. The schema defines what data can be stored in a TS table and what type it has. Queries can then be written against that schema and the TS query system can validate and execute them.

To make it easy we have combined the definition of the various keys and the data schema into a single SQL-like statement. The query language is a full sub-set of SQL, so you will use the field names and the table name in those queries, and SQL conventions, such as case sensitivity, hold.

Riak TS tables have a one-to-one mapping with Riak KV buckets.


### Anatomy of a Schema
Here is an example Riak TS `CREATE TABLE` statement (broken across many lines for clarity):

```sql
CREATE TABLE GeoCheckin
(
   myfamily    varchar   not null,
   myseries    varchar   not null,
   time        timestamp not null,
   weather     varchar   not null,
   temperature float,
   PRIMARY KEY (
     myfamily, myseries, (quantum(time, 15, 'm')),
     myfamily, myseries, time

           )
)
```


####Field Names
Field names (`myfamily`, `myseries`, etc) must be strings, in addition to having the correct case. If field names need to contain special cases (e.g. spaces or punctutation) they can be single quoted.

Field names define the structure of the data, taking the format:

```sql
name type [not null],
```

Valid types are:

* `varchar`
* `integer`
* `float`
* `boolean`
* `timestamp`
* `any`


####Primary Key
The `PRIMARY KEY` describes the partition and local keys. The partition key is defined as the three named fields in brackets:

```sql
(myfamily, myseries, (quantum(time, 15, 'm')),
```

You MUST have all three fields. The first (family) field is used for grouping types of data for deletion/expiry, and the second (series) field is for sets of time series data. The third (quantum) field sets the time intervals to group data by.

The second key (local key) MUST contain the same 3 fields in the same order.

The quantum function takes 3 parameters:

* the name of a field in the table definition of type `timestamp`
* a quantity
* a unit of time:
  * 'y' - years
  * 'mo' - months
  * 'd'  - days
  * 'h' - hours
  * 'm' - minutes
  * 's' - seconds


Additionally, the fields declared in the keys must have the flag `not null`.

###Optimizing Table Configuration

Multiple steps are involved in storing data with Riak TS. First, you must determin what partition the data will go in. Then you must determine where in that partition the data will go. Since the data is guaranteed to have a timestamp (or similar), it is tempting to just partition by timestamp (partition key) and sort by timestamp (local key). But, frequently, there are other significant fields, too. 

The partition and local keys are general composite keys. They do not cover this other significant information set. That must be included through the semantics of the other fields. 

It is important to keep this in mind when configuring your table. Take a heartbeat sensor, for instance. You would want to partition by {timestamp, whose heartbeat it is, sensor_type}, so when you looking for a particular person's heartrate on a particular day, you can search via a time range, a person, and monitor type. This is where you see a bigger payoff. It's cheaper to read 1000 points from each of three places than it is to read 3000 points from one place or one point from 3000 places.