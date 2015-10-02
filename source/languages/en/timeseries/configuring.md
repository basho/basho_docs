---
title: Configuring a Riak TS Bucket
project: timeseries
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner
---

Now that you've [installed][installing] Riak TS, you can configure a bucket.

## Basic Structure of a Riak TS Bucket

Riak TS enables querying large amounts of related data, so keys behave differently than in Riak KV.

Riak TS has two types of keys:

* *partition keys* determine where the data is placed on the cluster
* *local keys* determine how the data is actually stashed on the disk

*Time quantisation* is employed in partition keys to group data that will be queried together in the same physical part of the cluster. Time quantisation just says “group by data by 15 minute clumps, or 10 second clumps, or 2 month clumps” depending on the ingestion characteristics of your time series data. The quantisation is configurable on a bucket level.

In order to query TS data, data is stored in a schema. The schema defines what data can be stored in a time series bucket and what type it has. Queries can then be written against that schema and the TS query system can validate and execute them.

To make it easy we have combined the definition of the various keys and the data schema into a single SQL-like statement. The query language is a full sub-set of SQL, so you will use the field names and the table name (as buckets are called in SQL) in those queries, and SQL conventions hold (such as case sensitivity).

Riak TS buckets have a one-to-one mapping with Riak KV buckets.


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
     (quantum(time, 15, 'm'), myfamily, myseries),
     time,
     myfamily,
     myseries
  )
)
```

In addition to paying attention to case sensitivity, field names (`myfamily`, `myseries`, etc) must be strings. If they need to contain special cases (spaces, punctutation), they can be single quoted.

####Field Names
The field names section of the command defines the structure of the data, taking the format:

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
The `PRIMARY KEY` section of the command describes the partition and local keys. The partition key is defined as the three named fields in brackets. The first one must be the quantum function:

```sql
(quantum(time, 15, 'm'), myfamily, myseries),
```

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

The second (family) field is used for grouping types of data for deletion/expiry, and the third (series) field is for sets of time series data.

The second key (local key) MUST contain the same 3 fields in the same order.

Additionally, the fields declared in the keys must have the flag `not null`.

###Optimizing Bucket Configuration

Multiple steps are involved in storing data with Riak TS. First, what partition the data goes in must be determined. Then it must be determined where in that partition the data will go. Since the data is guaranteed to have a timestamp (or similar), it is tempting to just partition by timestamp (partition key) and sort by timestamp (local key). But, frequently, there are other significant fields, too. 

The partition and local keys are general composite keys. They do not cover this other significant information set. That must be included through the semantics of the other fields. 

It is important to keep this in mind when configuring your bucket. Take a heartbeat sensor, for instance. You would want to partition by {timestamp, whose heartbeat it is, sensor_type}, so when you looking for a particular person's heartrate on a particular day, you can search via a time range, a person, and monitor type. This is where you see a bigger payoff. It's cheaper to read 1000 points from each of three places than it is to read 3000 points from one place or one point from 3000 places.

