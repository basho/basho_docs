---
title: Planning Your Riak TS Table
project: riakts
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner
---

[activating]: https://www.docs.basho.com/riakts/1.0.0/using/activating
[advancedplanning]: https://www.docs.basho.com/riakts/1.0.0/advancedplanning
[installing]: https://www.docs.basho.com/riakts/1.0.0/installing/
[sql]: https://www.docs.basho.com/riakts/1.0.0/learn-about/sql

Now that you've [installed][installing] Riak TS, you're almost ready to create TS table. Before you can create your table, you'll need to plan it out. 

This page provides a basic overview of what you'll need and some guidelines/limitations. For a deeper dive into planning and designing Riak TS tables, check out [Advanced Planning][advancedplanning].

Riak TS tables are closely tied to SQL tables. If you are unfamiliar with SQL or would like to know more about how Riak TS integrates SQL, check out [SQL for Riak TS][sql].

###Anatomy of a Schema

In order to create a working Riak TS table, you'll need plan your table out. Here is an example Riak TS `CREATE TABLE` statement (broken across many lines for clarity):

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


####Field Names

Field names (`myfamily`, `myseries`, etc) must be ASCII strings, in addition to having the correct case. If field names need to contain special cases (e.g. spaces or punctuation) they can be single quoted.

Valid types are:

* `varchar`
* `sint64`
* `boolean`
* `timestamp`
  * Note: 0 (zero) is not a valid timestamp
* `double`
  * If you are using an IEEE specification, 'NaN' (not a number) and 'INF' (infinity) cannot be used.


Additionally, the fields declared in the keys must have the flag `not null`.

####Primary Key

You MUST have exactly three fields in the following order (ASCII only): 

1. The first field (family) is a class or type of data. 
2. The second field (series) identifies the specific instances of the class/type, such as username or device ID. 
3. The third field (quantum) sets the time intervals to group data by.

The quantum function takes 3 parameters:

* the name of a field in the table definition of type `timestamp`
* a quantity
* a unit of time:
  * 'd'  - days  
  * 'h' - hours
  * 'm' - minutes
  * 's' - seconds

>**Note:** Your quanta must be in UTC and UNIX epoch seconds in order for it to be queried.

>**Another note:** While you can create a table with duplicated elements, we strongly recommend not doing so.

#####Local Key

The second key (local key) MUST contain the same 3 fields in the same order as the partition key. This ensures that the same fields determining your data's partition also dictate the sorting of the data within that partition.

##Next Steps

Now that you know how your Riak TS table will be structured, you can move on to [creating and activating your table][activating].