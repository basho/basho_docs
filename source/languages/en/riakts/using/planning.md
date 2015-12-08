---
title: Configuring a Riak TS Table
project: riakts
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner
---

[installing]: https://www.docs.basho.com/riakts/1.0.0/installing/

Now that you've [installed][installing] Riak TS, you can configure a table.




### Anatomy of a Schema
Here is an example Riak TS `CREATE TABLE` statement (broken across many lines for clarity):

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

Field names define the structure of the data, taking the format:

```sql
name type [not null],
```

Valid types are:

* `varchar`
* `sint64`
* `double`
  * If you are using an IEEE specification, 'NaN' (not a number) and 'INF' (infinity) cannot be used.
* `boolean`
* `timestamp`
  * Note: 0 (zero) is not a valid timestamp


Additionally, the fields declared in the keys must have the flag `not null`.

####Primary Key
The `PRIMARY KEY` describes the partition and local keys. The partition key and the local key are nearly identical, differing only by the definition of the `quantum` used to colocate data.

#####Partition Key 
The partition key is defined as the three named fields in brackets:

```sql
(myfamily, myseries, (quantum(time, 15, 'm')),
```

You MUST have exactly three fields in the following order: 

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

#####Local Key
The second key (local key) MUST contain the same 3 fields in the same order as the partition key. This ensures that the same fields determining your data's partition also dictate the sorting of the data within that partition.
