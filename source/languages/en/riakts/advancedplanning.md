## Basic Structure of a Riak TS Table
Riak TS enables querying large amounts of related data, so keys behave differently than in Riak KV.

Riak TS has two types of keys:

* *partition keys*, which determine where the data is placed on the cluster, and
* *local keys*, which determine where the data is written in the partition.

Partition keys use *time quantization* to group data that will be queried together in the same physical part of the cluster. Time quantization says “group data by 15 minute clumps, or 10 second clumps, or 60 day clumps” depending on how quickly your time series data come in and how you need to analyze them. The quantization is configurable on a table level.

In order to query TS data, data is structured using a specific schema. The schema defines what data can be stored in a TS table and what type it has. Queries can then be written against that schema and the TS query system can validate and execute them.

To make it easy we have combined the definition of the various keys and the data schema into a single SQL-like statement. The query language is a full sub-set of SQL, so you will use the field names and the table name in those queries, and SQL conventions, such as case sensitivity, hold.

Riak TS tables have a one-to-one mapping with Riak KV buckets.

Field names define the structure of the data, taking the format:

```sql
name type [not null],
```

####Primary Key
The `PRIMARY KEY` describes the partition and local keys. The partition key and the local key are nearly identical, differing only by the definition of the `quantum` used to colocate data.

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
