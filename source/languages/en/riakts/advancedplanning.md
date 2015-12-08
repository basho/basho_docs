## Basic Structure of a Riak TS Table
Riak TS enables querying large amounts of related data, so keys behave differently than in Riak KV.

Riak TS has two types of keys:

* *partition keys*, which determine where the data is placed on the cluster, and
* *local keys*, which determine where the data is written in the partition.

Partition keys use *time quantization* to group data that will be queried together in the same physical part of the cluster. Time quantization says “group data by 15 minute clumps, or 10 second clumps, or 60 day clumps” depending on how quickly your time series data come in and how you need to analyze them. The quantization is configurable on a table level.

In order to query TS data, data is structured using a specific schema. The schema defines what data can be stored in a TS table and what type it has. Queries can then be written against that schema and the TS query system can validate and execute them.

To make it easy we have combined the definition of the various keys and the data schema into a single SQL-like statement. The query language is a full sub-set of SQL, so you will use the field names and the table name in those queries, and SQL conventions, such as case sensitivity, hold.

Riak TS tables have a one-to-one mapping with Riak KV buckets.
