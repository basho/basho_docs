---
title: Data Modeling in Riak TS
project: riakts
version: 1.0.0+
document: guide
toc: true
index: true
audience: intermediate
---



Riak TS


##

```sql
CREATE TABLE GeoCheckin
(
   myfamily    varchar   not null,                     -
   myseries    varchar   not null,                      |
   time        timestamp not null,                       --> Columns/fields
   weather     varchar   not null,                      |
   temperature double,                                 _
   PRIMARY KEY (
     (myfamily, myseries, quantum(time, 15, 'm')),     <-- Partition key
     myfamily, myseries, time                          <-- Local key
   )
)
```

The values in the partition key determine which vnodes handle its writes and queries. If the `family` and `series` fields are the same for large numbers of writes, and the quanta that is specified has a large duration, only n_val vnodes will process the incoming writes. In these instances, Riak will not be able to parallelize the workload across CPUs.

Riak TS queries work best when the data being queried is in the same family, series, and quanta because all keys and values are written contiguously on disk. For each family, series, and quanta, a sub-query is created and run against the vnodes that data is hashed to. Currently there is a limit of 4 sub-queries per single query to prevent overload.

Due to this limitation, there is a trade off to be made when deciding on the quanta size. Small quantas are best for writes and large quantas are best for queries. You should choose your quanta according to your data needs.

It is difficult to make any recommendations on quanta size, because the size of your quanta is heavily dependent on both the kind of data you are gathering and what you need to do with it.