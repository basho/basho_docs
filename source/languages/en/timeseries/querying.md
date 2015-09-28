---
title: Querying Data in Riak TS
project: timeseries
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner
---

Now that you have [written][writing] data to your Riak TS bucket, you can query it.

## Basic Querying

For ease and consistency, let's use the same example table:

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
    time, myfamily, myseries

           )
)
```

Basic queries return the full range of values between two given times for a series in a family.To query a bucket, issue a SQL statement against the Riak TS bucket:

````erlang
{ok, Pid} = riakc_pb_socket:start_link("myriakdb.host", 10017).
riakc_ts:query(Pid, "select * from GeoCheckin where time > 123456 and time < 987654 and myfamily = ‘family1’ and myseries = ‘series1’").
```

The SQL query must cover the entire time series key (`time`,  `myfamily`, `myseries`). If any part of the time series key is missing, you will get an error.

## Specific Querying

You can also select particular fields from the data. In the below example, **??** what specifically is happening here?:

```erlang
riakc_ts:query(Pid, "select weather, temperature from GeoCheckin where time > 123456 and time < 987654 and myfamily = ‘family1’ and myseries = ‘series1’").
```

Additionally, you can extend the query beyond the key. For example, **??** what's happening in this example?:

```erlang
riakc_ts:query(Pid, "select weather, temperature from GeoCheckin where time > 123456 and time < 987654 and myfamily = ‘family1’ and myseries = ‘series1’ and temperature > 27.0").
```

A small subset of SQL is supported. All comparisons are of the format: `Field Operator Constant`

The following operators are supported for each data type

|           | = | != | > | < | =< | >= |
| --------- | - | -- | - | - | -- | -- |
| binary    | X | X  |   |   |    |    |
| boolean   | X | X  |   |   |    |    |
| integer   | X | X  | X | X | X  | X  |
| float     | X | X  | X | X | X  | X  |
| timestamp | X | X  | X | X | X  | X  |
| any       |   |    |   |   |    |    |


## Limitations

The queueing system currently allows you to run 3 simultaneous queries only.

**??**There are buffer limitations, but I don't know what they are or how to guide users.
