---
title: Querying Data in Riak TS
project: riakts
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
   temperature double,
   PRIMARY KEY (
     (myfamily, myseries, quantum(time, 15, 'm')),
     myfamily, myseries, time
   )
)
```

Basic queries return the full range of values between two given times for a series in a family.To query a bucket, issue a SQL statement against the Riak TS bucket:

````erlang
{ok, Pid} = riakc_pb_socket:start_link("myriakdb.host", 10017).
riakc_ts:query(Pid, "select * from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1'").
```

```java
RiakClient client = RiakClient.newClient(10017, "myriakdb.host");
String queryText = "select * from GeoCheckin " +
                   "where time > 1234560 and time < 1234569 and " +
                   "myfamily = 'family1' and myseries = 'series1'";

Query query = new Query.Builder(queryText).build();
QueryResult queryResult = client.execute(query);
```

```ruby
client = Riak::Client.new 'myriakdb.host', pb_port: 10017
query = Riak::Timeseries::Query.new client, "select * from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1'"
results = query.issue!
>>>>>>> a0580a2273f2b212e24c716e5afb823ed7527f3b
```

The SQL query must cover the entire time series key (`myfamily`, `myseries`, and `time`). If any part of the time series key is missing, you will get an error.

## Specific Querying

You can also select particular fields from the data. In the below example, **??** what specifically is happening here?:

```erlang
riakc_ts:query(Pid, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1'").
```
```ruby
Riak::Timeseries::Query.new(client, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1'").issue!
```

```java
String queryText = "select weather, temperature from GeoCheckin " +
                   "where time > 1234560 and time < 1234569 and " +
                   "myfamily = 'family1' and myseries = 'series1'";

Query query = new Query.Builder(queryText).build();
QueryResult queryResult = client.execute(query);
```

Additionally, you can extend the query beyond the key. For example, **??** what's happening in this example?:

```erlang
riakc_ts:query(Pid, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1' and temperature > 27.0").
```
```ruby
Riak::Timeseries::Query.new(client, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1' and temperature > 27.0").issue!
```

```java
String queryText = "select weather, temperature from GeoCheckin " +
                   "where time > 1234560 and time < 1234569 and " +
                   "myfamily = 'family1' and myseries = 'series1' " +
                   "temperature > 27.0";

Query query = new Query.Builder(queryText).build();
QueryResult queryResult = client.execute(query);
```

When querying with user-supplied data, it is *essential* that you protect
against SQL injection. Time Series clients provide bound parameters to
eliminate the need to escape data on the client:

```erlang
riakc_ts:query(Pid,
  "select weather, temperature from GeoCheckin where time > :start and time < :end and myfamily = :family and myseries = :series and temperature > :temperature",
  [
    {"start", 1234560},
    {"end", 1234569},
    {"family", "myfamily"},
    {"series", "myseries"}
  ]).
```
```ruby
query = Riak::Timeseries::Query.new(client, "select weather, temperature from GeoCheckin where time > :start and time < :end and myfamily = :family and myseries = :series and temperature > :temperature")
query.interpolations = {
  'start' => 1234560,
  'end' => 1234569,
  'family' => 'myfamily',
  'series' => 'myseries'
}
query.issue!
```

A small subset of SQL is supported. All comparisons are of the format: `Field Operator Constant`

The following operators are supported for each data type

|           | = | != | > | < | =< | >= |
| --------- | - | -- | - | - | -- | -- |
| binary    | X | X  |   |   |    |    |
| boolean   | X | X  |   |   |    |    |
| sint64    | X | X  | X | X | X  | X  |
| double    | X | X  | X | X | X  | X  |
| timestamp | X | X  | X | X | X  | X  |
| any       |   |    |   |   |    |    |


## Limitations

In this early version queries can only range over 1 to 4 quanta. If you write a range that is too large, your query will generate too many subqueries and the query system will refuse to run.

**??** will the "1 o’clock" thing actually work or am i being trolled

For instance, if you have the default system with a `15m quanta`, then a query such as:

`time > 1 o’clock and time < 2 o’clock`

will be fine because it covers 4 quanta. While the query:

`time > 1 o’clock and time < 3 o’clock`

will fail.

**??**There are buffer limitations, but I don't know what they are or how to guide users.
