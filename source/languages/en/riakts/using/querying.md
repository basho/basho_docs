---
title: Querying Data in Riak TS
project: riakts
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner
---

[activating]: https://docs.basho.com/riakts/1.0.0/using/activating
[writing]: https://docs.basho.com/riakts/1.0.0/using/writingdata


Now that you have [created][activating] a Riak TS table and [written][writing] data to it, you can query your data.


##Basic Querying

Before you begin querying, there are some guidelines to keep in mind:

* You can use Unicode as well as ASCII.
* You must query in UTC/UNIX epochs. 
  * The parser will treat '2015-12-08 14:00 EDT' as a character literal/string/varchar.
* 2i index will not work with Riak TS.
* `riak search` will not work with Riak TS.

Basic queries return the full range of values between two given times for a series in a family. To demonstrate, we'll use the same example table:

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

Query a table by issuing a SQL statement against the table:

```erlang
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

Your query must cover the entire time series key (`myfamily`, `myseries`, and `time`). If any part of the time series key is missing, you will get an error.

## Specific Querying

You can also select particular fields from the data.

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

Additionally, you can extend the query beyond the key.

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

| |=|!=|>|<|<=|>=|
|-----------|---|---|---|---|---|---|
| varchar   | X | X |   |   |   |   |
| boolean   | X | X |   |   |   |   |
| sint64    | X | X | X | X | X | X |
| double    | X | X | X | X | X | X |
| timestamp | X | X | X | X | X | X |


>**Note**
>
>Field-to-field comparisons are not currently supported.

## Limitations

In this early version queries can only range over 1 to 4 quanta. A query covering more than 4 quanta will generate too many sub-queries and the query system will refuse to run it.  
  * Example: Assume a default system with a 15min quanta.
  * A query of “time > 1 o’clock and time < 2 o’clock” will be fine because it covers 4 quanta.
  * A query of “time > 1 o’clock and time < 3 o’clock” will fail because it covers more than 4 quanta.
The important information to construct examples here: time values are now always strictly integers, and they are expressed in milliseconds.

So if I give you an example query like this: select weather from GeoCheckin where time >= 3000 and time < 30000 and user = 'user_1' and location = ‘Scotland'

I’m saying that the time is >= 3 seconds and < 30 seconds.

The question of whether that spans multiple quanta, or indeed too many quanta, depends on the SQL(ish) code used to define the bucket type.

If, for example, this is the create table command issued:

CREATE TABLE GeoCheckin
 (geohash varchar not null,
  location varchar not null,
  user varchar not null,
  time timestamp not null,
  weather varchar not null,
  temperature varchar,
    PRIMARY KEY((location, user, quantum(time, 15, 's')),
                location, user, time))

that data is stored in chunks of 15 seconds, so the query I showed you would have to talk to 2 vnodes (2 quanta): 0-14.999 seconds, and 15.0-29.999 seconds.

Now that the quanta/query parameter is configurable, it’s harder to come up with a “definitive” example of a query that covers too much ground, but given the same table definition and 6 quanta as the limit, this query would not work because 6 quanta of 15 seconds each would be a total search space of 90 seconds:

select weather from GeoCheckin where time >= 3000 and time < 100000 and user = 'user_1' and location = ‘Scotland'

**??**There are buffer limitations, but I don't know what they are or how to guide users.
