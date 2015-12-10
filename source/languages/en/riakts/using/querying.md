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


##Querying

Before you begin querying, there are some guidelines to keep in mind:

* All elements of the compound primary key must be present
* Data may queried as Unicode or ASCII.
* You must query in UTC/UNIX epoch milliseconds. 
  * The parser will treat '2015-12-08 14:00 EDT' as a character literal/string/varchar, not a timestamp.
* All clauses must be in either 'ColumnName Comparison Literal' or 'Comparison BooleanOperator Comparison' order.
* The `or` operator will work only for columns that are NOT
  in the primary key. Multiple queries are required to select multiple values for primary key fields.
* When using `or`, you must surround the expression with parentheses or your query will return an error.

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
Your query must include all components of the primary key (`myfamily`, `myseries`, and `time`). If any part of the primary key is missing, you will get an error.


###Wildcard Example

Query a table by issuing a SQL statement against the table. Your query MUST include a 'where' clause with all components. 

In the following client-specific examples we'll select all fields from the GeoCheckin table where `time`, `myfamily`, and `myseries` match our supplied parameters:

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

```python
import datetime
from riak.client import RiakClient

epoch = datetime.datetime.utcfromtimestamp(0)

def unix_time_millis(dt):
    td = dt - epoch
    return int(td.total_seconds() * 1000.0)

tenMins = datetime.timedelta(0, 600)

now = datetime.datetime(2015, 1, 1, 12, 0, 0)
nowMS = unix_time_millis(now);

tenMinsAgo = now - tenMins
tenMinsAgoMS = unix_time_millis(tenMinsAgo);

tenMinsFromNow = now + tenMins
tenMinsFromNowMS = unix_time_millis(tenMinsFromNow);

# NB: modify 'host' and 'pb_port' to match your installation
client = RiakClient(host='myriakdb.host', pb_port=8087)

fmt = """
select * from GeoCheckin where
    time > {t1} and time < {t2} and
    myfamily = 'family1' and myseries = 'series1'
"""
query = fmt.format(t1=tenMinsAgoMS, t2=tenMinsFromNowMS)

ts_obj = client.ts_query('GeoCheckin', query)
print "Query result rows:", ts_obj.rows
```

```ruby
client = Riak::Client.new 'myriakdb.host', pb_port: 10017
query = Riak::Timeseries::Query.new client, "select * from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1'"
results = query.issue!
```


###Select Query

You can also select particular fields from the data:

```
select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1'
```

Client-specific examples:

```erlang
riakc_ts:query(Pid, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1'").
```

```java
String queryText = "select weather, temperature from GeoCheckin " +
                   "where time > 1234560 and time < 1234569 and " +
                   "myfamily = 'family1' and myseries = 'series1'";

Query query = new Query.Builder(queryText).build();
QueryResult queryResult = client.execute(query);
```

```python
fmt = """
select weather, temperature from GeoCheckin where
    time > {t1} and time < {t2} and
    myfamily = 'family1' and myseries = 'series1'
"""
query = fmt.format(t1=tenMinsAgoMsec, t2=nowMsec)
ts_obj = client.ts_query('GeoCheckin', query)
```


###Extended Query

You can extend the query beyond the primary key and use secondary columns to filter results. In this example, we are extending our query to filter based on the `temperature` column:

```
select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1' and temperature > 27.0
```

Client-specific examples:

```erlang
riakc_ts:query(Pid, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1' and temperature > 27.0").
```

```java
String queryText = "select weather, temperature from GeoCheckin " +
                   "where time > 1234560 and time < 1234569 and " +
                   "myfamily = 'family1' and myseries = 'series1' " +
                   "temperature > 27.0";

Query query = new Query.Builder(queryText).build();
QueryResult queryResult = client.execute(query);
```

```python
fmt = """
select weather, temperature from GeoCheckin where
    time > {t1} and time < {t2} and
    myfamily = 'family1' and myseries = 'series1' and
    temperature > 27.0
"""
query = fmt.format(t1=tenMinsAgoMsec, t2=nowMsec)
ts_obj = client.ts_query('GeoCheckin', query)
```

You can also use `or` when querying against values not in the primary key, such as `temperature` in our example. Note that the parentheses are required:

```
select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1' and (temperature > 27.0 or temperature < 0.0)
```

You cannot use `or` between two complete clauses, since keys cannot be specified twice.

###SQL Injection

When querying with user-supplied data, it is essential that you protect against SQL injection. Please verify the user-supplied data before constructing queries.


##SQL Support

A small subset of SQL is supported. All columns are of the format: 

```
Field    Operator   Constant
````

The following operators are supported for each data type:

| |=|!=|>|<|<=|>=|
|-----------|---|---|---|---|---|---|
| varchar   | X | X |   |   |   |   |
| boolean   | X | X |   |   |   |   |
| sint64    | X | X | X | X | X | X |
| double    | X | X | X | X | X | X |
| timestamp | X | X | X | X | X | X |


###Limitations

* Column to column comparisons are not currently supported.
* Secondary indexing (2i) will not work with Riak TS.
* Riak search will not work with Riak TS.
* Your query can only range over up to 4 quanta. See below for more detail.

####Quanta query range

In this early version queries can only range over 1 to 4 quanta. A query covering more than 4 quanta will generate too many sub-queries and the query system will refuse to run it. Assuming a default quanta of 15min, the maximum query time range is 1hr. 

The quanta is configurable but the 4 quanta limitation is not. In the below example we set a quanta of 15s:

```sql
CREATE TABLE GeoCheckin
 (geohash varchar not null,
  location varchar not null,
  user varchar not null,
  time timestamp not null,
  weather varchar not null,
  temperature varchar,
    PRIMARY KEY((location, user, quantum(time, 15, 's')),
                location, user, time))
```

The maximum time range we can query is 60s, anything beyond will fail.