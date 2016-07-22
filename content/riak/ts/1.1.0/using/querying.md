---
title: "Querying Data in Riak TS"
description: "Querying Data in Riak TS"
menu:
  riak_ts-1.1.0:
    name: "Query Data"
    identifier: "querying_data_riakts"
    weight: 304
    parent: "using"
project: "riak_ts"
project_version: "1.1.0"
toc: true
aliases:
    - /riakts/1.1.0/using/querying/
canonical_link: "https://docs.basho.com/riak/ts/latest/using/querying"
---

[table arch]: ../../learn-about/tablearchitecture
[activating]: ../creating-activating/
[writing]: ../writingdata/


Now that you have [created][activating] a Riak TS table and [written][writing] data to it, you can query your data.


## Basic Querying

When querying your data via fields, there are three categories of fields, each with a different set of rules for valid queries.


### Timestamp in the primary key

The timestamp in the primary key is an integer (in milliseconds) that must be compared either as a fully-enclosed range or as an exact match.

* Valid: `time > 1449864277000 and time < 1449864290000`
* Invalid: `time > 1449864277000`
* Invalid: `time > 1449864277000 or time < 1449864290000`


### Other fields in the primary key

The other two fields in the primary key must be compared using strict equality against literal values. No ranges are permitted, `!=` must not be used, and `or` will not work.

* Valid: `country_code = 'uk'`
* Invalid: `(country_code = 'uk' or country_code = 'de')`
* Invalid: `country_code != 'se'`
* Invalid: `temperature < 85.0`


### Fields not in the primary key

These fields may be queried with unbounded ranges, `!=`, and `or` comparisons.


### General Guidelines

Before you begin querying, there are some guidelines to keep in mind.

* Fields may not be compared against other fields in the query.
* All elements of the compound primary key must be present.
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


## Advanced Querying By Field

### Select Query

You can select particular fields from the data to query:

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

```ruby
Riak::Timeseries::Query.new(client, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1'").issue!
```


### Extended Query

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

```ruby
Riak::Timeseries::Query.new(client, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1' and temperature > 27.0").issue!
```

You can also use `or` when querying against values not in the primary key, such as `temperature` in our example. Note that the parentheses are required:

```
select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1' and (temperature > 27.0 or temperature < 0.0)
```

You cannot use `or` between two complete clauses, since keys cannot be specified twice.

### SQL Injection

When querying with user-supplied data, it is essential that you protect against SQL injection. Please verify the user-supplied data before constructing queries.


## SQL Support

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


### Limitations

* Column to column comparisons are not currently supported.
* Secondary indexing (2i) will not work with Riak TS.
* Riak search will not work with Riak TS.
* Queries are limited by the number of quanta they can span when specifying the time limits.


#### Quanta query range

A query covering more than a certain number of quanta (5 by default) will generate the error `too_many_subqueries` and the query system will refuse to run it. Assuming a default quanta of 15 minutes, the maximum query time range is 75 minutes.

In the below example we set a quanta of 15s:

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

See the Data Modeling section in [Table Architecture][table arch] for more information.

#### Leap seconds and quantum boundaries

Periodically [leap seconds](https://en.wikipedia.org/wiki/Leap_second)
are announced. These are inserted at the end of one day (in UTC).

UNIX treats them as one double-length second. For example, at the end of 1998 a second was added:

```
Date         Time of day   UNIX time
1998-12-31   23:59:58      915148798
1998-12-31   23:59:59      915148799
1998-12-31   23:59:60      915148800     <== Artificial leap second
1999-01-01   00:00:00      915148800
```

Effectively, there is no way in the UNIX time scheme to differentiate an event that occurred during the extra second at the end of 1998 to something that occurred the first second of 1999.

Similarly, Riak TS would treat `915148800` as the start of a new time quantum, and any data points which a client added for that second would be considered to be in the first time quantum in 1999.

The data is not lost, but a query against 1998 time quanta will not produce those data points despite the fact that some of the events flagged as `915148800` technically occurred in 1998.


## Querying Tables

### Query a table with SQL

Query a table by issuing a SQL statement against the table. Your query MUST include a 'where' clause with all components.

In the following client-specific examples we'll select all fields from the GeoCheckin table where `time`, `myfamily`, and `myseries` match our supplied parameters:

```erlang
{ok, Pid} = riakc_pb_socket:start_link("myriakdb.host", 10017).
riakc_ts:query(Pid, "select * from GeoCheckin where time > 1234560 and time < 1234569 and myfamily = 'family1' and myseries = 'series1'").
```

```java
import java.net.UnknownHostException;
import java.util.concurrent.ExecutionException;
import com.basho.riak.client.api.RiakClient;
import com.basho.riak.client.api.commands.timeseries.Query;
import com.basho.riak.client.core.query.timeseries.*;
import java.util.*;
public class RiakTSQuery {
public static void main(String [] args) throws UnknownHostException, ExecutionException, InterruptedException
{ // Riak Client with supplied IP and Port RiakClient client = RiakClient.newClient(10017, "myriakdb.host"); String queryText = "select * from GeoCheckin " + "where time >= 1234567 and time <= 1234567 and " + "myfamily = 'family1' and myseries = 'series1' "; Query query = new Query.Builder(queryText).build(); QueryResult queryResult = client.execute(query); List<Row> rows = queryResult.getRowsCopy(); client.shutdown(); }
}
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

### Query a table definition

You can now query a table definition with the `DESCRIBE` table query which returns the table's rows and columns.

For example:

```sql
DESCRIBE GeoCheckin
```

Returns:
 (Rows and Columns)

```
Column      | Type      | Is Null | Partition Key | Local Key
--------------------------------------------------------
myfamily    | varchar   | false   | 1             | 1
myseries    | varchar   | false   | 2             | 2
time        | timestamp | false   | 3             | 3
weather     | varchar   | false   | <null>        | <null>
temperature | double    | false   | <null>        | <null>
```

A successful DESCRIBE statement execution will return a language-specific representation of the table.

* **Node.js** - you may use the `TS.Query` command to execute a `DESCRIBE` statement, or use the purpose-built `TS.Describe` command. In both cases, the response object will have `columns` and `rows` properties corresponding to the above table.
* **Python** - either the `ts_query` or `ts_describe` methods of the client object can be used to executed a `DESCRIBE` statement. In both cases, the response object will have `columns` and `rows` properties corresponding to the above table.
* **Ruby** - Use the `Riak::TimeSeries::Query` object to execute the DESCRIBE statement. The returned results will have a collection of rows as well as a `columns` property corresponding to the above table.


## Single Key Fetch

You may find the need to fetch a single key from Riak TS, below you will find an example of how to do that in each of our official clients that support Riak TS.

```erlang
riakc_ts:get(Pid, <<"GeoCheckins">>, [<<"family1">>, <<"series1">>, 1420113600000]).
```

```java
final List<Cell> keyCells = Arrays.asList(new Cell("family1"), new Cell("series1"), Cell.newTimestamp(1420113600000));

Fetch fetch = new Fetch.Builder("GeoCheckins", keyCells).build();

QueryResult queryResult = client.execute(fetch);
```

```python
client.ts_get('GeoCheckins', ['family1', 'series1', datetime.datetime(2015, 1, 1, 12, 0, 0)])
```

```ruby
read_operation = Riak::TimeSeries::Read.new client, 'GeoCheckins'
read_operation.key = ['family1', 'series1', 1420113600000]
results = read_operation.read!
```

```javascript
var key = [ 'family1', 'series1', 1420113600000 ];

var cb = function (err, rslt) {
    // NB: rslt will be an object with two properties:
    // 'columns' - table columns
    // 'rows' - row matching the Get request
};

var cmd = new Riak.Commands.TS.Get.Builder()
    .withTable('GeoCheckins')
    .withKey(key)
    .withCallback(cb)
    .build();

client.execute(cmd);
```
