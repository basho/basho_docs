---
title: "Querying Data in Riak TS"
description: "Querying Data in Riak TS"
menu:
  riak_ts-1.3.0:
    name: "Query Data"
    identifier: "querying_data_riakts"
    weight: 304
    parent: "using"
project: "riak_ts"
project_version: "1.3.0"
toc: true
aliases:
    - /riakts/1.3.0/using/querying/
canonical_link: "https://docs.basho.com/riak/ts/latest/using/querying"
---


[table arch]: ../../learn-about/tablearchitecture
[activating]: ../creating-activating/
[writing]: ../writingdata/
[planning]: ../planning#column-definitions


Now that you have [created][activating] a Riak TS table and [written][writing] data to it, you can query your data.


## Basic Querying

You query data via columns. There are three categories of column, each with a different set of rules for valid queries. Query columns are based on rows in your TS table.

```
CREATE TABLE tab2(
a SINT64 NOT NULL,
b TIMESTAMP NOT NULL,
c BOOLEAN NOT NULL,
PRIMARY KEY  ((a, QUANTUM(b, 1, 's'))<-Partition Key, a,b,c)<-Local Key)
```


### Partition Key

All queries must cover the partition key fields and must use greater than and less than (>, >=, <, <=).

All unquantized fields in your partition key must be included in the query as exact matches.

Any quantized field in your partition key must be included in the query as either an exact match or a bounded range.

* Valid: `time > 1449864277000 and time < 1449864290000`
* Invalid: `time > 1449864277000`
* Invalid: `time > 1449864277000 or time < 1449864290000`


### Local Key

For any field in the local key that is not in the partition key, your
query can include any operator supported for that field's type and
bounded ranges are not required.

```
PRIMARY KEY ((a,b),a,b,c)
```

Here 'c' is in the local key only so does not have to be in the query.

Column names from the local key must be compared using strict equality against literal values. No ranges are permitted, `!=` must not be used, and `or` will not work.

* Valid: `country_code = 'uk'`
* Invalid: `(country_code = 'uk' or country_code = 'de')`
* Invalid: `country_code != 'se'`
* Invalid: `temperature < 85.0`


### Column Definitions

[Column definitions][planning] may be queried with unbounded ranges, `!=`, and `or` comparisons.


### General Guidelines

Before you begin querying, there are some guidelines to keep in mind.

* Columns may not be compared against other columns in the query.
* When using `or`, you must surround the expression with parentheses or your query will return an error.

Basic queries return the full range of values between two given times for an instance within a class or type of data. To demonstrate, we'll use the same example table, in which the `state` is an instance within the `region`:

```sql
CREATE TABLE GeoCheckin
(
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (region, state, QUANTUM(time, 15, 'm')),
     region, state, time
   )
)
```

Your query must include all components of the partition key. If any part of the partition key is missing, you will get an error.


## Advanced Querying By Column

### Select Query

You can select particular columns from the data to query:

```
select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina'
```

Client-specific examples:

```java
String queryText = "select weather, temperature from GeoCheckin " +
                   "where time > 1234560 and time < 1234569 and " +
                   "region = 'South Atlantic' and state = 'South Carolina'";

Query query = new Query.Builder(queryText).build();
QueryResult queryResult = client.execute(query);
```

```ruby
Riak::Timeseries::Query.new(client, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina'").issue!
```

```python
fmt = """
select weather, temperature from GeoCheckin where
    time > {t1} and time < {t2} and
    region = 'South Atlantic' and state = 'South Carolina'
"""
query = fmt.format(t1=tenMinsAgoMsec, t2=nowMsec)
ts_obj = client.ts_query('GeoCheckin', query)
```

```csharp
var now = DateTime.UtcNow;
var tenMinsAgo = now.AddMinutes(-10);
var qfmt = "SELECT * FROM GeoCheckin WHERE time > {0} and time < {1} and region = 'South Atlantic' and state = 'South Carolina'";
var q = string.Format(
    qfmt,
    DateTimeUtil.ToUnixTimeMillis(tenMinsAgo),
    DateTimeUtil.ToUnixTimeMillis(now));

var cmd = new Query.Builder()
    .WithTable("GeoCheckin")
    .WithQuery(q)
    .Build();

RiakResult rslt = client.Execute(cmd);
```

```javascript
var callback = function(err, resp) {
    // resp.rows and resp.columns
    // have data
};
// NB: t1/t2 are integers representing unix timestamps with
// millisecond resolution
var queryText = "select * from GeoCheckin where time > " + t1 +
                " and time < " + t2 +
                " and region = 'South Atlantic' and state = 'South Carolina'";
var q = new Riak.Commands.TS.Query.Builder()
    .withQuery(queryText)
    .withCallback(callback)
    .build();
cluster.execute(q);
```

```erlang
riakc_ts:query(Pid, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina'").
```

```php
$response = (new Command\Builder\TimeSeries\Query($riak))
    ->withQuery("select weather, temperature from GeoCheckin where region = 'South Atlantic' and state = 'state1' and time > 1234560 and time < 1234569")
    ->build()
    ->execute();
```

```golang
cmd, err := riak.NewTsQueryCommandBuilder()
    .WithQuery("select weather, temperature from GeoCheckin where region = 'South Atlantic' and state = 'state1' and time > 1234560 and time < 1234569")
    .Build()

if err != nil {
    return err
}

err = cluster.Execute(cmd)
```

### Extended Query

You can extend the query beyond the primary key and use secondary columns to filter results. In this example, we are extending our query to filter based on `temperature`:

```
select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina' and temperature > 27.0
```

Client-specific examples:

```java
String queryText = "select weather, temperature from GeoCheckin " +
                   "where time > 1234560 and time < 1234569 and " +
                   "region = 'South Atlantic' and state = 'South Carolina' " +
                   "temperature > 27.0";

Query query = new Query.Builder(queryText).build();
QueryResult queryResult = client.execute(query);
```

```ruby
Riak::Timeseries::Query.new(client, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina' and temperature > 27.0").issue!
```

```python
fmt = """
select weather, temperature from GeoCheckin where
    time > {t1} and time < {t2} and
    region = 'South Atlantic' and state = 'South Carolina' and
    temperature > 27.0
"""
query = fmt.format(t1=tenMinsAgoMsec, t2=nowMsec)
ts_obj = client.ts_query('GeoCheckin', query)
```

```csharp
var now = DateTime.UtcNow;
var tenMinsAgo = now.AddMinutes(-10);
var qfmt = "SELECT weather, temperature FROM GeoCheckin WHERE time > {0} and time < {1} and region = 'South Atlantic' and state = 'South Carolina' and temperature > 27.0";
var q = string.Format(
    qfmt,
    DateTimeUtil.ToUnixTimeMillis(tenMinsAgo),
    DateTimeUtil.ToUnixTimeMillis(now));

var cmd = new Query.Builder()
    .WithTable("GeoCheckin")
    .WithQuery(q)
    .Build();

RiakResult rslt = client.Execute(cmd);
```

```javascript
var callback = function(err, resp) {
    // resp.rows and resp.columns
    // have data
};
// NB: t1/t2 are integers representing unix timestamps with
// millisecond resolution
var queryText = "select weather, temperature from GeoCheckin where time > " + t1 +
                " and time < " + t2 +
                " and region = 'South Atlantic' and state = 'South Carolina'" +
                " and temperature > 27.0";
var q = new Riak.Commands.TS.Query.Builder()
    .withQuery(queryText)
    .withCallback(callback)
    .build();
cluster.execute(q);
```

```erlang
riakc_ts:query(Pid, "select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina' and temperature > 27.0").
```

```php
$response = (new Command\Builder\TimeSeries\Query($riak))
    ->withQuery("select weather, temperature from GeoCheckin where region = 'South Atlantic' and state = 'state1' and time > 1234560 and time < 1234569 and temperature > 27.0")
    ->build()
    ->execute();
```

```golang
cmd, err := riak.NewTsQueryCommandBuilder()
    .WithQuery("select weather, temperature from GeoCheckin where region = 'South Atlantic' and state = 'state1' and time > 1234560 and time < 1234569 and temperature > 27.0")
    .Build()

if err != nil {
    return err
}

err = cluster.Execute(cmd)
```

You can also use `or` when querying against column values, such as `temperature` in our example. Note that the parentheses are required:

```
select weather, temperature from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina' and (temperature > 27.0 or temperature < 0.0)
```

You cannot use `or` between two complete clauses, since keys cannot be specified twice.


### SQL Injection

When querying with user-supplied data, it is essential that you protect against SQL injection. Please verify the user-supplied data before constructing queries.


## SQL Support

A small subset of SQL is supported. All columns are of the format:

```sql
Field    Operator   Constant
```

The following operators are supported for each data type:

|           |=  |!= |>  |<  |<= |>=|
|-----------|---|---|---|---|---|---|
| varchar   | X | X |   |   |   |   |
| boolean   | X | X |   |   |   |   |
| sint64    | X | X | X | X | X | X |
| double    | X | X | X | X | X | X |
| timestamp | X | X | X | X | X | X |


### Limitations

* Column to column comparisons are not currently supported.
* Secondary indexing (2i) will not work with Riak TS.
* Riak Search will not work with Riak TS.
* Queries are limited by the number of quanta they can span when specifying the time limits.


#### Quanta query range

A query covering more than a certain number of quanta (5 by default) will generate the error `too_many_subqueries` and the query system will refuse to run it. Assuming a default quantum of 15 minutes, the maximum query time range is 75 minutes.

In the below example we set a quantum of 15s:

```sql
CREATE TABLE GeoCheckin
 (geohash VARCHAR NOT NULL,
  location VARCHAR NOT NULL,
  user VARCHAR NOT NULL,
  time TIMESTAMP NOT NULL,
  weather VARCHAR NOT NULL,
  temperature VARCHAR,
    PRIMARY KEY((location, user, QUANTUM(time, 15, 's')),
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

In the following client-specific examples we'll specify columns by selecting all fields from the GeoCheckin table where `time`, `region`, and `state` match our supplied parameters:

```java
import java.net.UnknownHostException;
import java.util.concurrent.ExecutionException;
import com.basho.riak.client.api.RiakClient;
import com.basho.riak.client.api.commands.timeseries.Query;
import com.basho.riak.client.core.query.timeseries.*;
import java.util.*;

public class RiakTSQuery
{
    public static void main(String [] args) throws UnknownHostException, ExecutionException, InterruptedException
    {
        // Riak Client with supplied IP and Port
        RiakClient client = RiakClient.newClient(10017, "myriakdb.host");
        String queryText = "select * from GeoCheckin " + "where time >= 1234567 and time <= 1234567 and " + "region = 'South Atlantic' and state = 'South Carolina' ";
        Query query = new Query.Builder(queryText).build();
        QueryResult queryResult = client.execute(query);
        List<Row> rows = queryResult.getRowsCopy();
        client.shutdown();
    }
}
```

```ruby
client = Riak::Client.new 'myriakdb.host', pb_port: 10017
query = Riak::Timeseries::Query.new client, "select * from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina'"
results = query.issue!
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
    region = 'South Atlantic' and state = 'South Carolina'
"""
query = fmt.format(t1=tenMinsAgoMS, t2=tenMinsFromNowMS)

ts_obj = client.ts_query('GeoCheckin', query)
print "Query result rows:", ts_obj.rows
```

```csharp
var now = DateTime.UtcNow;
var tenMinsAgo = now.AddMinutes(-10);
var tenMinsFromNow = now.AddMinutes(10);
var qfmt = "SELECT * FROM GeoCheckin WHERE time > {0} and time < {1} and region = 'South Atlantic' and state = 'South Carolina'";
var q = string.Format(
    qfmt,
    DateTimeUtil.ToUnixTimeMillis(tenMinsAgo),
    DateTimeUtil.ToUnixTimeMillis(tenMinsFromNow));

var cmd = new Query.Builder()
    .WithTable("GeoCheckin")
    .WithQuery(q)
    .Build();

RiakResult rslt = client.Execute(cmd);
```

```javascript
var callback = function(err, resp) {
    // resp.rows and resp.columns
    // have data
};

// This demonstrates getting the current timestamp in UTC with MS resolution
var now = new Date();
var nowUtcMs = now.getTime() + now.getTimezoneOffset() * 60000;
var tenMinsInMsec = 10 * 60 * 1000;
var tenMinsAgo = nowUtcMs - tenMinsInMsec;
var tenMinsFromNow = nowUtcMs + tenMinsInMsec;

var queryText = "select weather, temperature from GeoCheckin where time > " + tenMinsAgo +
                " and time < " + tenMinsFromNow +
                " and region = 'South Atlantic' and state = 'South Carolina'";
var q = new Riak.Commands.TS.Query.Builder()
    .withQuery(queryText)
    .withCallback(callback)
    .build();
cluster.execute(q);
```

```erlang
{ok, Pid} = riakc_pb_socket:start_link("myriakdb.host", 10017).
riakc_ts:query(Pid, "select * from GeoCheckin where time > 1234560 and time < 1234569 and region = 'South Atlantic' and state = 'South Carolina'").
```

```php
$response = (new Command\Builder\TimeSeries\Query($riak))
    ->withQuery("select * from GeoCheckin where region = 'South Atlantic' and state = 'South Carolina' and (time > 1234560 and time < 1234569)")
    ->build()
    ->execute();
```

```golang
cmd, err := riak.NewTsQueryCommandBuilder()
    .WithQuery("select * from GeoCheckin where region = 'South Atlantic' and state = 'South Carolina' and (time > 1234560 and time < 1234569)")
    .Build()

if err != nil {
    return err
}

err = cluster.Execute(cmd)
```

### Query a table definition

You can now query a table definition with the `DESCRIBE` table query which returns the table's information in rows and columns.

For example:

```sql
DESCRIBE GeoCheckin
```

Returns:

```
Column      | Type      | Is Null | Partition Key | Local Key
--------------------------------------------------------
region      | varchar   | false   | 1             | 1
state       | varchar   | false   | 2             | 2
time        | timestamp | false   | 3             | 3
weather     | varchar   | false   | <null>        | <null>
temperature | double    | false   | <null>        | <null>
```

A successful `DESCRIBE` statement execution will return a language-specific representation of the table.

* **Java** - Use a `Query` command to execute a `DESCRIBE` statement.
* **Ruby** - Use the `Riak::TimeSeries::Query` object to execute the `DESCRIBE` statement. The returned results will have a collection of rows as well as a `columns` property corresponding to the above table.
* **Python** - either the `ts_query` or `ts_describe` methods of the client object can be used to executed a `DESCRIBE` statement. In both cases, the response object will have `columns` and `rows` properties corresponding to the above table.
* **C#** - Use a `Query` command to execute a `DESCRIBE` statement.
* **Node.js** - you may use the `TS.Query` command to execute a `DESCRIBE` statement, or use the purpose-built `TS.Describe` command. In both cases, the response object will have `columns` and `rows` properties corresponding to the above table.


## Single Key Fetch

You may find the need to fetch a single key from Riak TS, below you will find an example of how to do that in each of our official clients that support Riak TS.

```java
final List<Cell> keyCells = Arrays.asList(new Cell("South Atlantic"), new Cell("South Carolina"), Cell.newTimestamp(1420113600000));

Fetch fetch = new Fetch.Builder("GeoCheckin", keyCells).build();

QueryResult queryResult = client.execute(fetch);
```

```ruby
read_operation = Riak::TimeSeries::Read.new client, 'GeoCheckin'
read_operation.key = ['South Atlantic', 'South Carolina', 1420113600000]
results = read_operation.read!
```

```python
client.ts_get('GeoCheckin', ['South Atlantic', 'South Carolina', datetime.datetime(2015, 1, 1, 12, 0, 0)])
```

```csharp
var keyCells = new Cell[]
{
    new Cell<string>("hash1"),
    new Cell<string>("user2"),
    new Cell<DateTime>(FiveMinsAgo)
};

var key = new Row(keyCells);

var cmd = new Get.Builder()
    .WithTable("GeoCheckin")
    .WithKey(key)
    .Build();

RiakResult rslt = client.Execute(cmd);
```

```javascript
var key = [ 'South Atlantic', 'South Carolina', 1420113600000 ];

var cb = function (err, rslt) {
    // NB: rslt will be an object with two properties:
    // 'columns' - table columns
    // 'rows' - row matching the Get request
};

var cmd = new Riak.Commands.TS.Get.Builder()
    .withTable('GeoCheckin')
    .withKey(key)
    .withCallback(cb)
    .build();

client.execute(cmd);
```

```erlang
riakc_ts:get(Pid, <<"GeoCheckin">>, [<<"South Atlantic">>, <<"South Carolina">>, 1420113600000]).
```

```php
$response = (new Command\Builder\TimeSeries\FetchRow($riak))
    ->atKey([
        (new Cell("region"))->setValue("South Atlantic"),
        (new Cell("state"))->setValue("South Carolina"),
        (new Cell("time"))->setTimestampValue(1420113600),
    ])
    ->inTable('GeoCheckins')
    ->build()
    ->execute();
```

```golang
key := make([]riak.TsCell, 3)

key[0] = NewStringTsCell("South Atlantic")
key[1] = NewStringTsCell("South Carolina")
key[2] = NewTimestampTsCell(1420113600)

cmd, err := riak.NewTsFetchRowCommandBuilder()
    .WithTable("GeoCheckin").WithKey(key)
    .Build()

if err != nil {
    return err
}

err = cluster.Execute(cmd)
```
