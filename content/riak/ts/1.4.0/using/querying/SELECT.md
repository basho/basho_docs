---
title: "Basic Querying in Riak TS"
description: "The basics of querying data in Riak TS"
menu:
  riak_ts-1.4.0:
    name: "Basic Querying"
    identifier: "basic_querying_riakts"
    weight: 110
    parent: "querying"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/querying/basic-querying
canonical_link: "https://docs.basho.com/riak/ts/latest/using/querying/basic-querying"
---

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
