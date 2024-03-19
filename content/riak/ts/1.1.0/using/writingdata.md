---
title: "Writing Data to Riak TS"
description: "Writing Data to Riak TS"
menu:
  riak_ts-1.1.0:
    name: "Write Data"
    identifier: "writing_data_riakts"
    weight: 303
    parent: "using"
project: "riak_ts"
project_version: "1.1.0"
lastmod: 2016-01-14T00:00:00-00:00
sitemap:
  priority: 0.1
toc: true
aliases:
    - /riakts/1.1.0/using/writingdata/
---

[activating]: ../creating-activating/
[planning]: ../planning/
[querying]: ../querying/

Now that you've [planned][planning] and [activated][activating] your Riak TS table, you are ready to write data to it.

## Writing Data

Riak TS allows you to write multiple rows of data at a time. To demonstrate, we'll use the example table from earlier:

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

To write data to your table, put the data in a list:

```erlang
{ok, Pid} = riakc_pb_socket:start_link("myriakdb.host", 10017).
riakc_ts:put(Pid, "GeoCheckin", [[<<"family1">>, <<"series1">>, 1234567, <<"hot">>, 23.5], [<<"family2">>, <<"series99">>, 1234567, <<"windy">>, 19.8]]).
```

```java
import java.net.UnknownHostException;
import java.util.concurrent.ExecutionException;
import com.basho.riak.client.api.RiakClient;
import com.basho.riak.client.api.commands.timeseries.Store;
import com.basho.riak.client.core.query.timeseries.*;
import java.util.*;
public class RiakTSInsert {
  public static void main(String [] args)
    throws UnknownHostException, ExecutionException, InterruptedException
  {
    // Riak Client with supplied IP and Port
    RiakClient client = RiakClient.newClient(10017, "myriakdb.host");
    List<Row> rows = Arrays.asList(
      new Row(
        new Cell("family1"),
        new Cell("series1"),
        Cell.newTimestamp(1234567),
        new Cell("hot"),
        new Cell(23.5)
      ),
      new Row(
        new Cell("family2"),
        new Cell("series99"),
        Cell.newTimestamp(1234567),
        new Cell("windy"),
        new Cell(19.8)
      )
    );

    Store storeCmd = new Store.Builder("GeoCheckin").withRows(rows).build();
    client.execute(storeCmd);
    client.shutdown();
  }
}
```

```python
import datetime
from riak.client import RiakClient

# NB: modify 'host' and 'pb_port' to match your installation
client = RiakClient(host='myriakdb.host', pb_port=8087)

fiveMins = datetime.timedelta(0, 300)
ts0 = datetime.datetime(2015, 1, 1, 12, 0, 0)
ts1 = ts0 + fiveMins

table = client.table('GeoCheckin')
rows = [
    ['family1', 'series1', ts0, 'hot', 23.5],
    ['family1', 'series1', ts1, 'windy', 19.8]
]
ts_obj = table.new(rows)
print "Store result:", ts_obj.store()
```

```ruby
client = Riak::Client.new 'myriakdb.host', pb_port: 10017
submission = Riak::TimeSeries::Submission.new client, "GeoCheckin"
submission.measurements = [["family1", "series1", 1234567, "hot", 23.5], ["family2", "series99", 1234567, "windy", 19.8]]
submission.write!
```

```javascript
var Riak = require('basho-riak-client');

var hosts = [ 'riak-1:8087', 'riak-2:8087' ];
var client = new Riak.Client(hosts);

var columns = [
    { name: 'geohash',     type: Riak.Commands.TS.ColumnType.Varchar },
    { name: 'user',        type: Riak.Commands.TS.ColumnType.Varchar },
    { name: 'time',        type: Riak.Commands.TS.ColumnType.Timestamp },
    { name: 'weather',     type: Riak.Commands.TS.ColumnType.Varchar },
    { name: 'temperature', type: Riak.Commands.TS.ColumnType.Double }
];

var rows = [
    [ 'hash1', 'user2', twentyMinsAgo, 'hurricane', 82.3 ],
    [ 'hash1', 'user2', fifteenMinsAgo, 'rain', 79.0 ],
    [ 'hash1', 'user2', fiveMinsAgo, 'wind', null ],
    [ 'hash1', 'user2', now, 'snow', 20.1 ]
];

var cb = function (err, response) {
    // NB: response will be true on success
};

var store = new Riak.Commands.TS.Store.Builder()
    .withTable('GeoCheckin')
    // NB: withColumns is optional
    // TS column types will be inferred if it's omitted
    .withColumns(columns)
    .withRows(rows)
    .withCallback(cb)
    .build();

client.execute(store);
```

>**Note on validation**:
>
>Riak TS 1.1.0 validates all rows on the server side before writing occurs, checking the number of row elements and types. If any of the rows fails validation then none of the rows will be written.  An error message will be returned with the index numbers of the invalid rows in the batch. The first item in the batch being index one.

Depending on your client, you will recieve different messages indicating whether or not your write was successful.

Successful responses:

* Erlang - `ok`
* Java - `void`, not throwing an exception indicates a successful write
* Python - `True`
* Ruby - `void`, not raising an error indicates a successful write
* Node.js - `true`

Failure responses:

* Erlang - `RpbErrorResp`
* Java - exceptions will be thrown
* Python - exceptions will be thrown
* Ruby - `RpbErrorResp` with errors
* Node.js - The `err` callback parameter will have information, and the `response` parameter will be `false`

In the event that your write fails, you should check the error message to see which rows failed validation. For example:

```
RiakClient client = RiakClient.newClient(10017, "myriakdb.host");
List<Row> someRows = Arrays.asList(
        // Good Row
        new Row(new Cell("hash1"), new Cell("user1"), Cell.newTimestamp(1234567), new Cell("cloudy"), new Cell(79.0)),
        // Bad Rows
        new Row(new Cell("hash1"), Cell.newTimestamp(fiveMinsAgo)), // too short
        new Row() // no data
        );

Store store = new Store.Builder("GeoCheckin").withRows(someRows).build();
final RiakFuture<Void, String> storeFuture = client.executeAsync(store);

storeFuture.await();
assertFalse(storeFuture.isSuccess());
System.out.println(storeFuture.cause().detailMessage);
// Prints "Invalid data found at row index(es) 2, 3"
```

You could also try the original write again. Failures may be transitory when servers are temporarily unable to talk to each other.

### Guidelines

* Batches should not be too large. In our testing, 100 rows per write is a sweet spot, but you should expect different results depending on your hardware and schema.
* Writes will assume that columns are in the same order they've been declared in the table.
* Timestamps should be in Unix epoch/UTC milliseconds.

## Deleting Data

Below are examples of how to delete data by key in each of our time series supported clients.

```erlang
riakc_ts:delete(Pid, <<"GeoCheckins">>, [<<"family1">>, <<"series1">>, 1420113600000]).
```

```java
final List<Cell> keyCells = Arrays.asList(
  new Cell("family1"), new Cell("series1"), Cell.newTimestamp(1420113600000));

Delete delete = new Delete.Builder("GeoCheckins", keyCells).build();

client.execute(delete);
```

```python
client.ts_delete('GeoCheckin', ['family1', 'series1', datetime.datetime(2015, 1, 1, 12, 0, 0)])
```

```ruby
delete_operation = Riak::TimeSeries::Deletion.new client, 'GeoCheckins'
delete_operation.key = ['family1', 'series1', 1420113600000]
delete_operation.delete!
```

```javascript
var key = [ 'family1', 'series1', 1420113600000 ];

var cb = function (err, response) {
    // NB: response will be true on success
};

var cmd = new Riak.Commands.TS.Delete.Builder()
    .withTable('GeoCheckins')
    .withKey(key)
    .withCallback(cb)
    .build();

client.execute(cmd);
```

## Next Steps

Now that you've written data to your tables, you can [query][querying] your data.
