---
title: "Writing Data to Riak TS"
description: "Writing Data to Riak TS"
menu:
  riak_ts-1.3.1:
    name: "Write Data"
    identifier: "writing_data_riakts"
    weight: 303
    parent: "using"
project: "riak_ts"
project_version: "1.3.1"
toc: true
aliases:
    - /riakts/1.3.1/using/writingdata/
---


[activating]: ../creating-activating/
[planning]: ../planning/
[querying]: ../querying/
[config reference]: {{<baseurl>}}riak/kv/2.1.4/configuring/reference/#the-advanced-config-file
[MDC]: {{<baseurl>}}riak/ts/1.3.1/using/mdc
[riakshell]: ../riakshell


Now that you've [planned][planning] and [activated][activating] your Riak TS table, you are ready to write data to it.


## Writing Data

Riak TS allows you to write multiple rows of data at a time. To demonstrate, we'll use the example table from earlier:

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

To write data to your table, put the data in a list:

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
        new Cell("South Atlantic"),
        new Cell("South Carolina"),
        Cell.newTimestamp(1234567),
        new Cell("hot"),
        new Cell(23.5)
      ),
      new Row(
        new Cell("Mid-Atlantic"),
        new Cell("New York"),
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

```ruby
client = Riak::Client.new 'myriakdb.host', pb_port: 10017
submission = Riak::TimeSeries::Submission.new client, "GeoCheckin"
submission.measurements = [["South Atlantic", "South Carolina", 1234567, "hot", 23.5], ["Mid-Atlantic", "New York", 1234567, "windy", 19.8]]
submission.write!
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
    ['South Atlantic', 'South Carolina', ts0, 'hot', 23.5],
    ['Mid-Atlantic', 'New York', ts1, 'windy', 19.8]
]
ts_obj = table.new(rows)
print "Store result:", ts_obj.store()
```

```csharp
var cells0 = new Cell[]
{
    new Cell<string>("Pacific"),
    new Cell<string>("Washington"),
    new Cell<DateTime>(TwentyMinsAgo),
    new Cell<string>("hurricane"),
    new Cell<double>(82.3)
};

var cells1 = new Cell[]
{
    new Cell<string>("Pacific"),
    new Cell<string>("Washington"),
    new Cell<DateTime>(FifteenMinsAgo),
    new Cell<string>("rain"),
    new Cell<double>(79.0)
};

var cells2 = new Cell[]
{
    new Cell<string>("Pacific"),
    new Cell<string>("Washington"),
    new Cell<DateTime>(FiveMinsAgo),
    new Cell<string>("wind"),
    Cell.Null
};

var cells3 = new Cell[]
{
    new Cell<string>("Pacific"),
    new Cell<string>("Washington"),
    new Cell<DateTime>(Now),
    new Cell<string>("snow"),
    new Cell<double>(20.1)
};

var rows = new Row[]
{
    new Row(cells0),
    new Row(cells1),
    new Row(cells2),
    new Row(cells3)
};

var columns = new Column[]
{
    new Column("region",      ColumnType.Varchar),
    new Column("state",       ColumnType.Varchar),
    new Column("time",        ColumnType.Timestamp),
    new Column("weather",     ColumnType.Varchar),
    new Column("temperature", ColumnType.Double)
};

var cmd = new Store.Builder()
        .WithTable(Table)
        .WithColumns(columns)
        .WithRows(rows)
        .Build();

RiakResult rslt = client.Execute(cmd);
```

```javascript
var Riak = require('basho-riak-client');

var hosts = [ 'riak-1:8087', 'riak-2:8087' ];
var client = new Riak.Client(hosts);

var columns = [
    { name: 'region',     type: Riak.Commands.TS.ColumnType.Varchar },
    { name: 'state',        type: Riak.Commands.TS.ColumnType.Varchar },
    { name: 'time',        type: Riak.Commands.TS.ColumnType.Timestamp },
    { name: 'weather',     type: Riak.Commands.TS.ColumnType.Varchar },
    { name: 'temperature', type: Riak.Commands.TS.ColumnType.Double }
];

var rows = [
    [ 'Pacific', 'Washington', twentyMinsAgo, 'hurricane', 82.3 ],
    [ 'Pacific', 'Washington', fifteenMinsAgo, 'rain', 79.0 ],
    [ 'Pacific', 'Washington', fiveMinsAgo, 'wind', null ],
    [ 'Pacific', 'Washington', now, 'snow', 20.1 ]
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

```erlang
%% TS 1.3 or newer. Records are represented as tuples.
{ok, Pid} = riakc_pb_socket:start_link("myriakdb.host", 10017).
riakc_ts:put(Pid, "GeoCheckin", [{<<"South Atlantic">>, <<"South Carolina">>, 1234567, <<"hot">>, 23.5}, {<<"Mid-Atlantic">>, <<"New York">>, 1234567, <<"windy">>, 19.8}]).

%% TS 1.2 or earlier. Records are represented as lists.
{ok, Pid} = riakc_pb_socket:start_link("myriakdb.host", 10017).
riakc_ts:put(Pid, "GeoCheckin", [[<<"South Atlantic">>, <<"South Carolina">>, 1234567, <<"hot">>, 23.5], [<<"Mid-Atlantic">>, <<"New York">>, 1234567, <<"windy">>, 19.8]]).
```

```php

require __DIR__ . '/../vendor/autoload.php';

use Basho\Riak;
use Basho\Riak\Command;
use Basho\Riak\Node;

$node = (new Node\Builder)
    ->atHost('riak-test')
    ->onPort(8087)
    ->build();

$riak = new Riak([$node], [], new Riak\Api\Pb());

$response = (new Command\Builder\TimeSeries\StoreRows($riak))
    ->inTable('GeoCheckins')
    ->withRow([
        (new Cell("region"))->setValue("South Atlantic"),
        (new Cell("state"))->setValue("South Carolina"),
        (new Cell("time"))->setTimestampValue(1420113600),
        (new Cell("weather"))->setValue("hot"),
        (new Cell("temperature"))->setValue(23.5),
    ])
    ->build()
    ->execute();

if (!$response->isSuccess()) {
    echo $response->getMessage();
    exit;
}
```

```golang
row := make([]TsCell, 5)

row[0] = NewStringTsCell("South Atlantic")
row[1] = NewStringTsCell("South Carolina")
row[2] = NewTimestampTsCell(1420113600)
row[3] = NewStringTsCell("hot")
row[4] = NewDoubleTsCell(23.5)

cmd, err := riak.NewTsStoreRowsCommandBuilder()
    .WithTable("GeoCheckin").WithRows([][]TsCell{row})
    .Build()

if err != nil {
    return err
}

err = cluster.Execute(cmd)
```

>**Note on validation**:
>
>Riak TS validates all rows on the server side before writing occurs, checking the number of row elements and types. If any of the rows fails validation then none of the rows will be written.  An error message will be returned with the index numbers of the invalid rows in the batch, the first item in the batch being index one.

Depending on your client, you will receive different messages indicating whether or not your write was successful.

Successful responses:

* Java - `void`, not throwing an exception indicates a successful write
* Ruby - `void`, not raising an error indicates a successful write
* Python - `True`
* C# - `IsSuccess` will be `true`
* Node.js - `true`
* Erlang - `ok`
* PHP - $response->isSuccess() == true;

Failure responses:

* Java - exceptions will be thrown
* Ruby - `RpbErrorResp` with errors
* Python - exceptions will be thrown
* C# - `IsSuccess` will be `false` and `ErrorMessage` will have information
* Node.js - The `err` callback parameter will have information, and the `response` parameter will be `false`
* Erlang - `RpbErrorResp`
* PHP - exceptions will be thrown

In the event that your write fails, you should check the error message to see which rows failed validation. For example:

```java
RiakClient client = RiakClient.newClient(10017, "myriakdb.host");
List<Row> someRows = Arrays.asList(
        // Good Row
        new Row(new Cell("East North Central"), new Cell("Ohio"), Cell.newTimestamp(1234567), new Cell("cloudy"), new Cell(79.0)),
        // Bad Rows
        new Row(new Cell("East North Central"), Cell.newTimestamp(fiveMinsAgo)), // too short
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

* Batches should not be too large. Our testing revealed 100 rows per write as a sweet spot, but you should expect different results depending on your hardware and schema.
* Writes will assume that fields are in the same order as they've been declared in the table.
* Timestamps should be in Unix epoch/UTC milliseconds.


### Tuning Batches

Batches of data from a single write are packaged for delivery to each destination server as a performance optimization. For Enterprise customers using [MDC], those batches are sent to the remote cluster via realtime sync.

It is possible to specify an approximate largest-batch size for tuning purposes. By default, batches have a soft cap size of 1MB of data, which we've found to be a reasonable size to avoid network congestion.

If you want to adjust that value, the configuration parameter `timeseries_max_batch_size` under `riak_kv` in [advanced.config][config reference] can be defined. The value is in bytes.


## Adding Data via SQL

This can be done either through the [query interface][querying] or via [riak shell][riakshell]. Basic SQL `INSERT` functionality is available, but more complicated things such as `INSERT...SELECT` or subqueries are not.

Here are a couple examples of adding rows from SQL:

```sql
INSERT INTO GeoCheckin (region, state, time, weather, temperature) VALUES ('South Atlantic','South Carolina',1420113600000,'snow',25.2);
```

or

```sql
INSERT INTO GeoCheckin VALUES ('South Atlantic','South Carolina',1420113700000,'rain',37.8);
```

As with standard SQL, if all of the field names are not provided before the `VALUES` keyword, the other fields are assumed to be null.

The fields can be in any order, but the field name and the values must match up. Without the `VALUES` keyword, all fields must be present in the same order as the schema definition.

The data types are validated on the server just like the client PUT commands above.


## Deleting Data

Below are examples of how to delete data by key in each of our Riak TS-supported clients:


```java
final List<Cell> keyCells = Arrays.asList(
  new Cell("South Atlantic"), new Cell("South Carolina"), Cell.newTimestamp(1420113600000));

Delete delete = new Delete.Builder("GeoCheckins", keyCells).build();

client.execute(delete);
```

```ruby
delete_operation = Riak::TimeSeries::Deletion.new client, 'GeoCheckins'
delete_operation.key = ['South Atlantic', 'South Carolina', 1420113600000]
delete_operation.delete!
```

```python
client.ts_delete('GeoCheckin', ['South Atlantic', 'South Carolina', datetime.datetime(2015, 1, 1, 12, 0, 0)])
```

```csharp
var keyCells = new Cell[]
{
    new Cell<string>("Pacific"),
    new Cell<string>("Washington"),
    new Cell<DateTime>(FifteenMinsAgo)
};
var keyToDelete = new Row(keyCells);

var delete = new Delete.Builder()
        .WithTable(Table)
        .WithKey(keyToDelete)
        .Build();
RiakResult rslt = client.Execute(delete);
// rslt.IsSuccess will be true
```

```javascript
var key = [ 'South Atlantic', 'South Carolina', 1420113600000 ];

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

```erlang
riakc_ts:delete(Pid, <<"GeoCheckins">>, [<<"South Atlantic">>, <<"South Carolina">>, 1420113600000]).
```

```php
$key = [
    (new Cell("region"))->setValue("South Atlantic"),
    (new Cell("state"))->setValue("South Carolina"),
    (new Cell("time"))->setTimestampValue(1420113600),
];

$response = (new Command\Builder\TimeSeries\DeleteRow($riak))
    ->atKey($key)
    ->inTable('GeoCheckins')
    ->build()
    ->execute();
```

```golang
key := make([]riak.TsCell, 3)

key[0] = NewStringTsCell("South Atlantic")
key[1] = NewStringTsCell("South Carolina")
key[2] = NewTimestampTsCell(1420113600)

cmd, err := riak.NewTsDeleteRowCommandBuilder()
    .WithTable("GeoCheckin").WithKey(key)
    .Build()

if err != nil {
    return err
}

err = cluster.Execute(cmd)
```

## Next Steps

Now that you've written data to your tables, you can [query][querying] your data.