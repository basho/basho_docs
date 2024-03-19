---
title: "Writing Data to Riak TS"
description: "Writing Data to Riak TS"
menu:
  riak_ts-3.0.0:
    name: "Write Data"
    identifier: "writing_data_riakts"
    weight: 303
    parent: "using"
project: "riak_ts"
project_version: "3.0.0"
lastmod: 2022-09-20T00:00:00-00:00
sitemap:
  priority: 0.9
toc: true
aliases:
  - /riakts/3.0.0/using/writingdata/

---

[activating]: ../creating-activating/
[planning]: ../planning/
[querying]: ../querying/
[http]: {{<baseurl>}}riak/ts/3.0.0/developing/http/
[config reference]: {{<baseurl>}}riak/kv/2.2.0/configuring/reference/#the-advanced-config-file
[MDC]: {{<baseurl>}}riak/ts/3.0.0/configuring/mdc
[riakshell]: ../riakshell
[iso8601]: ../timerepresentations/
[ISO 8601]: https://en.wikipedia.org/wiki/ISO_8601
[iso8601 accuracy]: ../timerepresentations/#reduced-accuracy
[Enterprise]: http://basho.com/products/riak-ts/

Now that you've [planned][planning] and [activated][activating] your Riak TS table, you are ready to write data to it.

## Writing Data

Riak TS allows you to write multiple rows of data at a time. To demonstrate, we'll use the example table from earlier:

```sql
CREATE TABLE GeoCheckin
(
   id           SINT64    NOT NULL,
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (id, QUANTUM(time, 15, 'm')),
      id, time
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
    RiakClient client = RiakClient.newClient(8087, "myriakdb.host");
    List<Row> rows = Arrays.asList(
      new Row(
      	new Cell(1),
        new Cell("South Atlantic"),
        new Cell("Florida"),
        Cell.newTimestamp(1451606401),
        new Cell("hot"),
        new Cell(23.5)
      ),
      new Row(
      	new Cell(2),
        new Cell("East North Central"),
        new Cell("Illinois"),
        Cell.newTimestamp(1451606402),
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
require 'riak'

client = Riak::Client.new(:nodes => [
  {:host => 'myriakdb.host', :pb_port => 8087},
])
submission = Riak::TimeSeries::Submission.new client, "GeoCheckin"
submission.measurements = [[1, "South Atlantic", "Florida", 1451606401, "hot", 23.5], [2, "East North Central", "Illinois", 1451606402, "windy", 19.8]]
submission.write!
```

```python
import datetime
from riak.client import RiakClient

# NB: modify 'host' and 'pb_port' to match your installation
client = RiakClient(host='myriakdb.host', pb_port=8087)

table = client.table('GeoCheckin')
rows = [
    [1, 'South Atlantic', 'Florida', 1451606401, 'hot', 23.5],
    [2, 'East North Central', 'Illinois', 1451606402, 'windy', 19.8]
]
ts_obj = table.new(rows)
print "Store result:", ts_obj.store()
```

```csharp
var cells0 = new Cell[]
{
    new Cell<string>("South Atlantic"),
    new Cell<string>("South Carolina"),
    new Cell<DateTime>(1234567),
    new Cell<string>("hot"),
    new Cell<double>(23.5)
};

var cells1 = new Cell[]
{
    new Cell<string>("Mid-Atlantic"),
    new Cell<string>("New York"),
    new Cell<DateTime>(1234567),
    new Cell<string>("windy"),
    new Cell<double>(19.8)
};

var rows = new Row[]
{
    new Row(cells0),
    new Row(cells1)
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

var hosts = [ 'myriakdb.host:8087' ];
var client = new Riak.Client(hosts);

var columns = [
    { name: 'id',     type: Riak.Commands.TS.ColumnType.Int64 },
    { name: 'region',     type: Riak.Commands.TS.ColumnType.Varchar },
    { name: 'state',        type: Riak.Commands.TS.ColumnType.Varchar },
    { name: 'time',        type: Riak.Commands.TS.ColumnType.Timestamp },
    { name: 'weather',     type: Riak.Commands.TS.ColumnType.Varchar },
    { name: 'temperature', type: Riak.Commands.TS.ColumnType.Double }
];

var rows = [
    [ 1, 'South Atlantic', 'Florida', 1451606401, 'hot', 23.5 ],
    [ 2, 'East North Central', 'Illinois', 1451606402, 'windy', 19.8 ]
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
{ok, Pid} = riakc_pb_socket:start_link("myriakdb.host", 8087).
riakc_ts:put(Pid, "GeoCheckin", [{1, <<"South Atlantic">>, <<"Florida">>, 1451606401, <<"hot">>, 23.5}, {2, <<"East North Central">>, <<"Illinois">>, 1451606402, <<"windy">>, 19.8}]).
```

```php

require __DIR__ . '/../vendor/autoload.php';

use Basho/Riak;
use Basho/Riak/Command;
use Basho/Riak/Node;

$node = (new Node/Builder)
    ->atHost('myriakdb.host')
    ->onPort(8087)
    ->build();

$riak = new Riak([$node], [], new Riak/Api/Pb());

$response = (new Command/Builder/TimeSeries/StoreRows($riak))
    ->inTable('GeoCheckins')
    ->withRow([
        (new Cell("region"))->setValue("South Atlantic"),
        (new Cell("state"))->setValue("South Carolina"),
        (new Cell("time"))->setTimestampValue(1234567),
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

{{% note title="Note on validation" %}}
Riak TS validates all rows on the server side before writing occurs, checking the number of row elements and types. If any of the rows fails validation then none of the rows will be written.  An error message will be returned with the index numbers of the invalid rows in the batch, the first item in the batch being index one.
{{% /note %}}

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
RiakClient client = RiakClient.newClient(8087, "myriakdb.host");
List<Row> someRows = Arrays.asList(
        // Good Row
        new Row(new Cell(1), new Cell("South Atlantic"), new Cell("Florida"), Cell.newTimestamp(1451606401), new Cell("hot"), new Cell(23.5)),
        // Bad Rows
        new Row(new Cell("South Atlantic"), Cell.newTimestamp(fiveMinsAgo)), // too short
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
* Timestamps should be in Unix epoch/UTC milliseconds or [ISO 8601 format](#iso-8601).

### Tuning Batches

Batches of data from a single write are packaged for delivery to each destination server as a performance optimization. For [Enterprise] customers using [MDC], those batches are sent to the remote cluster via realtime sync.

It is possible to specify an approximate largest-batch size for tuning purposes. By default, batches have a soft cap size of 1MB of data, which we've found to be a reasonable size to avoid network congestion.

If you want to adjust that value, the configuration parameter `timeseries_max_batch_size` under `riak_kv` in [advanced.config][config reference] can be defined. The value is in bytes.

## Adding Data via SQL

You can add data via SQL statements either through the [query interface][querying] or via [riak shell][riakshell]. Basic SQL `INSERT` functionality is available, but more complicated things such as `INSERT...SELECT` or subqueries are not.

{{% note title="INSERT limitations" %}}
Writing data via an SQL INSERT statement (as demonstrated below) has been found to be 3x slower than using one of our supported clients or the riak shell to insert data under a normal workload (10 bytes per column, up to ~ 50 columns). In these cases, we strongly recommend that you only `INSERT` small data updates and do not use it in a production environment.

Larger workloads should only use a supported client to insert data.
{{% /note %}}

Here are a couple examples of adding rows from SQL:

```sql
INSERT INTO GeoCheckin (id, region, state, time, weather, temperature) VALUES (1, 'South Atlantic', 'Florida', 1451606401, 'hot', 23.5);
```

or

```sql
INSERT INTO GeoCheckin VALUES (1, 'South Atlantic', 'Florida', 1451606401, 'hot', 23.5);
```

As with standard SQL, if all of the field names are not provided before the `VALUES` keyword, the other fields are assumed to be null.

The fields can be in any order, but the field name and the values must match up. Without the `VALUES` keyword, all fields must be present in the same order as the schema definition.

The data types are validated on the server just like the client PUT commands above.

### ISO 8601

It is possible to use [ISO 8601]-compliant date/time strings in INSERT statements instead of integer timestamps:
`
```sql
INSERT INTO GeoCheckin VALUES ('South Atlantic','South Carolina','2015-01-01 12:01:40Z','rain',37.8);
```

You must include single quotes around the ISO 8601 value. You cannot use [reduced accuracy time representations][iso8601 accuracy]. In other words, you must specify your time down to the second (or use fractional times).

See [our documentation on ISO 8601 support][iso8601] for more details on how to use ISO 8601.

### Blob data

If using a client library (or the underlying protocol buffers API), simply write binary data into blob columns.

Other interfaces, such as SQL and HTTP, do not allow raw binary data to be written as such.

When using SQL INSERT to add binary data to blob columns, use base 16 (hex) notation. An example using `riak-shell`:

```
riak-shell>INSERT INTO GeoCheckin VALUES ('SC', '2017-01-01T15:00:00', 'sunny', 43.2, 0x3af6240c1000035dbc), ('SC', '2017-01-01T16:00:00', 'cloudy', 41.5, 0x3af557bc4000042dbc), ('SC', '2017-01-01T17:00:00', 'windy', 33.0, 0x3af002ee10000a2dbc);
```

If using the HTTP API, see the [API docs][http] for information on encoding binary data to use with JSON or SQL data upload.

## Next Steps

Now that you've written data to your tables, you can [query][querying] your data.
