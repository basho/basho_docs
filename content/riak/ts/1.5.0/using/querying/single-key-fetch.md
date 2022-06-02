---
title: "Single Key Fetch in Riak TS"
description: "Using a single key fetch command in Riak TS"
menu:
  riak_ts-1.5.0:
    name: "Single Key Fetch"
    identifier: "singlekey_riakts"
    weight: 600
    parent: "querying_data_riakts"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/using/querying/single-key-fetch
---

You may find the need to fetch a single key from Riak TS. The below examples show you how to perform a single key fetch in each of our official clients that support Riak TS.

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
riakc_ts:get(Pid, <<"GeoCheckin">>, [<<"South Atlantic">>, <<"South Carolina">>, 1420113600000], []).
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