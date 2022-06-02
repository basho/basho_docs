---
title: "Deleting Data in Riak TS"
description: "Methods for deleting data in Riak TS."
menu:
  riak_ts-1.5.0:
    name: "Delete Data"
    identifier: "deleting_data_riakts"
    weight: 304
    parent: "using"
project: "riak_ts"
project_version: "1.5.0"
toc: true
version_history:
  in: "1.5.0+"
aliases:
    - /riakts/1.5.0/using/deleting-data/
---

[delete]: {{<baseurl>}}riak/ts/1.5.0/using/querying/delete
[expiry]: {{<baseurl>}}riak/ts/1.5.0/configuring/global-object-expiration

Riak TS offers several ways to delete data: with clients, using the DELETE statement, and through global expiry. Global expiry is more efficient than other delete options but operates on all of your data. `DELETE` works per-row but takes more resources to run.

## With Clients

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
    new Cell<string>("South Atlantic"),
    new Cell<string>("South Carolina"),
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

## With DELETE statement

Delete a single record by key with the DELETE statement:

```sql
DELETE FROM GeoCheckin WHERE id ='myid' AND time = '2016-08-01 14:05:51.425Z';
```

See the [`DELETE` in Riak TS][delete] page for more information on the DELETE statement.

## With Global Expiry

Riak TS lets you configure global object expiration (expiry) for your data by adding `leveldb.expiration` and related settings to your riak.conf file. The following example enables expiry and sets objects to be removed after 1 day:

```riak.conf
leveldb.expiration = on
leveldb.expiration.retention_time = 1d
leveldb.expiration.mode = whole_file
```

See the [Configure Global Object Expiration][expiry] page for more information.
