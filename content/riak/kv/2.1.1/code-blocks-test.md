---
title: "Code Blocks Test"
description: ""
project: "riak_kv"
project_version: "2.1.1"
lastmod: 2015-05-05T00:00:00-00:00
sitemap:
  priority: 0.1
toc: true
---

## All Languages

``` java
// In the Java client, it is best to specify a bucket type/bucket/key
// Location object that can be used as a reference for further
// operations, as in the example below:
Location myKey = new Location(new Namespace("animals", "dogs"), "rufus");
```

``` ruby
bucket = client.bucket_type('animals').bucket('dogs')
obj = bucket.get('rufus')
```

``` php
$response = (new \Basho\Riak\Command\Builder\FetchObject($riak))
  ->buildLocation('rufus', 'users', 'animals')
  ->build()
  ->execute();
```

``` python
bucket = client.bucket_type('animals').bucket('dogs')
obj = bucket.get('rufus')
```

``` csharp
// Using the Riak .NET Client it is best to specify a bucket type/bucket/key
// RiakObjectId object that can be used as a reference for further
// operations
var id = new RiakObjectId("animals", "dogs", "rufus");
```

``` javascript
client.fetchValue({ bucketType: 'animals', bucket: 'dogs', key: 'rufus' }, function (err, rslt) {
    assert(rslt.isNotFound);
});
```

``` erlang
{ok, Obj} = riakc_pb_socket:get(Pid,
                            {<<"animals">>, <<"dogs">>},
                            <<"rufus">>).
```

``` golang
cmd, err = riak.NewFetchValueCommandBuilder().
  WithBucketType("animals").
  WithBucket("dogs").
  WithKey("rufus").
  Build()
if err != nil {
    // error occurred
}
```

``` curl
curl http://localhost:8098/types/animals/buckets/dogs/keys/rufus
```

## All Languages 2

``` java
// In the Java client, it is best to specify a bucket type/bucket/key
// Location object that can be used as a reference for further
// operations, as in the example below:
Location myKey = new Location(new Namespace("animals", "dogs"), "rufus");
```

``` ruby
bucket = client.bucket_type('animals').bucket('dogs')
obj = bucket.get('rufus')
```

``` php
$response = (new \Basho\Riak\Command\Builder\FetchObject($riak))
  ->buildLocation('rufus', 'users', 'animals')
  ->build()
  ->execute();
```

``` python
bucket = client.bucket_type('animals').bucket('dogs')
obj = bucket.get('rufus')
```

``` csharp
// Using the Riak .NET Client it is best to specify a bucket type/bucket/key
// RiakObjectId object that can be used as a reference for further
// operations
var id = new RiakObjectId("animals", "dogs", "rufus");
```

``` javascript
client.fetchValue({ bucketType: 'animals', bucket: 'dogs', key: 'rufus' }, function (err, rslt) {
    assert(rslt.isNotFound);
});
```

``` erlang
{ok, Obj} = riakc_pb_socket:get(Pid,
                            {<<"animals">>, <<"dogs">>},
                            <<"rufus">>).
```

``` golang
cmd, err = riak.NewFetchValueCommandBuilder().
  WithBucketType("animals").
  WithBucket("dogs").
  WithKey("rufus").
  Build()
if err != nil {
    // error occurred
}
```

``` curl
curl http://localhost:8098/types/animals/buckets/dogs/keys/rufus
```

## Misc

``` java
// In the Java client, it is best to specify a bucket type/bucket/key
// Location object that can be used as a reference for further
// operations, as in the example below:
Location myKey = new Location(new Namespace("animals", "dogs"), "rufus");
```

Lorem markdownum Byblida. Modo **etiam** litora mittat vellera infelix caeli.
Studiosius forte, potuit pectore. Puer undas dignior iam turpe sorores abesse.
Deae Saturnia levius viribus membra.

``` riakconf
buckets.default.last_write_wins = true
buckets.default.r = 3
```

``` appconfig
{default_bucket_props, [
    {last_write_wins,true},
    {r,3},
    ...
    ]}
```

Puer undas dignior iam turpe sorores abesse. Deae Saturnia levius viribus membra.

``` vmargs
+sfwi 500
+scl false
```

Lorem markdownum Byblida.

``` advancedconfig
{riak_cs, [
    {parameter1, value},
    {parameter2, value},
    %% and so on...
]},
```

Modo **etiam** litora mittat vellera infelix caeli.

``` riakcsconf
stanchion_host = 127.0.0.1:8085
stanchion_ssl = on
```

Summis rediit pavidus tersere et at prosiluit natus Phaethon noxa. Singultibus
oblita **foedabis** orsa.

``` bash
data-platform-admin join --help
```

Singultibus oblita **foedabis** orsa.

```
code test
```
