---
title: "Using Secondary Indexes (2i)"
description: ""
project: "riak_kv"
project_version: "2.0.6"
menu:
  riak_kv-2.0.6:
    name: "Using Secondary Indexes"
    identifier: "usage_2i"
    weight: 107
    parent: "developing_usage"
toc: true
aliases:
  - /riak/2.0.6/dev/using/2i
  - /riak/kv/2.0.6/dev/using/2i
---

[plan backend leveldb]: {{<baseurl>}}riak/kv/2.0.6/setup/planning/backend/leveldb
[plan backend memory]: {{<baseurl>}}riak/kv/2.0.6/setup/planning/backend/memory
[use ref strong consistency]: {{<baseurl>}}riak/kv/2.0.6/using/reference/strong-consistency

> **Note: Riak Search preferred for querying**
>
> If you're interested in non-primary-key-based querying in Riak, i.e. if
you're looking to go beyond straightforward K/V operations, we now
recommend [Riak Search]({{<baseurl>}}riak/kv/2.0.6/developing/usage/search/) rather than secondary indexes for
a variety of reasons. Most importantly, Riak Search has a far more
capacious querying API and can be used with all of Riak's storage
backends.

Secondary indexes (2i) in Riak enable you to tag objects stored in Riak,
at write time, with one or more queryable values. Those values can then
be used to find multiple objects in Riak. If you're storing [user data]({{<baseurl>}}riak/kv/2.0.6/developing/data-modeling/#user-accounts), for example, you could tag each object
associated with that user with a username or other unique marker. Once
tagged, you could find all objects in a Riak bucket sharing that tag.
Secondary indexes can be either a binary or string, such as
`sensor_1_data` or `admin_user` or `click_event`, or an integer, such as
`99` or `141121`.

[Riak Search]({{<baseurl>}}riak/kv/2.0.6/developing/usage/search/) serves analogous purposes but is quite
different because it parses key/value data itself and builds indexes on
the basis of Solr schemas.

Please note that 2i can be used only with the [LevelDB][plan backend leveldb] and [Memory][plan backend memory]
backends.

## Features

* Allows two types of secondary attributes: integers and strings (aka
  binaries)
* Allows querying by exact match or range on one index
* Allows pagination of results
* Allows streaming of results
* Query results can be used as input to a [MapReduce]({{<baseurl>}}riak/kv/2.0.6/developing/usage/mapreduce/)
  query

> **Note on 2i and strong consistency**
Secondary indexes do not currently work with the [strong consistency][use ref strong consistency]
feature introduced in Riak version 2.0. If you store objects in
[strongly consistent buckets]({{<baseurl>}}riak/kv/2.0.6/developing/app-guide/strong-consistency/#creating-a-strongly-consistent-bucket-type) and attach
secondary index metadata to those objects, you can still perform
strongly consistent operations on those objects but the secondary
indexes will be ignored.

## When to Use Secondary Indexes

Secondary indexes are useful when you want to find data on the basis of
something other than objects' bucket type, bucket, and key, i.e. when
you want objects to be discoverable based on more than their location
alone.

2i works best for objects whose value is stored in an opaque blob, like
a binary file, because those objects don't offer any clues that enable
you to discover them later. Indexing enables you to tag those objects
and find all objects with the same tag in a specified bucket later on.

2i is thus recommended when your use case requires an easy-to-use search
mechanism that does not require a schema (as does [Riak Search]({{<baseurl>}}riak/kv/2.0.6/using/reference/search/#schemas)) and a basic query interface, i.e. an interface that
enables an application to tell Riak things like "fetch all objects
tagged with the string `Milwaukee_Bucks`" or "fetch all objects tagged
with numbers between 1500 and 1509."

2i is also recommended if your use case requires anti-entropy. Since
secondary indexes are just metadata attached to key/value objects, 2i
piggybacks off of read-repair.

## When Not to Use Secondary Indexes

* If your ring size exceeds 512 partitions, 2i can cause performance
  issues in large clusters.
* When you need more than the exact match and range searches that 2i
  supports. If that's the case, we recommend checking out [Riak Search]({{<baseurl>}}riak/kv/2.0.6/developing/usage/search/).
* When you want to use composite queries. A query like
  `last_name=zezeski AND state=MD` would have to be split into two
  queries and the results merged (or it would need to involve
  [MapReduce]({{<baseurl>}}riak/kv/2.0.6/developing/usage/mapreduce/)).

## Query Interfaces and Examples

Typically, the result set from a 2i query is a list of object keys from
the specified bucket that include the index values in question. As we'll
see below, when executing range queries in Riak 1.4 or higher, it is
possible to retrieve the index values along with the object keys.

### Inserting Objects with Secondary Indexes

In this example, the key `john_smith` is used to store user data in the
bucket `users`, which bears the `default` bucket type. Let's say that an
application would like add a Twitter handle and an email address to this
object as secondary indexes.

```java
Location johnSmithKey = new Location(new Namespace("default", "users"), "john_smith");

// In the Java client (and all clients), if you do not specify a bucket type,
// the client will use the default type. And so the following store command
// would be equivalent to the one above:
Location johnSmithKey = new Location(new Namespace("users"), "john_smith");

RiakObject obj = new RiakObject()
        .setContentType("application/json")
        .setValue(BinaryValue.create("{'user_data':{ ... }}"));

obj.getIndexes().getIndex(StringBinIndex.named("twitter")).add("jsmith123");
obj.getIndexes().getIndex(StringBinIndex.named("email")).add("jsmith@basho.com");

StoreValue store = new StoreValue.Builder(obj)
        .withLocation(johnSmithKey)
        .build();
client.execute(store);
```

```ruby
bucket = client.bucket_type('default').bucket('users')
obj = Riak::RObject.new(bucket, 'john_smith')
obj.content_type = 'application/json'
obj.raw_data = '{"user_data":{ ... }}'

# String/binary indexes must be set as an array of strings
obj.indexes['twitter_bin'] = %w{ jsmith123 }
obj.indexes['email_bin'] = %w{ jsmith@basho.com }
obj.store

# In the Ruby client (and all clients), if you do not specify a bucket
# type, the client will use the default type. And so the following set
# of commands would be equivalent to the one above:

bucket = client.bucket('users')
# repeat the same commands for building the object
obj.store
```

```php
$object = (new \Basho\Riak\Object('{"user_data":{ ... }}', ['Content-type' => 'application/json']))
  ->addValueToIndex('twitter_bin', 'jsmith123')
  ->addValueToIndex('email_bin', 'jsmith@basho.com');

(new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->withObject($object)
  ->buildLocation('john_smith', 'users', 'default')
  ->build()
  ->execute();
```

```python
bucket = client.bucket_type('default').bucket('users')
# In the Python client (and all clients), if you do not specify a bucket type,
# the client will use the default type. And so the following store command
# would be equivalent to the one above:
bucket = client.bucket('users')

obj = RiakObject(client, bucket, 'john_smith')
obj.content_type = 'text/plain'
obj.data = '...user data...'
obj.add_index('twitter_bin', 'jsmith123')
obj.add_index('email_bin', 'jsmith@basho.com')
obj.store()
```

```csharp
var id = new RiakObjectId("default", "users", "john_smith");
var obj = new RiakObject(id, "...user data...",
    RiakConstants.ContentTypes.TextPlain);
obj.BinIndex("twitter").Set("jsmith123");
obj.BinIndex("email").Set"jsmith@basho.com");
var rslt = client.Put(obj);
```

```javascript
var riakObj = new Riak.Commands.KV.RiakObject();
riakObj.setContentType('text/plain');
riakObj.setBucket('users');
riakObj.setKey('john_smith');
riakObj.setValue('...user data...');
riakObj.addToIndex('twitter_bin', 'jsmith123');
riakObj.addToIndex('email_bin', 'jsmith@basho.com');
client.storeValue({ value: riakObj }, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
Obj = riakc_obj:new({<<"default">>, <<"users">>},
                    <<"john_smith">>,
                    <<"...user data...">>,
                    <<"text/plain">>),
%% In the Erlang client (and all clients), if you do not specify a bucket type,
%% the client will use the default type. And so the following object would be
%% equivalent to the one above:

Obj = riakc_obj:new(<<"users">>,
                    <<"john_smith">>,
                    <<"...user data...">>,
                    <<"text/plain">>),
MD1 = riakc_obj:get_update_metadata(Obj),
MD2 = riakc_obj:set_secondary_index(
    MD1,
    [{{binary_index, "twitter"}, [<<"jsmith123">>]},
     {{binary_index, "email"}, [<<"jsmith@basho.com">>]}]),
Obj2 = riakc_obj:update_metadata(Obj, MD2),
riakc_pb_socket:put(Pid, Obj2).
```

```golang
obj := &riak.Object{
    ContentType:     "text/plain",
    Charset:         "utf-8",
    ContentEncoding: "utf-8",
    BucketType:      "indexes",
    Bucket:          "users",
    Key:             "john_smith",
    Value:           []byte("…user data…"),
}

obj.AddToIndex("twitter_bin", "jsmith123")
obj.AddToIndex("email_bin", "jsmith@basho.com")

cmd, err := riak.NewStoreValueCommandBuilder().
    WithContent(obj).
    Build()
if err != nil {
    return err
}

if err := cluster.Execute(cmd); err != nil {
    return err
}
```

```curl
curl -XPOST localhost:8098/types/default/buckets/users/keys/john_smith \
  -H 'x-riak-index-twitter_bin: jsmith123' \
  -H 'x-riak-index-email_bin: jsmith@basho.com' \
  -H 'Content-Type: application/json' \
  -d '{"userData":"data"}'
```

> **Getting started with Riak clients**
>
> If you are connecting to Riak using one of Basho's official [client libraries]({{<baseurl>}}riak/kv/2.0.6/developing/client-libraries), you can find more information about getting started with
your client in the [Developing with Riak KV: Getting Started]({{<baseurl>}}riak/kv/2.0.6/developing/getting-started) section.

This has accomplished the following:

* The object has been stored with a primary bucket/key of
  `users`/`john_smith`
* The object now has a secondary index called `twitter_bin` with a value
  of `jsmith123`
* The object now has a secondary index called `email_bin` with a value
  of `jsmith@basho.com`

### Querying Objects with Secondary Indexes

Let's query the `users` bucket on the basis of Twitter handle to make
sure that we can find our stored object:

```java
Namespace usersBucket = new Namespace("users");
BinIndexQuery biq = new BinIndexQuery.Builder(usersBucket, "twitter", "jsmith123")
        .build();
BinIndexQuery.Response response = client.execute(biq);
List<BinIndexQuery.Response.Entry> entries = response.getEntries();
for (BinIndexQuery.Response.Entry entry : entries) {
    System.out.println(entry.getRiakObjectLocation().getKey());
}
```

```ruby
bucket = client.bucket('users')
bucket.get_index('twitter_bin', 'jsmith123')

# This is equivalent to the following:
bucket = client.bucket_type('default').bucket('users')
bucket.get_index('twitter_bin', 'jsmith123')
```

```php
$response = (new \Basho\Riak\Command\Builder\QueryIndex($riak))
  ->buildBucket('users')
  ->withIndexName('twitter_bin')
  ->withScalarValue('jsmith123')
  ->build()
  ->execute()
  ->getResults();
```

```python
bucket = client.bucket('users') # equivalent to client.bucket_type('default').bucket('users')
bucket.get_index('twitter_bin', 'jsmith123').results
```

```csharp
var idxId = new RiakIndexId("default", "users", "twitter");
var rslt = client.GetSecondaryIndex(idxId, "jsmith123");
var idxRslt = rslt.Value;
foreach (var keyTerm in idxRslt.IndexKeyTerms)
{
    Debug.WriteLine(keyTerm.Key);
}
```

```javascript
var query_keys = [];
function query_cb(err, rslt) {
    if (err) {
        throw new Error(err);
    }

    if (rslt.done) {
        query_keys.forEach(function (key) {
            logger.info("2i query key: '%s'", key);
        });
    }

    if (rslt.values.length > 0) {
        Array.prototype.push.apply(query_keys,
            rslt.values.map(function (value) {
                return value.objectKey;
            }));
    }
}

var cmd = new Riak.Commands.KV.SecondaryIndexQuery.Builder()
    .withBucket('users')
    .withIndexName('twitter_bin')
    .withIndexKey('jsmith123')
    .withCallback(query_cb)
    .build();
client.execute(cmd);
```

```erlang
{ok, Results} =
    riakc_pb_socket:get_index(Pid,
                              <<"users">>, %% bucket
                              {binary_index, "twitter"}, %% index name
                              <<"jsmith123">>). %% index
```

```golang
cmd, err := riak.NewSecondaryIndexQueryCommandBuilder().
    WithBucketType("indexes").
    WithBucket("users").
    WithIndexName("twitter_bin").
    WithIndexKey("jsmith123").
    Build()
if err != nil {
    return err
}

if err := cluster.Execute(cmd); err != nil {
    return err
}
```

```curl
curl localhost:8098/buckets/users/index/twitter_bin/jsmith123
```

The response:

```java
john_smith
```

```ruby
["john_smith"]
```

```php
['john_smith']
```

```python
['john_smith']
```

```csharp
john_smith
```

```javascript
john_smith
```

```erlang
{ok,{index_results_v1,[<<"john_smith">>],
                      undefined,undefined}}.
```

```golang
john_smith
```

```curl
{
  "keys": [
    "john_smith"
  ]
}
```

## Examples

To run the following examples, make sure that Riak is configured to use
an index-capable storage backend, such as [LevelDB][plan backend leveldb] or [Memory][plan backend memory].

## Indexing Objects

The following example indexes four different objects. Notice that we're
storing both integer and string (aka binary) fields. Field names are
automatically lowercased, some fields have multiple values, and
duplicate fields are automatically de-duplicated, as in the following
example:

```java
Namespace peopleBucket = new Namespace("indexes", "people");

RiakObject larry = new RiakObject()
        .setValue(BinaryValue.create("My name is Larry"));
larry.getIndexes().getIndex(StringBinIndex.named("field1")).add("val1");
larry.getIndexes().getIndex(LongIntIndex.named("field2")).add(1001L);
StoreValue storeLarry = new StoreValue.Builder(larry)
        .withLocation(peopleBucket.setKey("larry"))
        .build();
client.execute(storeLarry);

RiakObject moe = new RiakObject()
        .setValue(BinaryValue.create("Ny name is Moe"));
moe.getIndexes().getIndex(StringBinIdex.named("Field1")).add("val2");
moe.getIndexes().getIndex(LongIntIndex.named("Field2")).add(1002L);
StoreValue storeMoe = new StoreValue.Builder(moe)
        .withLocation(peopleBucket.setKey("moe"))
        .build();
client.execute(storeMoe);

RiakObject curly = new RiakObject()
        .setValue(BinaryValue.create("My name is Curly"));
curly.getIndexes().getIndex(StringBinIndex.named("FIELD1")).add("val3");
curly.getIndexes().getIndex(LongIntIndex.named("FIELD2")).add(1003L);
StoreValue storeCurly = new StoreValue.Builder(curly)
        .withLocation(peopleBucket.setKey("curly"))
        .build();
client.execute(storeCurly);

RiakObject veronica = new RiakObject()
        .setValue(BinaryValue.create("My name is Veronica"));
veronica.getIndexes().getIndex(StringBinIndex.named("field1"))
        .add("val4").add("val4");
veronica.getIndexes().getIndex(LongIntIndex.named("field2"))
        .add(1004L).add(1005L).add(1006L).add(1004L).add(1004L).add(1007L);
StoreValue storeVeronica = new StoreValue.Builder(veronica)
        .withLocation(peopleBucket.setKey("veronica"))
        .build();
client.execute(storeVeronica);
```

```ruby
bucket = client.bucket_type('indexes').bucket('people')

obj1 = Riak::RObject.new(bucket, 'larry')
obj1.content_type = 'text/plain'
obj1.raw_data = 'My name is Larry'
obj1.indexes['field1_bin'] = %w{ val1 }
# Like binary/string indexes, integer indexes must be set as an array,
# even if you wish to add only a single index
obj1.indexes['field2_int'] = [1001]
obj1.store

obj2 = Riak::RObject.new(bucket, 'moe')
obj2.content_type = 'text/plain'
obj2.raw_data = 'My name is Larry'
obj2.indexes['Field1_bin'] = %w{ val2 }
obj2.indexes['Field2_int'] = [1002]
obj2.store

obj3 = Riak::RObject.new(bucket, 'curly')
obj3.content_type = 'text/plain'
obj3.raw_data = 'My name is Curly'
obj3.indexes['FIELD1_BIN'] = %w{ val3 }
obj3.indexes['FIELD2_INT'] = [1003]
obj3.store

obj4 = Riak::RObject.new(bucket, 'veronica')
obj4.content_type = 'text/plain'
obj4.raw_data = 'My name is Veronica'
obj4.indexes['field1_bin'] = %w{ val4 val4 val4a val4b }
obj4.indexes['field2_int'] = [1004, 1004, 1005, 1006]
obj4.indexes['field2_int'] = [1004]
obj4.indexes['field2_int'] = [1004]
obj4.indexes['field2_int'] = [1004]
obj4.indexes['field2_int'] = [1007]
obj4.store
```

```php
$bucket = new \Basho\Riak\Bucket('people', 'indexes');

$object = (new \Basho\Riak\Object'My name is Larry', ['Content-type' => 'text/plain']))
  ->addValueToIndex('field1_bin', 'val1')
  ->addValueToIndex('field2_int', 1001);

(new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->withObject($object)
  ->withLocation(new \Basho\Riak\Location('larry', $bucket))
  ->build()
  ->execute();

$object = (new \Basho\Riak\Object'My name is Moe', ['Content-type' => 'text/plain']))
  ->addValueToIndex('Field1_bin', 'val2')
  ->addValueToIndex('Field2_int', 1002);

(new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->withObject($object)
  ->withLocation(new \Basho\Riak\Location('moe', $bucket))
  ->build()
  ->execute();

$object = (new \Basho\Riak\Object'My name is Curly', ['Content-type' => 'text/plain']))
  ->addValueToIndex('FIELD1_BIN', 'val3')
  ->addValueToIndex('FIELD2_int', 1003);

(new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->withObject($object)
  ->withLocation(new \Basho\Riak\Location('curly', $bucket))
  ->build()
  ->execute();

$object = (new \Basho\Riak\Object'My name is Veronica', ['Content-type' => 'text/plain']))
  ->addValueToIndex('field1_bin', 'val4')
  ->addValueToIndex('field1_bin', 'val4')
  ->addValueToIndex('field1_bin', 'val4a')
  ->addValueToIndex('field1_bin', 'val4b')
  ->addValueToIndex('field2_int', 1004)
  ->addValueToIndex('field2_int', 1005)
  ->addValueToIndex('field2_int', 1006)
  ->addValueToIndex('field2_int', 1004)
  ->addValueToIndex('field2_int', 1004)
  ->addValueToIndex('field2_int', 1007);

(new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->withObject($object)
  ->withLocation(new \Basho\Riak\Location('veronica', $bucket))
  ->build()
  ->execute();
```

```python
bucket = client.bucket_type('indexes').bucket('people')

obj1 = RiakObject(client, bucket, 'larry')
obj1.content_type = 'text/plain'
obj1.data = 'My name is Larry'
obj1.add_index('field1_bin', 'val1').add_index('field2_int', 1001)
obj1.store()

obj2 = RiakObject(client, bucket, 'moe')
obj2.content_type = 'text/plain'
obj2data = 'Moe'
obj2.add_index('Field1_bin', 'val2').add_index('Field2_int', 1002)
obj2.store()

obj3 = RiakObject(client, bucket, 'curly')
obj3.content_type = 'text/plain'
obj3.data = 'Curly'
obj3.add_index('FIELD1_BIN', 'val3').add_index('FIELD2_INT', 1003)
obj3.store()

obj4 = RiakObject(client, bucket, 'veronica')
obj4.content_type = 'text/plain'
obj4.data = 'Veronica'
obj4.add_index('field1_bin', 'val4').add_index('field1_bin', 'val4a').add_index('field1_bin', 'val4b').add_index('field2_int', 1004).add_index('field2_int', 1004).add_index('field2_int', 1005).add_index('field2_int', 1006).add_index('field2_int', 1004).add_index('field2_int', 1004).add_index('field2_int', 1004).add_index('field2_int', 1007)
obj4.store()
```

```csharp
var larryId = new RiakObjectId("indexes", "people", "larry");
var larry = new RiakObject(larryId, "My name is Larry",
    RiakConstants.ContentTypes.TextPlain);

larry.BinIndex("field1").Set("val1");
larry.IntIndex("field2").Set(1001);

client.Put(larry);

var moeId = new RiakObjectId("indexes", "people", "moe");
var moe = new RiakObject(moeId, "My name is Moe",
    RiakConstants.ContentTypes.TextPlain);

moe.BinIndex("Field1").Set("val2");
moe.IntIndex("Field2").Set(1002);

client.Put(moe);

var curlyId = new RiakObjectId("indexes", "people", "curly");
var curly = new RiakObject(curlyId, "My name is Curly",
    RiakConstants.ContentTypes.TextPlain);

curly.BinIndex("FIELD1").Set("val3");
curly.IntIndex("FIELD2").Set(1003);

client.Put(curly);

var veronicaId = new RiakObjectId("indexes", "people", "veronica");
var veronica = new RiakObject(veronicaId, "My name is Veronica",
    RiakConstants.ContentTypes.TextPlain);

veronica.BinIndex("FIELD1").Set(new string[] { "val4", "val4a", "val4b" });
veronica.IntIndex("FIELD2").Set(new BigInteger[] {
    1004, 1005, 1006, 1004, 1004, 1007
});

client.Put(veronica);
```

```javascript
function store_cb(err, rslt, async_cb) {
    if (err) {
        throw new Error(err);
    }
    async_cb(null, rslt);
}

var storeFuncs = [
    function (async_cb) {
        var riakObj = new Riak.Commands.KV.RiakObject();
        riakObj.setContentType('text/plain');
        riakObj.setBucketType('indexes');
        riakObj.setBucket('people');
        riakObj.setKey('larry');
        riakObj.setValue('My name is Larry');
        riakObj.addToIndex('field1_bin', 'val1');
        riakObj.addToIndex('field2_int', 1001);
        client.storeValue({ value: riakObj }, function (err, rslt) {
            store_cb(err, rslt, async_cb);
        });
    },
    function (async_cb) {
        var riakObj = new Riak.Commands.KV.RiakObject();
        riakObj.setContentType('text/plain');
        riakObj.setBucketType('indexes');
        riakObj.setBucket('people');
        riakObj.setKey('moe');
        riakObj.setValue('My name is Moe');
        riakObj.addToIndex('Field1_bin', 'val2');
        riakObj.addToIndex('Field2_int', 1002);
        client.storeValue({ value: riakObj }, function (err, rslt) {
            store_cb(err, rslt, async_cb);
        });
    },
    function (async_cb) {
        var riakObj = new Riak.Commands.KV.RiakObject();
        riakObj.setContentType('text/plain');
        riakObj.setBucketType('indexes');
        riakObj.setBucket('people');
        riakObj.setKey('curly');
        riakObj.setValue('My name is Curly');
        riakObj.addToIndex('FIELD1_BIN', 'val3');
        riakObj.addToIndex('FIELD2_INT', 1003);
        client.storeValue({ value: riakObj }, function (err, rslt) {
            store_cb(err, rslt, async_cb);
        });
    },
    function (async_cb) {
        var riakObj = new Riak.Commands.KV.RiakObject();
        riakObj.setContentType('text/plain');
        riakObj.setBucketType('indexes');
        riakObj.setBucket('people');
        riakObj.setKey('veronica');
        riakObj.setValue('My name is Veronica');
        riakObj.addToIndex('FIELD1_bin', 'val4');
        riakObj.addToIndex('FIELD1_bin', 'val4');
        riakObj.addToIndex('FIELD1_bin', 'val4a');
        riakObj.addToIndex('FIELD1_bin', 'val4b');
        riakObj.addToIndex('FIELD2_int', 1004);
        riakObj.addToIndex('FIELD2_int', 1005);
        riakObj.addToIndex('FIELD2_int', 1006);
        riakObj.addToIndex('FIELD2_int', 1004);
        riakObj.addToIndex('FIELD2_int', 1004);
        riakObj.addToIndex('FIELD2_int', 1007);
        client.storeValue({ value: riakObj }, function (err, rslt) {
            store_cb(err, rslt, async_cb);
        });
    }
];
async.parallel(storeFuncs, function (err, rslts) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
Larry = riakc_obj:new(
    {<<"indexes">>, <<"people">>},
    <<"larry">>,
    <<"My name is Larry">>,
    <<"text/plain">>),
LarryMetadata = riakc_obj:get_update_metadata(Larry),
LarryIndexes = riakc_obj:set_secondary_index(
    LarryMetadata,
    [{{binary_index, "field1"}, [<<"val1">>]}, {{integer_index, "field2"}, [1001]}]
),
LarryWithIndexes = riakc_obj:update_metadata(Larry, LarryIndexes).

Moe = riakc_obj:new(
    {<<"indexes">>, <<"people">>},
    <<"moe">>,
    <<"My name is Moe">>,
    <<"text/plain">>),
MoeMetadata = riakc_obj:get_update_metadata(Moe),
MoeIndexes = riakc_obj:set_secondary_index(
    MoeMetadata,
    [{{binary_index, "Field1"}, [<<"val2">>]}, {{integer_index, "Field2"}, [1002]}]
),
MoeWithIndexes = riakc_obj:update_metadata(Moe, MoeIndexes).

Curly = riakc_obj:new(
    {<<"indexes">>, <<"people">>},
    <<"curly">>,
    <<"My name is Curly">>,
    <<"text/plain">>),
CurlyMetadata = riakc_obj:get_update_metadata(Curly),
CurlyIndexes = riakc_obj:set_secondary_index(
    CurlyMetadata,
    [{{binary_index, "FIELD1"}, [<<"val3">>]}, {{integer_index, "FIELD2"}, [1003]}]
),
CurlyWithIndexes = riakc_obj:update_metadata(Curly, CurlyIndexes).

Veronica = riakc_obj:new(
    {<<"indexes">>, <<"people">>},
    <<"veronica">>,
    <<"My name is Veronica">>,
    <<"text/plain">>),
VeronicaMetadata = riakc_obj:get_update_metadata(Veronica),
VeronicaIndexes = riakc_obj:set_secondary_index(
    VeronicaMetadata,
    [{{binary_index, "field1"}, [<<"val4">>]}, {{binary_index, "field1"}, [<<"val4">>]}, {{integer_index, "field2"}, [1004]}, {{integer_index, "field2"}, [1004]}, {{integer_index, "field2"}, [1005]}, {{integer_index, "field2"}, [1006]}, {{integer_index, "field2"}, [1004]}, {{integer_index, "field2"}, [1004]}, {{integer_index, "field2"}, [1007]}]
),
VeronicaWithIndexes = riakc_obj:update_metadata(Veronica, VeronicaIndexes).
```

```golang
o1 := &riak.Object{
    Key:   "larry",
    Value: []byte("My name is Larry"),
}
o1.AddToIndex("field1_bin", "val1")
o1.AddToIntIndex("field2_int", 1001)

o2 := &riak.Object{
    Key:   "moe",
    Value: []byte("My name is Moe"),
}
o2.AddToIndex("Field1_bin", "val2")
o2.AddToIntIndex("Field2_int", 1002)

o3 := &riak.Object{
    Key:   "curly",
    Value: []byte("My name is Curly"),
}
o3.AddToIndex("FIELD1_BIN", "val3")
o3.AddToIntIndex("FIELD2_INT", 1003)

o4 := &riak.Object{
    Key:   "veronica",
    Value: []byte("My name is Veronica"),
}
o4.AddToIndex("FIELD1_bin", "val4")
o4.AddToIndex("FIELD1_bin", "val4")
o4.AddToIndex("FIELD1_bin", "val4a")
o4.AddToIndex("FIELD1_bin", "val4b")
o4.AddToIntIndex("FIELD2_int", 1004)
o4.AddToIntIndex("FIELD2_int", 1005)
o4.AddToIntIndex("FIELD2_int", 1006)
o4.AddToIntIndex("FIELD2_int", 1004)
o4.AddToIntIndex("FIELD2_int", 1004)
o4.AddToIntIndex("FIELD2_int", 1007)

objs := [...]*riak.Object{o1, o2, o3, o4}

wg := &sync.WaitGroup{}
for _, obj := range objs {
    obj.ContentType = "text/plain"
    obj.Charset = "utf-8"
    obj.ContentEncoding = "utf-8"

    cmd, err := riak.NewStoreValueCommandBuilder().
        WithBucketType("indexes").
        WithBucket("people").
        WithContent(obj).
        Build()
    if err != nil {
        return err
    }

    args := &riak.Async{
        Command: cmd,
        Wait:    wg,
    }
    if err := cluster.ExecuteAsync(args); err != nil {
        return err
    }
}

wg.Wait()
```

```curl
curl -v -XPUT localhost:8098/types/indexes/buckets/people/keys/larry \
  -H "x-riak-index-field1_bin: val1" \
  -H "x-riak-index-field2_int: 1001" \
  -d 'My name is Larry'

curl -v -XPUT localhost:8098/types/indexes/buckets/people/keys/moe \
  -H "x-riak-index-Field1_bin: val2" \
  -H "x-riak-index-Field2_int: 1002" \
  -d 'My name is Moe'

curl -v -XPUT localhost:8098/types/indexes/buckets/people/keys/curly \
  -H "X-RIAK-INDEX-FIELD1_BIN: val3" \
  -H "X-RIAK-INDEX-FIELD2_INT: 1003" \
  -d 'My name is Curly'

curl -v -XPUT 127.0.0.1:8098/types/indexes/buckets/people/keys/veronica \
  -H "x-riak-index-field1_bin: val4, val4, val4a, val4b" \
  -H "x-riak-index-field2_int: 1004, 1004, 1005, 1006" \
  -H "x-riak-index-field2_int: 1004" \
  -H "x-riak-index-field2_int: 1004" \
  -H "x-riak-index-field2_int: 1004" \
  -H "x-riak-index-field2_int: 1007" \
  -d 'My name is Veronica'
```

The above objects will end up having the following secondary indexes,
respectively:

* `Larry` --- Binary index `field1_bin` and integer index `field2_int`
* `Moe` --- Binary index `field1_bin` and integer index `field2_int`
  (note that the index names are set to lowercase by Riak)
* `Curly` --- Binary index `field1_bin` and integer index `field2_int`
  (note again that the index names are set to lowercase)
* `Veronica` --- Binary index `field1_bin` with the values `val4`,
  `val4a`, and `val4b` and integer index `field2_int` with the values
  `1004`, `1005`, `1006`, and `1007` (note that redundancies have been removed)

As these examples show, there are safeguards in Riak that both normalize
the names of indexes and prevent the accumulation of redundant indexes.

## Invalid Field Names and Types

The following examples demonstrate what happens when an index field is
specified with an invalid field name or type. The system responds with
`400 Bad Request` and a description of the error.

Invalid field name:

```java
// The Java client will not allow you to provide invalid index names,
// because you are not required to add "_bin" or "_int" to the end of
// those names
```

```ruby
bucket = client.bucket_type('indexes').bucket('people')
obj = Riak::RObject.new(bucket, 'larry')
obj.indexes['field2_foo'] = [1001]

# The Ruby client will let you get away with this...at first. But when
# you attempt to store the object, you will get an error response such
# as this:

NoMethodError: undefined method 'map' for 1001:Fixnum
```

```php
// throws \InvalidArgumentException
$object = (new \Basho\Riak\Object('{"user_data":{ ... }}', ['Content-type' => 'application/json']))
  ->addValueToIndex('twitter', 'jsmith123');
```

```python
bucket = client.bucket_type('indexes').bucket('people')
obj = RiakObject(client, bucket, 'larry')
obj.add_index('field2_foo', 1001)

# Result:
riak.RiakError: "Riak 2i fields must end with either '_bin' or '_int'."
```

```csharp
// The Riak .NET Client will not allow you to provide invalid index names,
// because you are not required to add "_bin" or "_int" to the end of
// those names
```

```javascript
var cmd = new Riak.Commands.KV.SecondaryIndexQuery.Builder()
    .withBucketType('indexes')
    .withBucket('people')
    .withIndexName('field2_foo')
    .withIndexKey('jsmith123')
    .withCallback(query_cb)
    .build();
client.execute(cmd);

// Produces the following stack trace (truncated):
error: query_cb err: 'Error processing incoming message: error:function_clause:[{riak_api_pb_server,
    send_error,
    [{unknown_field_type,
        <<"field2_foo">>},
    {state,
        {gen_tcp,inet},
        #Port<0.68338>,
        undefined,
        ...
        ...
        ...
```

```erlang
Obj = riakc_obj:new(
    {<<"indexes">>, <<"people">>},
    <<"larry">>,
    <<"some data">>,
    <<"text/plain">>
),
MD1 = riakc_obj:get_update_metadata(Obj),
MD2 = riakc_obj:set_secondary_index(MD1, [{{foo_index, "field2"}, [1001]}]).

%% The Erlang client will return an error message along these lines:
** exception error: no function clause matching
                    riakc_obj:set_secondary_index( ... ).
```

```golang
cmd, err := riak.NewSecondaryIndexQueryCommandBuilder().
    WithBucketType("indexes").
    WithBucket("users").
    WithIndexName("field2_foo").
    WithIndexKey("jsmith123").
    Build()
if err != nil {
    return err
}

if err := cluster.Execute(cmd); err != nil {
    fmt.Println("[DevUsing2i] field name error:", err)
} else {
    return errors.New("[DevUsing2i] expected an error!")
}

// Produces the following stack trace (truncated):
error: query_cb err: 'Error processing incoming message: error:function_clause:[{riak_api_pb_server,
    send_error,
    [{unknown_field_type,
        <<"field2_foo">>},
    {state,
        {gen_tcp,inet},
        #Port<0.68338>,
        undefined,
        ...
        ...
        ...
```

```curl
curl -XPUT 127.0.0.1:8098/types/indexes/buckets/people/keys/larry \
  -H "x-riak-index-field2_foo: 1001" \
  -d 'data1'

# Response
Unknown field type for field: 'field2_foo'.
```

Incorrect data type:

```java
Location key = new Location(new Namespace("people"), "larry");
RiakObject obj = new RiakObject();
obj.getIndexes().getIndex(LongIntIndex.named("field2")).add("bar");

// The Java client will return a response indicating a type mismatch.
// The output may look something like this:

Error:(46, 68) java: no suitable method found for add(java.lang.String)
    method com.basho.riak.client.query.indexes.RiakIndex.add(java.lang.Long) is not applicable
      (argument mismatch; java.lang.String cannot be converted to java.lang.Long)
    method com.basho.riak.client.query.indexes.RiakIndex.add(java.util.Collection<java.lang.Long>) is not applicable
      (argument mismatch; java.lang.String cannot be converted to java.util.Collection<java.lang.Long>)
```

```ruby
bucket = client.bucket_type('indexes').bucket('people')
obj = Riak::RObject.new(bucket, 'larry')
obj.indexes['field2_int'] = %w{ bar }

# The Ruby client will let you get away with this...at first. But when
# you attempt to store the object, you will get an error response such
# as this:

NoMethodError: undefined method 'map' for 1001:Fixnum
```

```php
// throws \InvalidArgumentException
$object = (new \Basho\Riak\Object('{"user_data":{ ... }}', ['Content-type' => 'application/json']))
  ->addValueToIndex('twitter_int', 'not_an_int');

// throws \InvalidArgumentException
$object = (new \Basho\Riak\Object('{"user_data":{ ... }}', ['Content-type' => 'application/json']))
  ->addValueToIndex('twitter_int', ['arrays', 'are', 'not', 'strings']);

// does not throw an exception, it will just write ints as a string
// only requirement is that value is scalar (int, float, string, bool)
$object = (new \Basho\Riak\Object('{"user_data":{ ... }}', ['Content-type' => 'application/json']))
  ->addValueToIndex('twitter_bin', 12);
```

```python
bucket = client.bucket_type('indexes').bucket('people')
obj = RiakObject(client, bucket, 'larry')
obj.add_index('field2_int', 'bar')

# The Python client will let you get away with this...at first. But when you
# attempt to store the object, you will get an error response such as this:
riak.RiakError: '{precommit_fail,[{field_parsing_failed,{<<"field2_int">>,<<"bar">>}}]}'
```

```csharp
var id = new RiakObjectId("indexes", "people", "larry");
var obj = new RiakObject(id, "test value", "text/plain");
var intIdx = obj.IntIndex("test-int-idx");
intIdx.Add("invalid-value");

// The .NET client will throw a FormatException at this point
// The output may look something like this:

The value could not be parsed.
```

```javascript
var riakObj = new Riak.Commands.KV.RiakObject();
riakObj.setContentType('text/plain');
riakObj.setBucketType('indexes');
riakObj.setBucket('people');
riakObj.setKey('larry');
riakObj.addToIndex('field2_int', 'bar');
try {
    client.storeValue({ value: riakObj }, function (err, rslt) {
        logger.error("incorrect_data_type err: '%s'", err);
    });
} catch (e) {
    logger.error("incorrect_data_type err: '%s'", e);
}

// Output:
buffer.js:67
    throw new TypeError('must start with number, buffer, array or string');
          ^
TypeError: must start with number, buffer, array or string
    at new Buffer (buffer.js:67:11)
```

```erlang
Obj = riakc_obj:new(
    {<<"indexes">>, <<"people">>},
    <<"larry">>,
    <<"some data">>,
    <<"text/plain">>
),
MD1 = riakc_obj:get_update_metadata(Obj),
MD2 = riakc_obj:set_secondary_index(MD1, [{{integer_index, "field2"}, [<<"bar">>]}]).

%% The Erlang client will return an error message along these lines:
** exception error: bad argument
     in function  integer_to_list/1
        called as integer_to_list(<<"bar">>) ...
```

```golang
obj := &riak.Object{
    BucketType:      "indexes",
    Bucket:          "people",
    Key:             "larry",
    ContentType:     "text/plain",
    Charset:         "utf-8",
    ContentEncoding: "utf-8",
    Value:           []byte("My name is Larry"),
}
obj.AddToIndex("field2_int", "bar")

cmd, err := riak.NewStoreValueCommandBuilder().
    WithContent(obj).
    Build()
if err != nil {
    return err
}

if err := cluster.Execute(cmd); err != nil {
    fmt.Println("[DevUsing2i] index data type error:", err)
} else {
    return errors.New("[DevUsing2i] expected an error!")
}

// The riak.Error object will contain:
{precommit_fail,[{field_parsing_failed,{<<"field2_int">>,<<"bar">>}}]}
```

```curl
curl -XPUT 127.0.0.1:8098/types/indexes/buckets/people/keys/larry \
  -H "x-riak-index-field2_int: bar" \
  -d 'data1'

# Response
HTTP/1.1 400 Bad Request

Could not parse field 'field2_int', value 'bar'.
```

## Querying

> **Note on 2i queries and the R parameter**
>
> For all 2i queries, the [R]({{<baseurl>}}riak/kv/2.0.6/developing/app-guide/replication-properties#r-value-and-read-failure-tolerance) parameter is set to 1,
which means that queries that are run while [handoffs]({{<baseurl>}}riak/kv/2.0.6/learn/glossary/#hinted-handoff) and related operations are underway may not
return all keys as expected.

### Exact Match

The following examples perform an exact match index query.

Query a binary index:

```java
Namespace myBucket = new Namespace("indexes", "people");
BinIndexQuery biq = new BinIndexQuery.Builder(myBucket, "field1", "val1").build();
BinIndexQuery.Response response = client.execute(biq);
```

```ruby
bucket = client.bucket_type('indexes').bucket('people')
bucket.get_index('field1_bin', 'val1')
```

```php
(new \Basho\Riak\Command\Builder\QueryIndex($riak))
  ->buildBucket('people', 'indexes')
  ->withIndexName('field1_bin')
  ->withScalarValue('val1')
  ->build()
  ->execute()
  ->getResults();
```

```python
bucket = client.bucket_type('indexes').bucket('people')
bucket.get_index('field1_bin', 'val1')
```

```csharp
var riakIndexId = new RiakIndexId("indexes", "people", "field1");
// Note: using a string argument indicates a binary index query:
var indexRiakResult = client.GetSecondaryIndex(riakIndexId, "val1");
var indexResult = indexRiakResult.Value;
```

```javascript
var binIdxCmd = new Riak.Commands.KV.SecondaryIndexQuery.Builder()
    .withBucketType('indexes')
    .withBucket('people')
    .withIndexName('field1_bin')
    .withIndexKey('val1')
    .withCallback(query_cb)
    .build();
client.execute(binIdxCmd);
```

```erlang
{ok, Results} = riakc_pb_socket:get_index(
    Pid,
    {<<"indexes">>, <<"people">>}, %% bucket type and bucket name
    {binary_index, "field2"},
    <<"val1">>
).
```

```golang
c1, err := riak.NewSecondaryIndexQueryCommandBuilder().
    WithBucketType("indexes").
    WithBucket("people").
    WithIndexName("field1_bin").
    WithIndexKey("val1").
    Build()
if err != nil {
    return err
}
```

```curl
curl localhost:8098/types/indexes/buckets/people/index/field1_bin/val1
```

Query an integer index:

```java
Namespace myBucket = new Namespace("indexes", "people");
IntIndexQuery iiq = new IntIndexQuery.Builder(myBucket, "field2", 1001L)
        .build();
IntIndexQuery.Response response = client.execute(iiq);
```

```ruby
bucket = client.bucket_type('indexes').bucket('people')
bucket.get_index('field2_int', 1001)
```

```php
(new \Basho\Riak\Command\Builder\QueryIndex($riak))
  ->buildBucket('people', 'indexes')
  ->withIndexName('field2_int')
  ->withScalarValue(1001)
  ->build()
  ->execute()
  ->getResults();
```

```python
bucket = client.bucket_type('indexes').bucket('people')
bucket.get_index('field2_int', 1001)
```

```csharp
var riakIndexId = new RiakIndexId("indexes", "people", "field2");
// Note: using an integer argument indicates an int index query:
var indexRiakResult = client.GetSecondaryIndex(riakIndexId, 1001);
var indexResult = indexRiakResult.Value;
```

```javascript
var intIdxCmd = new Riak.Commands.KV.SecondaryIndexQuery.Builder()
    .withBucketType('indexes')
    .withBucket('people')
    .withIndexName('field2_int')
    .withIndexKey(1001)
    .withCallback(query_cb)
    .build();
client.execute(intIdxCmd);
```

```erlang
{ok, Results} = riakc_pb_socket:get_index(
    Pid,
    {<<"indexes">>, <<"people">>}, %% bucket type and bucket name
    {integer_index, "field2"},
    1001
).
```

```golang
cmd, err := riak.NewSecondaryIndexQueryCommandBuilder().
    WithBucketType("indexes").
    WithBucket("people").
    WithIndexName("field2_int").
    WithIntIndexKey(1001).
    Build()
if err != nil {
    return err
}
```

```curl
curl localhost:8098/types/indexes/buckets/people/index/field2_int/1001
```

The following example performs an exact match query and pipes the
results into a MapReduce job:

```curl
curl -XPOST localhost:8098/mapred \
  -H "Content-Type: application/json" \
  -d @-<<EOF
{
  "inputs": {
    "bucket": "people",
    "index": "field2_bin",
    "key":"val3"
  },
  "query": [
    {
      "reduce": {
        "language":"erlang",
        "module": "riak_kv_mapreduce",
        "function": "reduce_identity",
        "keep": true
      }
    }
  ]
}
EOF
```

### Range

The following examples perform a range query.

Query a binary index...

```java
Namespace myBucket = new Namespace("indexes", "people");
BinIndexQuery biq = new BinIndexQuery.Builder(myBucket, "field1", "val2", "val4")
        .build();
BinIndexQuery.Response response = client.execute(biq);
```

```ruby
bucket = client.bucket_type('indexes').bucket('people')
bucket.get_index('field1_bin', 'val2'..'val4')
```

```php
(new \Basho\Riak\Command\Builder\QueryIndex($riak))
  ->buildBucket('people', 'indexes')
  ->withIndexName('field1_bin')
  ->withRangeValue('val2', 'val4')
  ->build()
  ->execute()
  ->getResults();
```

```python
bucket = client.bucket_type('indexes').bucket('people')
bucket.get_index('field1_bin', 'val2', 'val4')
```

```csharp
var riakIndexId = new RiakIndexId("indexes", "people", "field1");
var indexRiakResult = client.GetSecondaryIndex(riakIndexId, "val2", "val4");
var indexResult = indexRiakResult.Value;
```

```javascript
var binIdxCmd = new Riak.Commands.KV.SecondaryIndexQuery.Builder()
    .withBucketType('indexes')
    .withBucket('people')
    .withIndexName('field1_bin')
    .withRange('val2', 'val4')
    .withCallback(query_cb)
    .build();
client.execute(binIdxCmd);
```

```erlang
{ok, Results} = riakc_pb_socket:get_index_range(
    Pid,
    {<<"indexes">>, <<"people">>}, %% bucket type and bucket name
    {binary_index, "field1"}, %% index name
    <<"val2">>, <<"val4">> %% range query for keys between "val2" and "val4"
).
```

```golang
c1, err := riak.NewSecondaryIndexQueryCommandBuilder().
    WithBucketType("indexes").
    WithBucket("people").
    WithIndexName("field1_bin").
    WithRange("val2", "val4").
    Build()
if err != nil {
    return err
}
```

```curl
curl localhost:8098/types/indexes/buckets/people/index/field1_bin/val2/val4
```

Or query an integer index...

```java
Namespace myBucket = new Namespace("indexes", "people");
IntIndexQuery iiq = new IntIndexQuery.Builder(myBucket, "field2", 1002L, 1004L)
        .build();
IntIndexQuery.Response response = client.execute(iiq);
```

```ruby
bucket = client.bucket_type('indexes').bucket('people')
bucket.get_index('field2_int', 1002..1004)
```

```php
(new \Basho\Riak\Command\Builder\QueryIndex($riak))
  ->buildBucket('people', 'indexes')
  ->withIndexName('field2_int')
  ->withRangeValue(1002, 1004)
  ->build()
  ->execute()
  ->getResults();
```

```python
bucket = client.bucket_type('indexes').bucket('people')
bucket.get_index('field2_int', 1002, 1004)
```

```csharp
var riakIndexId = new RiakIndexId("indexes", "people", "field2");
var indexRiakResult = client.GetSecondaryIndex(riakIndexId, 1002, 1004);
var indexResult = indexRiakResult.Value;
```

```javascript
var intIdxCmd = new Riak.Commands.KV.SecondaryIndexQuery.Builder()
    .withBucketType('indexes')
    .withBucket('people')
    .withIndexName('field2_int')
    .withRange(1002, 1004)
    .withCallback(query_cb)
    .build();
client.execute(intIdxCmd);
```

```erlang
{ok, Results} = riakc_pb_socket:get_index_range(
    Pid,
    {<<"indexes">>, <<"people">>}, %% bucket type and bucket name
    {integer_index, "field2"}, %% index name
    1002, 1004 %% range query for keys between "val2" and "val4"
).
```

```golang
cmd, err := riak.NewSecondaryIndexQueryCommandBuilder().
    WithBucketType("indexes").
    WithBucket("people").
    WithIndexName("field2_int").
    WithIntRange(1002, 1004).
    Build()
```

```curl
curl localhost:8098/types/indexes/buckets/people/index/field2_int/1002/1004
```

The following example performs a range query and pipes the results into
a MapReduce job:

```curl
curl -XPOST localhost:8098/mapred\
  -H "Content-Type: application/json" \
  -d @-<<EOF
{
  "inputs": {
    "bucket": "people",
    "index": "field2_bin",
    "start": "1002",
    "end": "1004"
  },
  "query": [
    {
      "reduce": {
        "language": "erlang",
        "module": "riak_kv_mapreduce",
        "function": "reduce_identity",
        "keep": true
      }
    }
  ]
}
EOF
```

#### Range with terms

When performing a range query, it is possible to retrieve the matched
index values alongside the Riak keys using `return_terms=true`. An
example from a small sampling of Twitter data with indexed hash tags:

```java
Namespace tweetsBucket = new Namespace("indexes", "tweets");
BinIndexQuery biq = new BinIndexQuery.Builder(tweetsBucket, "hashtags", "rock", "rocl")
        .withKeyAndIndex(true)
        .build();
BinIndexQuery.Response response = client.execute(biq);
```

```ruby
bucket = client.bucket_type('indexes').bucket('tweets')
bucket.get_index('hashtags_bin', 'rock'..'rocl', return_terms: true)
```

```php
(new \Basho\Riak\Command\Builder\QueryIndex($riak))
  ->buildBucket('tweets', 'indexes')
  ->withIndexName('hashtags')
  ->withRangeValue('rock', 'rocl')
  ->withReturnTerms()
  ->build()
  ->execute()
  ->getResults();
```

```python
bucket = client.bucket_type('indexes').bucket('tweets')
bucket.get_index('hashtags_bin', 'rock', 'rocl', return_terms=True)
```

```csharp
var riakIndexId = new RiakIndexId("indexes", "tweets", "hashtags");
var options = new RiakIndexGetOptions();
options.SetReturnTerms(true);
var indexRiakResult = client.GetSecondaryIndex(riakIndexId, "rock", "rocl", options);
var indexResult = indexRiakResult.Value;
```

```javascript
var binIdxCmd = new Riak.Commands.KV.SecondaryIndexQuery.Builder()
    .withBucketType('indexes')
    .withBucket('tweets')
    .withIndexName('hashtags_bin')
    .withRange('rock', 'rocl')
    .withReturnKeyAndIndex(true)
    .withCallback(query_cb)
    .build();
client.execute(binIdxCmd);
```

```erlang
{ok, Results} = riakc_pb_socket:get_index_range(
    Pid,
    {<<"indexes">>, <<"people">>}, %% bucket type and bucket name
    {binary_index, "hashtags"},     %% index name
    <<"rock">>, <<"rocl">>          %% range query for keys between "val2" and "val4"
).
```

```golang
cmd, err := riak.NewSecondaryIndexQueryCommandBuilder().
    WithBucketType("indexes").
    WithBucket("tweets").
    WithIndexName("hashtags_bin").
    WithRange("rock", "rocl").
    Build()
if err != nil {
    return err
}

if err := cluster.Execute(cmd); err != nil {
    return err
}
```

```curl
curl localhost:8098/types/indexes/buckets/tweets/index/hashtags_bin/rock/rocl?return_terms=true
```

Response:

```json
{
  "results": [
    {
      "rock": "349224101224787968"
    },
    {
      "rocks": "349223639880699905"
    }
  ]
}
```

### Pagination

When asking for large result sets, it is often desirable to ask the
servers to return chunks of results instead of a firehose. You can do so
using `max_results=<n>`, where `n` is the number of results you'd like
to receive.

Assuming more keys are available, a `continuation` value will be
included in the results to allow the client to request the next page.

Here is an example of a range query with both `return_terms` and
pagination against the same Twitter data set.

```java
Namespace tweetsBucket = new Namespace("indexes", "tweets");
BinIndexQuery biq = new BinIndexQuery.Builder(tweetsBucket, "hashtags", "ri", "ru")
        .withMaxResults(5)
        .build();
BinIndexQuery.Response response = client.execute(biq);
```

```ruby
bucket = client.bucket_type('indexes').bucket('tweets')
bucket.get_index('hashtags_bin', 'ri'..'ru', max_results: 5)
```

```php
(new \Basho\Riak\Command\Builder\QueryIndex($riak))
  ->buildBucket('tweets', 'indexes')
  ->withIndexName('hashtags')
  ->withRangeValue('ri', 'ru')
  ->withMaxResults(5)
  ->build()
  ->execute()
  ->getResults();
```

```python
bucket = client.bucket_type('indexes').bucket('tweets')
bucket.get_index('hashtags_bin', 'ri', 'ru', max_results=5)
```

```csharp
var idxId = new RiakIndexId("indexes", "tweets", "hashtags");
var options = new RiakIndexGetOptions();
options.SetMaxResults(5);
var rslt = client.GetSecondaryIndex(idxId, "ri", "ru", options);

options.SetContinuation(rslt.Continuation);
rslt = client.GetSecondaryIndex(idxId, "ri", "ru", options);
```

```javascript
function do_query(continuation) {
    var binIdxCmdBuilder = new Riak.Commands.KV.SecondaryIndexQuery.Builder()
        .withBucketType('indexes')
        .withBucket('tweets')
        .withIndexName('hashtags_bin')
        .withRange('ri', 'ru')
        .withMaxResults(5)
        .withCallback(pagination_cb);

    if (continuation) {
        binIdxCmdBuilder.withContinuation(continuation);
    }

    client.execute(binIdxCmdBuilder.build());
}

var query_keys = [];
function pagination_cb(err, rslt) {
    if (err) {
        logger.error("query_cb err: '%s'", err);
        return;
    }

    if (rslt.done) {
        query_keys.forEach(function (key) {
            logger.info("2i query key: '%s'", key);
        });
        query_keys = [];

        if (rslt.continuation) {
            do_query(rslt.continuation);
        }
    }

    if (rslt.values.length > 0) {
        Array.prototype.push.apply(query_keys,
            rslt.values.map(function (value) {
                return value.objectKey;
            }));
    }
}

do_query();
```

```erlang
{ok, Results} = riakc_pb_socket:get_index_range(
    Pid,
    {<<"indexes">>, <<"tweets">>}, %% bucket type and bucket name
    {binary_index, "hashtags"}, %% index name
    <<"ri">>, <<"ru">>, %% range query from "ri" to "ru"
    {max_results, 5}
).
```

```golang
func doPaginatedQuery(cluster *riak.Cluster, continuation []byte) error {
  builder := riak.NewSecondaryIndexQueryCommandBuilder().
    WithBucketType("indexes").
    WithBucket("tweets").
    WithIndexName("hashtags_bin").
    WithRange("ri", "ru").
    WithMaxResults(5)

  if continuation != nil && len(continuation) > 0 {
    builder.WithContinuation(continuation)
  }

  cmd, err := builder.Build()
  if err != nil {
    return err
  }

  if err := cluster.Execute(cmd); err != nil {
    return err
  }

  printIndexQueryResults(cmd)

  sciq := cmd.(*riak.SecondaryIndexQueryCommand)
  if sciq.Response == nil {
    return errors.New("[DevUsing2i] expected response but did not get one")
  }

  rc := sciq.Response.Continuation 
  if rc != nil && len(rc) > 0 {
    return doPaginatedQuery(cluster, sciq.Response.Continuation)
  }

  return nil
}

func queryingPagination(cluster *riak.Cluster) error {
  return doPaginatedQuery(cluster, nil)
}
```

```curl
curl localhost:8098/types/indexes/buckets/tweets/index/hashtags_bin/ri/ru?max_results=5&return_terms=true
```

Here is an example JSON response (your client-specific response may differ):

```json
{
  "continuation": "g2gCbQAAAAdyaXBqYWtlbQAAABIzNDkyMjA2ODcwNTcxMjk0NzM=",
  "results": [
    { "rice": "349222574510710785" },
    { "rickross": "349222868095217664" },
    { "ridelife": "349221819552763905" },
    { "ripjake": "349220649341952001" },
    { "ripjake": "349220687057129473" }
  ]
}
```

Take the continuation value from the previous result set and feed it
back into the query.

```java
Namespace tweetsBucket = new Namespace("indexes", "tweets");
BinIndexQuery biq = new BinIndexQuery.Builder(tweetsBucket, "hashtags", "ri", "ru")
        .withContinuation(BinaryValue.create("g2gCbQAAAAdyaXBqYWtlbQAAABIzNDkyMjA2ODcwNTcxMjk0NzM"))
        .withMaxResults(5)
        .withKeyAndIndex(true)
        .build();
BinIndexQuery.Response response = client.execute(biq);
```

```ruby
bucket = client.bucket_type('indexes').bucket('tweets')
bucket.get_index(
  'hashtags_bin',
  'ri'..'ru',
  continuation: 'g2gCbQAAAAdyaXBqYWtlbQAAABIzNDkyMjA2ODcwNTcxMjk0NzM',
  max_results: 5,
  return_terms: true
)
```

```php
(new \Basho\Riak\Command\Builder\QueryIndex($riak))
  ->buildBucket('tweets', 'indexes')
  ->withIndexName('hashtags')
  ->withRangeValue('ri', 'ru')
  ->withMaxResults(5)
  ->withContinuation('g2gCbQAAAAdyaXBqYWtlbQAAABIzNDkyMjA2ODcwNTcxMjk0NzM')
  ->build()
  ->execute()
  ->getResults();
```

```python
bucket = client.bucket_type('indexes').bucket('tweets')
bucket.get_index(
    'hashtags_bin',
    'ri', 'ru',
    continuation='g2gCbQAAAAdyaXBqYWtlbQAAABIzNDkyMjA2ODcwNTcxMjk0NzM',
    max_results=5,
    return_terms=True
)
```

```csharp
// rslt is the previous 2i fetch result
var idxId = new RiakIndexId("indexes", "tweets", "hashtags");
var options = new RiakIndexGetOptions();
options.SetMaxResults(5);
options.SetContinuation(rslt.Continuation);
rslt = client.GetSecondaryIndex(idxId, "ri", "ru", options);
```

```javascript
// See above example
```

```erlang
{ok, Results} = riakc_pb_socket:get_index_range(
    Pid,
    {<<"indexes">>, <<"tweets">>}, %% bucket type and bucket name
    {binary_index, "hashtags"}, %% index name
    <<"ri">>, <<"ru">>, %% range query from "ri" to "ru"
    [
        {continuation, <<"g2gCbQAAAAdyaXBqYWtlbQAAABIzNDkyMjA2ODcwNTcxMjk0NzM">>},
        {max_results, 5},
        {return_terms, true}
    ]
).
```

```golang
// See above example
```

```curl
curl localhost:8098/types/indexes/buckets/tweets/index/hashtags_bin/ri/ru?continuation=g2gCbQAAAAdyaXBqYWtlbQAAABIzNDkyMjA2ODcwNTcxMjk0NzM=&max_results=5&return_terms=true
```

The result:

```json
{
  "continuation": "g2gCbQAAAAlyb2Jhc2VyaWFtAAAAEjM0OTIyMzcwMjc2NTkxMjA2NQ==",
  "results": [
    {
      "ripjake": "349221198774808579"
    },
    {
      "ripped": "349224017347100672"
    },
    {
      "roadtrip": "349221207155032066"
    },
    {
      "roastietime": "349221370724491265"
    },
    {
      "robaseria": "349223702765912065"
    }
  ]
}
```

### Streaming

It is also possible to stream results:

```java
// Available in Riak Java Client 2.1.0 and later
int pollTimeoutMS = 200;
Namespace ns = new Namespace("indexes", "tweets");
String indexName = "hashtags";

BinIndexQuery indexQuery =
    new BinIndexQuery.Builder(ns, indexName, "ri", "ru").build();

final RiakFuture<BinIndexQuery.StreamingResponse, BinIndexQuery> streamingFuture =
    client.executeAsyncStreaming(indexQuery, pollTimeoutMS);

// For streaming commands, the future's value will be available before
// the future is complete, so you may begin to pull results from the
// provided iterator as soon as possible.
final BinIndexQuery.StreamingResponse streamingResponse = streamingFuture.get();

for (BinIndexQuery.Response.Entry e : streamingResponse)
{
    // Do something with key...
}

streamingFuture.await();
Assert.assertTrue(streamingFuture.isDone());
```

```ruby
bucket = client.bucket_type('indexes').bucket('people')
bucket.get_index('myindex_bin', 'foo', stream: true)
```

```php
/*
  It is not currently possible to stream results using the PHP client
*/
```

```python
bucket = client.bucket_type('indexes').bucket('people')
keys = []
for key in bucket.stream_index('myindex_bin', 'foo'):
    keys.append(key)
```

```csharp
var riakIndexId = new RiakIndexId("indexes", "tweets", "hashtags");
var indexRiakResult = client.StreamGetSecondaryIndex(riakIndexId, "ri", "ru");
var indexResult = indexRiakResult.Value;
foreach (var key in indexResult.IndexKeyTerms)
{
    // Do something with key...
}
```

```javascript
var binIdxCmd = new Riak.Commands.KV.SecondaryIndexQuery.Builder()
    .withBucketType('indexes')
    .withBucket('tweets')
    .withIndexName('hashtags_bin')
    .withRange('ri', 'ru')
    .withStreaming(true);
    .withCallback(query_cb) // See query_cb in other examples
    .build();
client.execute(binIdxCmd);
```

```erlang
{ok, KeyStream} = riakc_pb_socket:get_index_eq(
    Pid,
    {<<"indexes">>, <<"people">>}, %% bucket type and bucket name
    {binary_index, "myindex"}, %% index name and type
    <<"foo">>, %% value of the index
    [{stream, true}] %% enable streaming
).
```

```golang
cmd, err := riak.NewSecondaryIndexQueryCommandBuilder().
    WithBucketType("indexes").
    WithBucket("tweets").
    WithIndexName("hashtags_bin").
    WithRange("ri", "ru").
    WithStreaming(true).
    WithCallback(streamingCallback).
    Build()
if err != nil {
    return err
}

if err := cluster.Execute(cmd); err != nil {
    return err
}
```

```curl
curl localhost:8098/types/indexes/buckets/people/index/myindex_bin/foo?stream=true
```

Streaming can also be combined with `pagination` and `return_terms`.

### Sorting

As of Riak 1.4, the result set is sorted on index values (when executing
range queries) and object keys. See the pagination example above: hash
tags (2i keys) are returned in ascending order, and the object keys
(Twitter IDs) for the messages which contain the `ripjake` hash tag are
also returned in ascending order.

### Retrieve all Bucket Keys via the `$bucket` Index

The following example retrieves the keys for all objects stored in the
bucket `people` using an exact match on the special `$bucket` index.

```curl
curl localhost:8098/types/indexes/buckets/people/index/\$bucket/_
```

### Count Bucket Objects via $bucket Index

The following example performs a secondary index lookup on the $bucket
index like in the previous example and pipes this into a MapReduce that
counts the number of records in the `people` bucket. In order to
improve efficiency, the batch size has been increased from the default
size of 20.

```curl
curl -XPOST localhost:8098/mapred\
  -H "Content-Type: application/json" \
  -d @-<<EOF
{
  "inputs": {
    "bucket": "people",
    "index": "\$bucket",
    "key":"people"
  },
  "query": [
    {
      "reduce": {
        "language": "erlang",
        "module": "riak_kv_mapreduce",
        "function": "reduce_count_inputs",
        "arg": {
          "reduce_phase_batch_size":1000
        }
      }
    }
  ]
}
EOF
```
