---
title: "Using Search"
description: ""
project: "riak_kv"
project_version: "2.0.2"
menu:
  riak_kv-2.0.2:
    name: "Searching"
    identifier: "usage_searching"
    weight: 105
    parent: "developing_usage"
toc: true
aliases:
  - /riak/2.0.2/dev/using/search
  - /riak/kv/2.0.2/dev/using/search
---

[usage search schema]: ../search-schemas/
[bucket types]: ../bucket-types/

## Setup

Riak Search 2.0 is an integration of Solr (for indexing and querying)
and Riak (for storage and distribution). There are a few points of
interest that a user of Riak Search will have to keep in mind in order
to properly store and later query for values.

1. **Schemas** explain to Solr how to index fields
2. **Indexes** are named Solr indexes against which you will query
3. **Bucket-index association** signals to Riak *when* to index values
   (this also includes bucket type-index association)

{{% note %}}
Riak search uses active anti-entropy (AAE) to ensure that the data is 
consistent between the Riak backends and the Solr indexes.  When using 
Riak search, you should not disable AAE without understanding the risks 
of divergence between the data in the Riak backends and the Solr indexes 
and how that can impact your application. More information about how 
Riak search uses AAE is in the 
[Riak search reference](../../../using/reference/search/#active-anti-entropy-aae).
{{% /note %}}

Riak Search must first be configured with a Solr schema so that Solr
knows how to index value fields. If you don't define one, you're
provided with a default schema named `_yz_default`, which can be found
[on
GitHub](https://raw.githubusercontent.com/basho/yokozuna/develop/priv/default_schema.xml).

The examples in this document will presume the default. You can read
more about creating custom schemas in [Search Schema][usage search schema], which you'll likely want to use in a production environment.

Next, you must create a named Solr index through Riak Search. This index
represents a collection of similar data that you connect with to perform
queries. When creating an index, you can optionally provide a schema. If
you do not, the default schema will be used. Here we'll `curl` create an
index named `famous` with the default schema.

Both schema and index creation will be covered immediately below.

{{% note title="Note on index names" %}}
Note that index names may only be
[ASCII](http://en.wikipedia.org/wiki/ASCII) values from 32-127 (spaces,
standard punctuation, digits, and word characters). This may change in
the future to allow full [Unicode](http://en.wikipedia.org/wiki/Unicode)
support.
{{% /note %}}

All `curl` examples in this document assume that you have set an
environment variable named `RIAK_HOST`, which points to a Riak base URL,
such as `http://localhost:8098`. The appropriate value for `RIAK_HOST`
will depend on your [configuration]({{<baseurl>}}riak/kv/2.0.2/configuring/reference#client-interfaces).

## Creating an Index

Let's start by creating an index called `famous` that uses the default
schema.

```java
YokozunaIndex famousIndex = new YokozunaIndex("famous");
StoreIndex storeIndex =
    new StoreIndex.Builder(famousIndex).build();
client.execute(storeIndex);
```

```ruby
client.create_search_index('famous')
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\StoreIndex($riak))
  ->withName('famouse')
  ->build()
  ->execute();
```

```python
client.create_search_index('famous')
```

```csharp
var idx = new SearchIndex("famous");
var rslt = client.PutSearchIndex(idx);
```

```javascript
var storeIndex_cb = function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
    if (!rslt) {
        // error...
    }
};

var store = new Riak.Commands.YZ.StoreIndex.Builder()
    .withIndexName("famous")
    .withCallback(storeIndex_cb)
    .build();

client.execute(store);
```

```erlang
riakc_pb_socket:create_search_index(Pid, <<"famous">>).
```

```golang
cmd, err := riak.NewStoreIndexCommandBuilder().
    WithIndexName("famous").
    WithTimeout(time.Second * 30).
    Build()
if err != nil {
    return err
}

err = cluster.Execute(cmd)
```

```curl
export RIAK_HOST="http://localhost:8098"

curl -XPUT $RIAK_HOST/search/index/famous
```

> **Getting started with Riak clients**
>
> If you are connecting to Riak using one of Basho's official [client libraries]({{<baseurl>}}riak/kv/2.0.2/developing/client-libraries), you can find more information about getting started with your client in the [Developing with Riak KV: Getting Started]({{<baseurl>}}riak/kv/2.0.2/developing/getting-started) section.


Note that the above command is exactly the same as the following, which
explicitly defines the default schema.

```java
YokozunaIndex famousIndex = new YokozunaIndex("famous", "_yz_default");
StoreIndex storeIndex = new StoreIndex.Builder(famousIndex)
        .build();
client.execute(storeIndex);
```

```ruby
client.create_search_index("famous", "_yz_default")
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\StoreIndex($riak))
  ->withName('scores')
  ->usingSchema('_yz_default')
  ->build()
  ->execute();
```

```python
client.create_search_index('famous', '_yz_default')
```

```csharp
var idx = new SearchIndex("famous", "_yz_default");
var rslt = client.PutSearchIndex(idx);
```

```javascript
var store = new Riak.Commands.YZ.StoreIndex.Builder()
    .withIndexName("famous")
    .withSchemaName("_yz_default")
    .withCallback(storeIndex_cb)
    .build();

client.execute(store);
```

```erlang
riakc_pb_socket:create_search_index(Pid, <<"famous">>, <<"_yz_default">>, []).
```

```golang
cmd, err := riak.NewStoreIndexCommandBuilder().
    WithIndexName("famous").
    WithSchemaName("_yz_default").
    WithTimeout(time.Second * 30).
    Build()
if err != nil {
    return err
}

err = cluster.Execute(cmd)
```

```curl
curl -XPUT $RIAK_HOST/search/index/famous \
     -H 'Content-Type: application/json' \
     -d '{"schema":"_yz_default"}'
```

## Associating an Index

The last set-up item that you need to perform is to associate your Solr index
with either a [bucket type][bucket types] or a custom bucket. You
only need do this once per bucket type, and all buckets within that type
will use the same Solr index. For example, to associate a bucket type
named `animals` with the `famous` index, you can set the bucket type
property `search_index` to `animals`. If a Solr index is to be used by
only *one* Riak bucket, you can set the `search_index` property on that
bucket. If more than one bucket is to share a Solr index, a bucket type
should be used. More on bucket types in the section directly below.

### Associating via Bucket Type

We suggest that you use [bucket
types][bucket types] to namespace and configure all buckets you
use. Bucket types have a lower overhead within the cluster than the
default bucket namespace but require an additional set-up step on the
command line.

When creating a new bucket type, you can create a bucket type without
any properties and set individual buckets to be indexed. The step below
creates and activates the bucket type:

```bash
riak-admin bucket-type create animals '{"props":{}}'
riak-admin bucket-type activate animals
```

And this step applies the index to the `cats` bucket, which bears the
`animals` bucket type we just created and activated:

```curl
curl -XPUT $RIAK_HOST/types/animals/buckets/cats/props \
     -H 'Content-Type: application/json' \
     -d '{"props":{"search_index":"famous"}}'
```

Another possibility is to set the `search_index` as a default property
of the bucket type. This means _any_ bucket under that type will
inherit that setting and have its values indexed.

```bash
riak-admin bucket-type create animals '{"props":{"search_index":"famous"}}'
riak-admin bucket-type activate animals
```

If you ever need to turn off indexing for a bucket, set the
`search_index` property to the `_dont_index_` sentinel value.

### Associating an Index via Custom Bucket Properties

Although we recommend that you use all new buckets under a bucket type,
if you have existing data with a type-free bucket (i.e. under the
default bucket type) you can set the `search_index` property for a
specific bucket.

```java
Namespace catsBucket = new Namespace("cats");
StoreBucketPropsOperation storePropsOp = new StoreBucketPropsOperation.Builder(catsBucket)
        .withSearchIndex("famous")
        .build();
client.execute(storePropsOp);
```

```ruby
bucket = client.bucket('cats')
bucket.properties = {'search_index' => 'famous'}
```

```php
(new \Basho\Riak\Command\Builder\Search\AssociateIndex($riak))
    ->withName('famous')
    ->buildBucket('cats')
    ->build()
    ->execute();
```

```python
bucket = client.bucket('cats')
bucket.set_properties({'search_index': 'famous'})
```

```csharp
var properties = new RiakBucketProperties();
properties.SetSearchIndex("famous");
var rslt = client.SetBucketProperties("cats", properties);
```

```javascript
var bucketProps_cb = function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
    // success
};

var store = new Riak.Commands.KV.StoreBucketProps.Builder()
    .withBucket("cats")
    .withSearchIndex("famous")
    .withCallback(bucketProps_cb)
    .build();

client.execute(store);
```

```erlang
riakc_pb_socket:set_search_index(Pid, <<"cats">>, <<"famous">>).
```

```golang
cmd, err := riak.NewStoreBucketPropsCommandBuilder().
    WithBucketType("animals").
    WithBucket("cats").
    WithSearchIndex("famous").
    Build()
if err != nil {
    return err
}

err = cluster.Execute(cmd)
```

```curl
curl -XPUT $RIAK_HOST/buckets/cats/props \
     -H'content-type:application/json' \
     -d'{"props":{"search_index":"famous"}}'
```

Once you have created the index association, any new data will be indexed on 
ingest according to your schema.

## Riak Search Security Setup

[Security]({{<baseurl>}}riak/kv/2.0.2/using/security/) is a new feature as of
Riak 2.0 that lets an administrator limit access to certain resources.
In the case of search, your options are to limit administration of
schemas or indexes (the `search.admin` permission) to certain users, and
to limit querying (the `search.query` permission) to any index or to a
specific index. The example below shows the various options.

```bash
riak-admin security grant search.admin on schema to username
riak-admin security grant search.admin on index to username
riak-admin security grant search.query on index to username
riak-admin security grant search.query on index famous to username
```

Those permissions can also be revoked:

```bash
riak-admin security revoke search.admin on schema from username
riak-admin security revoke search.admin on index from username
riak-admin security revoke search.query on index from username
riak-admin security revoke search.query on index famous from username
```

## Indexing Values

> **Note on indexing and lag times**
>
> There is typically a one-second delay between storing an object in Riak
and that object being available in Search queries. You should take this
into account when writing Riak client tests, benchmarking, and so on.
More information can be found in the [Solr
documentation](http://wiki.apache.org/solr/SolrPerformanceFactors).

With a Solr schema, index, and association in place (and possibly a
security setup as well), we're ready to start using Riak Search. First,
populate the `cat` bucket with values, in this case information about
four cats: Liono, Cheetara, Snarf, and Panthro.

Depending on the driver you use, you may have to specify the content
type, which for this example is `application/json`. In the case of Ruby
and Python the content type is automatically set for you based on the
object given.

```java
Namespace animalsBucket = new Namespace("animals");
String json = "application/json";

RiakObject liono = new RiakObject()
        .setContentType(json)
        .setValue(BinaryValue.create("{\"name_s\":\"Lion-o\",\"age_i\":30,\"leader_b\":true}"));
RiakObject cheetara = new RiakObject()
        .setContentType(json)
        .setValue(BinaryValue.create("{\"name_s\":\"Cheetara\",\"age_i\":30,\"leader_b\":false}"));
RiakObject snarf = new RiakObject()
        .setContentType(json)
        .setValue(BinaryValue.create("{\"name_s\":\"Snarf\",\"age_i\":43,\"leader_b\":false}"));
RiakObject panthro = new RiakObject()
        .setContentType(json)
        .setValue(BinaryValue.create("{\"name_s\":\"Panthro\",\"age_i\":36,\"leader_b\":false}"));
Location lionoLoc = new Location(animalsBucket, "liono");
Location cheetaraLoc = new Location(animalsBucket, "cheetara");
Location snarfLoc = new Location(animalsBucket, "snarf");
Location panthroLoc = new Location(animalsBucket, "panthro");

StoreValue lionoStore = new StoreValue.Builder(liono).withLocation(lionoLoc).build();
// The other StoreValue operations can be built the same way

client.execute(lionoStore);
// The other storage operations can be performed the same way
```

```ruby
bucket = client.bucket_type('animals').bucket("cats")

cat = bucket.get_or_new("liono")
cat.data = {"name_s" => "Lion-o", "age_i" => 30, "leader_b" => true}
cat.store

cat = bucket.get_or_new("cheetara")
cat.data = {"name_s" => "Cheetara", "age_i" => 28, "leader_b" => false}
cat.store

cat = bucket.get_or_new("snarf")
cat.data = {"name_s" => "Snarf", "age_i" => 43}
cat.store

cat = bucket.get_or_new("panthro")
cat.data = {"name_s" => "Panthro", "age_i" => 36}
cat.store
```

```php
$bucket = new \Basho\Riak\Bucket('cats', 'animals');

$storeObjectBuilder = (new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->withLocation(new \Basho\Riak\Location('liono', $bucket))
  ->buildJsonObject(['name_s' => 'Lion-o', 'age_i' => 30, 'leader_b' => true]);

$storeObjectBuilder->build()->execute();

$storeObjectBuilder->withLocation(new \Basho\Riak\Location('cheetara', $bucket))
  ->buildJsonObject(['name_s' => 'Cheetara', 'age_i' => 28, 'leader_b' => false]);

$storeObjectBuilder->build()->execute();

$storeObjectBuilder->withLocation(new \Basho\Riak\Location('snarf', $bucket))
  ->buildJsonObject(['name_s' => 'Snarf', 'age_i' => 43]);

$storeObjectBuilder->build()->execute();

$storeObjectBuilder->withLocation(new \Basho\Riak\Location('panthro', $bucket))
  ->buildJsonObject(['name_s' => 'Panthro', 'age_i' => 36]);

$storeObjectBuilder->build()->execute();
```

```python
bucket = client.bucket_type('animals').bucket('cats')

cat = bucket.new('liono', {'name_s': 'Lion-o', 'age_i': 30, 'leader_b': True})
cat.store()

cat = bucket.new('cheetara', {'name_s':'Cheetara', 'age_i':28, 'leader_b': True})
cat.store()

cat = bucket.new('snarf', {'name_s':'Snarf', 'age_i':43})
cat.store()

cat = bucket.new('panthro', {'name_s':'Panthro', 'age_i':36})
cat.store()
```

```csharp
var lionoId = new RiakObjectId("animals", "cats", "liono");
var lionoObj = new { name_s = "Lion-o", age_i = 30, leader = true };
var lionoRiakObj = new RiakObject(lionoId, lionoObj);

var cheetaraId = new RiakObjectId("animals", "cats", "cheetara");
var cheetaraObj = new { name_s = "Cheetara", age_i = 30, leader = false };
var cheetaraRiakObj = new RiakObject(cheetaraId, cheetaraObj);

var snarfId = new RiakObjectId("animals", "cats", "snarf");
var snarfObj = new { name_s = "Snarf", age_i = 43, leader = false };
var snarfRiakObj = new RiakObject(snarfId, snarfObj);

var panthroId = new RiakObjectId("animals", "cats", "panthro");
var panthroObj = new { name_s = "Panthro", age_i = 36, leader = false };
var panthroRiakObj = new RiakObject(panthroId, panthroObj);

var rslts = client.Put(new[] {
    lionoRiakObj, cheetaraRiakObj, snarfRiakObj, panthroRiakObj
});
```

```javascript
function store_cb(err, rslt, async_cb) {
    if (err) {
        throw new Error(err);
    }
    async_cb(null, rslt);
}

var objs = [
    [ 'liono', { name_s: 'Lion-o', age_i: 30, leader: true } ],
    [ 'cheetara', { name_s: 'Cheetara', age_i: 30, leader: false } ],
    [ 'snarf', { name_s: 'Snarf', age_i: 43, leader: false } ],
    [ 'panthro', { name_s: 'Panthro', age_i: 36, leader: false } ],
];

var storeFuncs = [];
objs.forEach(function (o) {
    var storeFunc = function (async_cb) {
        var key = o[0];
        var value = o[1];
        var riakObj = new Riak.Commands.KV.RiakObject();
        riakObj.setContentType('application/json');
        riakObj.setBucketType('animals');
        riakObj.setBucket('cats');
        riakObj.setKey(key);
        riakObj.setValue(value);
        client.storeValue({ value: riakObj }, function (err, rslt) {
            store_cb(err, rslt, async_cb);
        });
    };
    storeFuncs.push(storeFunc);
});

async.parallel(storeFuncs, function (err, rslts) {
    if (err) {
        throw new Error(err);
    }
    // NB: all objects stored and indexed...
});
```

```erlang
CO = riakc_obj:new({<<"animals">>, <<"cats">>}, <<"liono">>,
    <<"{\"name_s\":\"Lion-o\", \"age_i\":30, \"leader_b\":true}">>,
    "application/json"),
riakc_pb_socket:put(Pid, CO),

C1 = riakc_obj:new({<<"animals">>, <<"cats">>}, <<"cheetara">>,
    <<"{\"name_s\":\"Cheetara\", \"age_i\":28, \"leader_b\":false}">>,
    "application/json"),
riakc_pb_socket:put(Pid, C1),

C2 = riakc_obj:new({<<"animals">>, <<"cats">>}, <<"snarf">>,
    <<"{\"name_s\":\"Snarf\", \"age_i\":43}">>,
    "application/json"),
riakc_pb_socket:put(Pid, C2),

C3 = riakc_obj:new({<<"animals">>, <<"cats">>}, <<"panthro">>,
    <<"{\"name_s\":\"Panthro\", \"age_i\":36}">>,
    "application/json"),
riakc_pb_socket:put(Pid, C3),
```

```golang
o1 := &riak.Object{
    Key:             "liono",
    Value:           []byte("{\"name_s\":\"Lion-o\",\"age_i\":30,\"leader_b\":true}"),
}
o2 := &riak.Object{
    Key:             "cheetara",
    Value:           []byte("{\"name_s\":\"Cheetara\",\"age_i\":30,\"leader_b\":false}"),
}
o3 := &riak.Object{
    Key:             "snarf",
    Value:           []byte("{\"name_s\":\"Snarf\",\"age_i\":43,\"leader_b\":false}"),
}
o4 := &riak.Object{
    Key:             "panthro",
    Value:           []byte("{\"name_s\":\"Panthro\",\"age_i\":36,\"leader_b\":false}"),
}

objs := [...]*riak.Object{o1, o2, o3, o4}

wg := &sync.WaitGroup{}
for _, obj := range objs {
    obj.ContentType = "application/json"
    obj.Charset = "utf-8"
    obj.ContentEncoding = "utf-8"

    cmd, err := riak.NewStoreValueCommandBuilder().
        WithBucketType("animals").
        WithBucket("cats").
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
curl -XPUT $RIAK_HOST/types/animals/buckets/cats/keys/liono \
     -H 'Content-Type: application/json' \
     -d '{"name_s":"Lion-o", "age_i":30, "leader_b":true}'

curl -XPUT $RIAK_HOST/types/animals/buckets/cats/keys/cheetara \
     -H 'Content-Type: application/json' \
     -d '{"name_s":"Cheetara", "age_i":28, "leader_b":false}'

curl -XPUT $RIAK_HOST/types/animals/buckets/cats/keys/snarf \
     -H 'Content-Type: application/json' \
     -d '{"name_s":"Snarf", "age_i":43}'

curl -XPUT $RIAK_HOST/types/animals/buckets/cats/keys/panthro \
     -H 'Content-Type: application/json' \
     -d '{"name_s":"Panthro", "age_i":36}'
```

If you've used Riak before, you may have noticed that this is no
different from storing values without Riak Search. That's because we
designed Riak Search with the following design goal in mind:

#### Write it like Riak, query it like Solr

But how does Riak Search know how to index values, given that you can
store opaque values in Riak? For that, we employ extractors.

## Extractors

Extractors are modules in Riak that accept a Riak value with a certain
content type and convert it into a list of fields that can be indexed by
Solr. This is done transparently and automatically as part of the
indexing process. You can even create your own [custom extractors]({{<baseurl>}}riak/kv/2.0.2/developing/usage/custom-extractors).

Our current example uses the JSON extractor, but Riak Search also
extracts indexable fields from the following content types:

* JSON (`application/json`)
* XML (`application/xml`, `text/xml`)
* Plain text (`text/plain`)
* [Riak Data Types]({{<baseurl>}}riak/kv/2.0.2/developing/data-types/)
  * counter (`application/riak_counter`)
  * map (`application/riak_map`)
  * set (`application/riak_set`)
* noop (unknown content type)

More on Riak Data Types can be found in [Riak Data Types and Search]({{<baseurl>}}riak/kv/2.0.2/developing/usage/searching-data-types).

In the examples we've seen, the JSON field `name_s` is translated to a
Solr index document field insert. Solr will index any field that it
recognizes, based on the index's schema. The default schema
(`_yz_default`) uses the suffix to decide the field type (`_s`
represents a string, `_i` is an integer, `_b` is binary and so on).

If the content type allows for nested values (e.g. JSON and XML), the
extractors will flatten each field, separated by dots. For example, if
you have this XML:

```xml
<person>
  <pets>
    <pet>
      <name_s>Spot</name_s>
    </pet>
  </pets>
</person>
```

The extractor will convert it to the Solr field `person.pets.pet.name_s`
with value `Spot`. Lists of values are assumed to be Solr multi-valued
fields.

```json
{"people_ss":["Ryan", "Eric", "Brett"]}
```

The above JSON will insert a list of three values into Solr to be
indexed: `people_ss=Ryan`, `people_ss=Eric`, `people_ss=Brett`.

You can also create your own custom extractors if your data doesn't fit
one of the default types. A full tutorial can be found in [Custom Search Extractors]({{<baseurl>}}riak/kv/2.0.2/developing/usage/custom-extractors).

### Automatic Fields

When a Riak object is indexed, Riak Search automatically inserts a few
extra fields as well. These are necessary for a variety of technical
reasons, and for the most part you don't need to think about them.
However, there are a few fields which you may find useful:

- `_yz_rk` (Riak key)
- `_yz_rt` (Riak bucket type)
- `_yz_rb` (Riak bucket)
- `_yz_err` (extraction error)

You can query on the basis of these fields, just like any other normal
Solr fields. Most of the time, however, you'll use `_yz_rk` as a query
result, which tells you the Riak key that matches the query you just
ran. Let's see this in detail by running some queries in the next
section.

## Querying

After the schema, index, association, and population/extraction/indexing
are taken care of, you can get down to the fun part of querying your
data.

### Simple Query

The basic query parameter is `q` via HTTP, or the first parameter of
your chosen driver's `search` function (there are examples from all of
our client libraries below). All distributed Solr queries are supported,
which actually includes most of the single-node Solr queries. This
example searches for all documents in which the `name_s` value begins
with `Lion` by means of a glob (wildcard) match.

```java
SearchOperation searchOp = new SearchOperation
        .Builder(BinaryValue.create("famous"), "name_s:Lion*")
        .build();
cluster.execute(searchOp);
// This will display the actual results as a List of Maps:
List<Map<String, List<String>>> results = searchOp.get().getAllResults();
// This will display the number of results:
System.out.println(results);
```

```ruby
results = client.search("famous", "name_s:Lion*")
p results
p results['docs']
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\FetchObjects($riak))
  ->withIndexName('famous')
  ->withQuery('name_s:Lion*')
  ->build()
  ->execute();

$response->getNumFound(); // 1

var_dump($response->getDocs());
```

```python
results = client.fulltext_search('famous', 'name_s:Lion*')
print results
print results['docs']
```

```csharp
var search = new RiakSearchRequest
{
    Query = new RiakFluentSearch("famous", "name_s")
        .Search("Lion*")
        .Build()
};

var rslt = client.Search(search);
RiakSearchResult searchResult = rslt.Value;
foreach (RiakSearchResultDocument doc in searchResult.Documents)
{
    var args = new[] {
        doc.BucketType,
        doc.Bucket,
        doc.Key,
        string.Join(", ", doc.Fields.Select(f => f.Value).ToArray())
    };
    Debug.WriteLine(
        format: "BucketType: {0} Bucket: {1} Key: {2} Values: {3}",
        args: args);
}
```

```javascript
function search_cb(err, rslt) {
    if (err) {
        throw new Error(err);
    }
    logger.info("docs:", JSON.stringify(rslt.docs));
}

var search = new Riak.Commands.YZ.Search.Builder()
    .withIndexName('famous')
    .withQuery('name_s:Lion*')
    .withCallback(search_cb)
    .build();
client.execute(search);
```

```erlang
{ok, Results} = riakc_pb_socket:search(Pid, <<"famous">>, <<"name_s:Lion*">>),
io:fwrite("~p~n", [Results]),
Docs = Results#search_results.docs,
io:fwrite("~p~n", [Docs]).

%% Please note that this example relies on an Erlang record definition
%% for the search_result record found here:
%% https://github.com/basho/riak-erlang-client/blob/master/include/riakc.hrl
```

```golang
cmd, err := riak.NewSearchCommandBuilder().
    WithIndexName("famous").
    WithQuery("name_s:Lion*").
    Build();
if err != nil {
    return err
}

if err := cluster.Execute(cmd); err != nil {
    return err
}

sc := cmd.(*riak.SearchCommand)
if json, jerr := json.MarshalIndent(sc.Response.Docs, "", "  "); jerr != nil {
    return jerr
} else {
    fmt.Println(string(json))
}
```

```curl
curl "$RIAK_HOST/search/query/famous?wt=json&q=name_s:Lion*" | jsonpp
```

The response to a query will be an object containing details about the
response, such as a query's max score and a list of documents which
match the given query. It's worth noting two things:

* The documents returned are Search documents (a set of Solr
  field/values), not a Riak value
* The HTTP response is a direct Solr response, while the drivers use
  Protocol Buffers and are encoded with different field names

This is a common HTTP `response` value:

```json
{
  "numFound": 1,
  "start": 0,
  "maxScore": 1.0,
  "docs": [
    {
      "leader_b": true,
      "age_i": 30,
      "name_s": "Lion-o",
      "_yz_id": "default_cats_liono_37",
      "_yz_rk": "liono",
      "_yz_rt": "default",
      "_yz_rb": "cats"
    }
  ]
}
```

The most important field returned is `docs`, which is the list of
objects that each contain fields about matching index documents. The
values you'll use most often are `_yz_rt` (Riak bucket type), `_yz_rb`
(Riak bucket), `_yz_rk` (Riak key), and `score` which represent the
similarity of the matching doc to the query via [Lucene
scoring](https://lucene.apache.org/core/4_6_0/core/org/apache/lucene/search/package-summary.html#scoring).

In this example the query fields are returned because they're stored in
Solr. This depends on your schema. If they are not stored, you'll have
to perform a separate Riak GET operation to retrieve the value using the
`_yz_rk` value.

```java
// Using the results object from above
Map<String, List<String> doc = results.get(0);
String bucketType = doc.get("_yz_rt").get(0);
String bucket = doc.get("yz_rb").get(0);
String key = doc.get("_yz_rk").get(0);
Namespace namespace = new Namespace(bucketType, bucket);
Location objectLocation = new Location(namespace, key);
FetchValue fetchOp = new FetchValue.Builder(objectLocation)
        .build();
RiakObject obj = client.execute(fetchOp).getValue(RiakObject.class);
System.out.println(obj.getValue());

// {"name_s": "Lion-o", "age_i": 30, "leader_b": true}
```

```ruby
doc = results['docs'].first
btype = Riak::BucketType.new(client, doc["_yz_rt"]) # animals
bucket = Riak::Bucket.new(client, doc["_yz_rb"])    # cats
object = bucket.get( doc["_yz_rk"] )                # liono
p object.data

# {"name_s" => "Lion-o", "age_i" => 30, "leader_b" => true}
```

```php
$doc = $response->getDocs()[0];
$btype = $doc->_yz_rt; // animals
$bucket = $doc->_yz_rb; // cats
$key = $doc->_yz_rk; // liono
$name = $doc->name_s; // Lion-o

$object = (new \Basho\Riak\Command\Builder\FetchObject($riak))
  ->buildLocation($key, $bucket, $btype)
  ->build()
  ->execute()
  ->getObject();

var_dump($object->getData());
```

```python
doc = results['docs'][0]
bucket = client.bucket_type(doc['_yz_rt']).bucket(doc['_yz_rb']) # animals/cats
object = bucket.get(doc['_yz_rk'])    # liono
print object.data

# {"name_s": "Lion-o", "age_i": 30, "leader_b": true}
```

```csharp
RiakSearchResult searchResult = searchRslt.Value;

RiakSearchResultDocument doc = searchResult.Documents.First();
var id = new RiakObjectId(doc.BucketType, doc.Bucket, doc.Key);
var rslt = client.Get(id);

RiakObject obj = rslt.Value;
Debug.WriteLine(Encoding.UTF8.GetString(obj.Value));

// {"name_s":"Lion-o","age_i":30,"leader_b":true}
```

```javascript
var doc = rslt.docs.pop();
var args = {
    bucketType: doc._yz_rt,
    bucket: doc._yz_rb,
    key: doc._yz_rk,
    convertToJs: true
};
client.fetchValue(args, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
    logger.info(rslt.values[0].value);
});
```

```erlang
[{Index,Doc}|_] = Docs,
BType  = proplists:get_value(<<"_yz_rt">>, Doc),  %% <<"animals">>
Bucket = proplists:get_value(<<"_yz_rb">>, Doc),  %% <<"cats">>
Key    = proplists:get_value(<<"_yz_rk">>, Doc),  %% <<"liono">>
{ok, Obj} = riakc_pb_socket:get(Pid, {BType, Bucket}, Key),
Val = riakc_obj:get_value(Obj),
io:fwrite("~s~n", [Val]).

%% {"name_s":"Lion-o", "age_i":30, "leader_b":true}
```

```golang
doc := sc.Response.Docs[0] // NB: SearchDoc struct type

cmd, err = riak.NewFetchValueCommandBuilder().
    WithBucketType(doc.BucketType).
    WithBucket(doc.Bucket).
    WithKey(doc.Key).
    Build()
if err != nil {
    return err
}

if err := cluster.Execute(cmd); err != nil {
    return err
}
```

```curl
curl $RIAK_HOST/types/animals/buckets/cats/keys/liono

# Response:

{"name_s":"Lion-o", "age_i":30, "leader_b":true}
```

This was one simple glob query example. There are many query options, a
more complete list of which can be found by digging into [searching
Solr](https://cwiki.apache.org/confluence/display/solr/Searching). Let's
look at a few others.

### Range Queries

Range queries are searches within a
[range](https://cwiki.apache.org/confluence/display/solr/The+Standard+Query+Parser#TheStandardQueryParser-DifferencesbetweenLuceneQueryParserandtheSolrStandardQueryParser)
of numerical or
date/[datemath](http://lucene.apache.org/solr/4_6_0/solr-core/org/apache/solr/util/DateMathParser.html)
values.

To find the ages of all famous cats who are 30 or younger: `age_i:[0 TO
30]`. If you wanted to find all cats 30 or older, you could include a
glob as a top end of the range: `age_i:[30 TO *]`.

```java
String index = "famous";
String query = "age_i:[30 TO *]";
SearchOperation searchOp = new SearchOperation
        .Builder(BinaryValue.create(index), query)
        .build();
cluster.execute(searchOp);
SearchOperation.Response results = searchOp.get();
```

```ruby
client.search("famous", "age_i:[30 TO *]")
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\FetchObjects($riak))
  ->withIndexName('famous')
  ->withQuery('age_i:[30 TO *]')
  ->build()
  ->execute();
```

```python
client.fulltext_search('famous', 'age_i:[30 TO *]')
```

```csharp
var search = new RiakSearchRequest("famous", "age_i:[30 TO *]");

/*
 * Fluent interface:
 * 
 * var search = new RiakSearchRequest
 * {
 *     Query = new RiakFluentSearch("famous", "age_i")
 *         .Between("30", "*")
 *         .Build()
 * };
 */
var rslt = client.Search(search);
```

```javascript
var search = new Riak.Commands.YZ.Search.Builder()
    .withIndexName('famous')
    .withQuery('age_i:[30 TO *]')
    .withCallback(search_cb)
    .build();
client.execute(search);
```

```erlang
riakc_pb_socket:search(Pid, <<"famous">>, <<"age_i:[30 TO *]">>),
```

```golang
cmd, err := riak.NewSearchCommandBuilder().
    WithIndexName("famous").
    WithQuery("age_i:[30 TO *]").
    Build();
if err != nil {
    return err
}

if err := cluster.Execute(cmd); err != nil {
    return err
}
```

```curl
curl "$RIAK_HOST/search/query/famous?wt=json&q=age_i:%5B30%20TO%20*%5D" | jsonpp
```

<!-- TODO: pubdate:[NOW-1YEAR/DAY TO NOW/DAY+1DAY] -->

### Boolean

You can perform logical conjunctive, disjunctive, and negative
operations on query elements as, respectively, `AND`, `OR`, and `NOT`.
Let's say we want to see who is capable of being a US Senator (at least
30 years old, and a leader). It requires a conjunctive query:
`leader_b:true AND age_i:[25 TO *]`.

```java
String index = "famous";
String query = "leader_b:true AND age_i:[30 TO *]";
Search searchOp = new Search.Builder(index, query).build();
cluster.execute(searchOp);
SearchOperation.Response results = searchOp.get();
```

```ruby
client.search("famous", "leader_b:true AND age_i:[30 TO *]")
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\FetchObjects($riak))
  ->withIndexName('famous')
  ->withQuery('leader_b:true AND age_i:[30 TO *]')
  ->build()
  ->execute();
```

```python
client.fulltext_search('famous', 'leader_b:true AND age_i:[30 TO *]')
```

```csharp
var search = new RiakSearchRequest
{
    Query = new RiakFluentSearch("famous", "leader_b")
        .Search("true").AndBetween("age_i", "30", "*")
        .Build()
};
```

```javascript
var search = new Riak.Commands.YZ.Search.Builder()
    .withIndexName('famous')
    .withQuery('leader_b:true AND age_i:[30 TO *]')
    .withCallback(search_cb)
    .build();
client.execute(search);
```

```erlang
riakc_pb_socket:search(Pid, <<"famous">>, <<"leader_b:true AND age_i:[30 TO *]">>),
```

```golang
cmd, err := riak.NewSearchCommandBuilder().
    WithIndexName("famous").
    WithQuery("leader_b:true AND age_i:[30 TO *]").
    Build();
if err != nil {
    return err
}

if err := cluster.Execute(cmd); err != nil {
    return err
}
```

```curl
curl "$RIAK_HOST/search/query/famous?wt=json&q=leader_b:true%20AND%20age_i:%5B25%20TO%20*%5D" | jsonpp
```

### Deleting Indexes

Indexes may be deleted if they have no buckets associated with them:

```java
String index = "famous";
YzDeleteIndexOperation deleteOp = new YzDeleteIndexOperation.Builder(index)
        .build();
cluster.execute(deleteOp);
```

```ruby
client.delete_search_index('famous')
```

```php
(new Command\Builder\Search\DeleteIndex($riak))
  ->withName('famous')
  ->build()
  ->execute();
```

```python
client.delete_search_index('famous')
```

```csharp
var rslt = client.DeleteSearchIndex("famous");
```

```javascript
function delete_cb(err, rslt) {
    if (err) {
        throw new Error(err);
    }
    if (rslt === true) {
        // success
    } else {
        // error
    }
}

// NB: first make sure that no bucket types or buckets are using the index
var search = new Riak.Commands.YZ.DeleteIndex.Builder()
    .withIndexName('famous')
    .withCallback(delete_cb)
    .build();
client.execute(search);
```

```erlang
riakc_pb_socket:delete_search_index(Pid, <<"famous">>, []),
```

```golang
cmd, err := riak.NewStoreBucketPropsCommandBuilder().
    WithBucketType("animals").
    WithBucket("cats").
    WithSearchIndex("_dont_index_").
    Build()
if err != nil {
    return err
}

if err := cluster.Execute(cmd); err != nil {
    return err
}

cmd, err = riak.NewDeleteIndexCommandBuilder().
    WithIndexName("famous").
    Build()
if err != nil {
    return err
}

if err := cluster.Execute(cmd); err != nil {
    return err
}
```

```curl
curl -XDELETE $RIAK_HOST/search/index/famous
```

If an index does have a bucket associated with it, then that index's
`search_index` property must be changed to either a different index name
or to the sentinel value `_dont_index_`.

```curl
curl -XPUT $RIAK_HOST/types/animals/buckets/cats/props \
     -H 'Content-Type: application/json' \
     -d '{"props":{"search_index":"_dont_index_"}}'
```

#### Pagination

A common requirement you may face is paginating searches, where an
ordered set of matching documents are returned in non-overlapping
sequential subsets (in other words, *pages*). This is easy to do with
the `start` and `rows` parameters, where `start` is the number of
documents to skip over (the offset) and `rows` are the number of results
to return in one go.

For example, assuming we want two results per page, getting the second
page is easy, where `start` is calculated as (rows per page) * (page
number - 1).

```java
int rowsPerPage = 2;
int page = 2;
int start = rowsPerPage * (page - 1);

SearchOperation searchOp = new SearchOperation
        .Builder(BinaryValue.create("famous"), "*:*")
        .withStart(start)
        .withNumRows(rowsPerPage)
        .build();
client.execute(searchOp);
StoreOperation.Response response = searchOp.get();
```

```ruby
ROWS_PER_PAGE=2
page = 2
start = ROWS_PER_PAGE * (page - 1)

client.search("famous", "*:*", {:start => start, :rows => ROWS_PER_PAGE})
```

```php
$maxRows = 2;
$page = 2;
$start = $rowsPerPAge * (page - 1);

(new \Basho\Riak\Command\Builder\Search\FetchObjects($riak))
  ->withIndexName('famous')
  ->withQuery('*:*')
  ->withMaxRows($maxRows)
  ->withStartRow($start)
  ->build()
  ->execute();
```

```python
ROWS_PER_PAGE=2
page = 2
start = ROWS_PER_PAGE * (page - 1)

client.fulltext_search('famous', '*:*', start=start, rows=ROWS_PER_PAGE)
```

```csharp
int rowsPerPage = 2;
int page = 2;
int start = rowsPerPage * (page - 1);

var search = new RiakSearchRequest
{
    Start = start,
    Rows = rowsPerPage,
    Query = new RiakFluentSearch("famous", "*")
        .Search("*")
        .Build(),
};

var rslt = client.Search(search);
```

```javascript
var rowsPerPage = 2;
var page = 2;
var start = rowsPerPage * (page - 1);

var search = new Riak.Commands.YZ.Search.Builder()
    .withIndexName('famous')
    .withQuery('*:*')
    .withStart(start)
    .withNumRows(rowsPerPage)
    .withCallback(search_cb)
    .build();
client.execute(search);
```

```erlang
-define(ROWS_PER_PAGE, 2).

Page = 2,
Start = ?ROWS_PER_PAGE * (Page - 1),

riakc_pb_socket:search(Pid, <<"famous">>, <<"*:*">>, [{start, Start},{rows, ?ROWS_PER_PAGE}]),
```

```golang
rowsPerPage := uint32(2)
page := uint32(2)
start := rowsPerPage * (page - uint32(1))

cmd, err := riak.NewSearchCommandBuilder().
    WithIndexName("famous").
    WithQuery("*:*").
    WithStart(start).
    WithNumRows(rowsPerPage).
    Build();
if err != nil {
    return err
}

if err := cluster.Execute(cmd); err != nil {
    return err
}
```

```curl
ROWS_PER_PAGE=2
PAGE=2
START=$(($ROWS_PER_PAGE * ($PAGE-1)))

curl
curl "$RIAK_HOST/search/query/famous?wt=json&q=*:*&start=$START&rows=$ROWS_PER_PAGE" | jsonpp
```

### Pagination Warning

Distributed pagination in Riak Search cannot be used reliably when
sorting on fields that can have different values per replica of the same
object, namely `score` and `_yz_id`. In the case of sorting by these
fields, you may receive redundant objects. In the case of `score`, the
top-N can return different results over multiple runs.

If you are paginating simply to get all keys that match and don't care
about the score, then you can sort on type-bucket-key (eg. `_yz_rt asc`,
`_yz_rb asc`, `_yz_rk asc`) to get consistent results.

If you want to sort by score without repeating results then you must set
`rows` >= `numFound`. This requires having some idea of how many rows
will match before running the query.

[This issue](https://github.com/basho/yokozuna/issues/355) is caused by
the way Search must minimally distribute a query across multiple Solr
nodes (called a *coverage plan*) and then filter duplicate results to
retrieve a full result set. Since this plan is frequently recalculated,
successive page queries may use a different plan, and thus calculate
alternate `score`s or filter different `_yz_id` values. We have plans to
fix this shortcoming in a future version of Riak.

### MapReduce

Riak Search allows for piping search results as inputs for
[MapReduce]({{<baseurl>}}riak/kv/2.0.2/developing/usage/mapreduce/) jobs. This is a useful cross-section for
performing post-calculations of results or aggregations of ad-hoc
queries. The Riak Search MapReduce integration works similarly to
regular MapReduce, with the notable exception that your input is not a
bucket, but rather index and query arguments to the `yokozuna` module
and `mapred_search` function (an Erlang `module:function` pair that adds
the Riak Search hook to MapReduce).

```json
{
  "inputs": {
    "module": "yokozuna",
    "function": "mapred_search",
    "arg": ["famous","NOT leader_b:true"]
  },
  "query": [
    {
      "map": {
        "language": "javascript",
        "keep": false,
        "source": "function(v) { return [1]; }"
      }
    },
    {
      "reduce": {
        "language": "javascript",
        "keep": true,
        "name": "Riak.reduceSum"
      }
    }
  ]
}
```

In this example we're searching for all famous cats that are not
leaders and counting up the results using Javascript for both map and
reduce. It should return the reduced sum of `[3]`.

```curl
curl -XPOST $RIAK_HOST/mapred \
     -H 'Content-Type: application/json' \
     -d '{"inputs":{"module":"yokozuna","function":"mapred_search","arg":["famous","NOT leader_b:true"]},"query":[{"map":{"language":"javascript","keep":false,"source":"function(v) { return [1]; }"}},{"reduce":{"language":"javascript","keep":true,"name":"Riak.reduceSum"}}]}'
```
