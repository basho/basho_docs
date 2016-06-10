---
title: "Searching with Data Types"
description: ""
project: "riak_kv"
project_version: "2.0.1"
menu:
  riak_kv-2.0.1:
    name: "Searching with Data Types"
    identifier: "usage_search_data_types"
    weight: 111
    parent: "developing_usage"
toc: true
aliases:
  - /riak/2.0.1/dev/search/search-data-types
  - /riak/kv/2.0.1/dev/search/search-data-types
canonical_link: "https://docs.basho.com/riak/kv/latest/developing/usage/searching-data-types"
---

Although [Riak Data Types](/riak/kv/2.0.1/developing/data-types) function differently from other
Riak objects in some respects, when you're using Search you can think of
them as normal Riak objects with special metadata attached (metadata
that you don't need to worry about as a user). Riak's [counters](/riak/kv/2.0.1/developing/data-types/counters), [sets](/riak/kv/2.0.1/developing/data-types/sets), and [maps](/riak/kv/2.0.1/developing/data-types/maps)
can be indexed and have their contents searched just like other Riak
objects.

## Data Type MIME Types

Like all objects stored in Riak, Riak Data Types are assigned content
types. Unlike other Riak objects, this happens automatically. When you
store, say, a counter in Riak, it will automatically be assigned the
type `application/riak_counter`. The table below provides the full list
of content types:

Data Type | Content Type
:---------|:------------
Counters | `application/riak_counter`
Sets | `application/riak_set`
Maps | `application/riak_map`

When using Search, you won't need to worry about this, as Riak Data
Types are automatically indexed on the basis of these content types.

## Data Type Schemas

There are two types of schemas related to Riak Data Types:

* **Top-level schemas** relate to Data Types that are stored at the key
    level (counters and sets)
* **Embedded schemas** relate to Data Types nested inside of maps
    (flags, counters, registers, and sets)

As you can see from the [default Search
schema](https://github.com/basho/yokozuna/blob/develop/priv/default_schema.xml#L96),
each of the Data Types has its own default schema, with the exception of
maps, which means that the `_yz_default` schema will automatically index
Data Types on the basis of their assigned content type. This means that
there is no extra work involved in indexing Riak Data Types. You can
simply store them and begin querying, provided that they are properly
indexed, which is covered in the [examples](#riak-data-types-and-search) section below.

As mentioned above, there are no default schemas available for maps.
This is because maps are essentially carriers for the other Data Types.
Even when maps are embedded within other maps, all of the data that you
might wish to index and search is contained in counters, sets,
registers, and flags.

The sections immediately below provide the default schemas for each Riak
Data Type. Because you will not need to manipulate these default schemas
to search Data Types, they are provided only for reference.

### Top-level Schemas

The default schema for [counters](/riak/kv/2.0.1/developing/data-types/counters) indexes each
counter as an integer.

```xml
<field name="counter" type="int" indexed="true" stored="true" multiValued="false" />
```

Constructing queries for counters involves prefacing the query with
`counter`. Below are some examples:

Query | Syntax
:-----|:------
Counters with a value over 10 | `counter:[10 TO *]`
Counters with a value below 10 and above 50 | `counter:[* TO 10] AND counter:[50 TO *]`
Counters with a value of 15 | `counter:15`
All counters within the index | `counter:*`

The schema for [sets](/riak/kv/2.0.1/developing/data-types/sets) indexes each element of a set as
a string and indexes the set itself as multi-valued.

```xml
<field name="set" type="string" indexed="true" stored="false" multiValued="true" />
```

To query sets, preface the query with `set`. The table below shows some
examples:

Query | Syntax
:-----|:------
Sets that contain the value `apple` | `set:apple`
Sets that contain an item beginning with `level` | `set:level*`
Sets that contain both `apple` and `orange` | `set:apple AND set:orange`
All sets within the index | `set:*`

### Embedded Schemas

For searching within [maps](/riak/kv/2.0.1/developing/data-types/maps), there are four schemas
for embedded, aka dynamic, fields. Flags are indexed as booleans:

```xml
<dynamicField name="*_flag" type="boolean" indexed="true" stored="true" multiValued="false" />
```

Counters, like their top-level counterparts, are indexed as integers:

```xml
<dynamicField name="*_counter" type="int" indexed="true" stored="true" multiValued="false" />
```

Registers are indexed as strings, but unlike sets they are not
multi-valued.

```xml
<dynamicField name="*_register" type="string" indexed="true" stored="true" multiValued="false" />
```

Finally, sets at the embedded level are indexed as multi-valued strings.

```xml
<dynamicField name="*_set" type="string" indexed="true" stored="true" multiValued="true" />
```

To query embedded fields, you must provide the name of the field. The
table below provides some examples:

Query | Syntax
:-----|:------
Maps containing a set called `hobbies` | `hobbies_set:*`
Maps containing a `score` counter over 50 | `score_counter:[50 TO *]`
Maps containing disabled `advanced` flags | `advanced_flag:false`
Maps containing enabled `advanced` flags and `score` counters under 10 | `advanced_flag:true AND score_counter:[* TO 10]`

You can also query maps within maps, which is covered in the **Querying
maps within maps** section below.

## Data Types and Search Examples

In this section, we'll start with two simple examples, one involving
counters and the other involving sets. Later on, we'll introduce a
slightly more complex map example.

## Counters Example

Let's say that we're storing scores in a multiplayer online game in
Riak. The game is called Boulderdash and it involves smashing digital
boulders armed with nothing but witty retorts and arcane trivia
knowledge. We'll create and activate a [bucket type](/riak/kv/2.0.1/developing/usage/bucket-types) for [storing counters](/riak/kv/2.0.1/developing/data-types/counters) simply called
`counters`, like so:

```bash
riak-admin bucket-type create counters '{"props":{"datatype":"counter"}}'
riak-admin bucket-type activate counters
```

Now, we'll create a search index called `scores` that uses the default
schema (as in some of the examples above):

```java
YokozunaIndex scoresIndex = new YokozunaIndex("scores", "_yz_default");
StoreIndex storeIndex = new StoreIndex.Builder(scoresIndex)
        .build();
client.execute(storeIndex);
```

```ruby
client.create_search_index('scores', '_yz_default')
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\StoreIndex($riak))
  ->withName('scores')
  ->usingSchema('_yz_default')
  ->build()
  ->execute();
```

```python
client.create_search_index('scores', '_yz_default')
```

```csharp
var idx = new SearchIndex("scores", "_yz_default");
var rslt = client.PutSearchIndex(idx);
```

```javascript
var options = {
    schemaName: '_yz_default',
    indexName: 'scores'
};
client.storeIndex(options, function (err, rslt) {
});
```

```erlang
riakc_pb_socket:create_search_index(Pid, <<"scores">>, <<"_yz_default">>, []).
```

```curl
curl -XPUT $RIAK_HOST/search/index/hobbies \
  -H 'Content-Type: application/json' \
  -d '{"schema":"_yz_default"}'
```

Now, we can modify our `counters` bucket type to associate that bucket
type with our `scores` index:

```bash
riak-admin bucket-type update counters '{"props":{"search_index":"scores"}}'
```

At this point, all of the counters that we stored in any bucket with the
bucket type `counters` will be indexed in our `scores` index. So let's
start playing with some counters. All counters will be stored in the
bucket `people`, while the key for each counter will be the username of
each player:

```java
Namespace peopleBucket = new Namespace("counters", "people");

Location christopherHitchensCounter = new Location(peopleBucket, "christ_hitchens");
CounterUpdate cu = new CounterUpdate(10);
UpdateCounter update = new UpdateCounter.Builder(christopherHitchensCounter, cu)
        .build();
client.execute(update);

Location joanRiversCounter = new Location(peopleBucket, "joan_rivers");
CounterUpdate cu = new CounterUpdate(25);
UpdateCounter update = new UpdateCounter.Builder(joanRiversCounter, cu)
        .build();
client.execute(update);
```

```ruby
bucket = client.bucket('people')

christopher_hitchens_counter = Riak::Crdt::Counter.new(bucket, 'chris_hitchens', 'counters')
christopher_hitchens_counter.increment(10)

joan_rivers_counter = Riak::Crdt::Counter.new(bucket, 'joan_rivers', 'counters')
joan_rivers_counter.increment(25)
```

```php
$builder = (new \Basho\Riak\Command\Builder\IncrementCounter($riak))
    ->withIncrement(10)
    ->buildLocation('chris_hitchens', 'people', 'counters');

$builder->build->execute();

$builder->withIncrement(25)
    ->buildLocation('joan_rivers', 'people', 'counters')
    ->build()
    ->execute();
```

```python
from riak.datatypes import Counter

bucket = client.bucket_type('counters').bucket('people')

christopher_hitchens_counter = Counter(bucket, 'chris_hitchens')
christopher_hitchens_counter.increment(10)
christopher_hitchens_counter.store()

joan_rivers_counter = Counter(bucket, 'joan_rivers')
joan_rivers_counter.increment(25)
joan_rivers_counter.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Search/SearchDataTypes.cs

var cmd = new UpdateCounter.Builder()
    .WithBucketType("counters")
    .WithBucket("people")
    .WithKey("christ_hitchens")
    .WithIncrement(10)
    .Build();
RiakResult rslt = client.Execute(cmd);

cmd = new UpdateCounter.Builder()
    .WithBucketType("counters")
    .WithBucket("people")
    .WithKey("joan_rivers")
    .WithIncrement(25)
    .Build();
rslt = client.Execute(cmd);
```

```javascript
var funcs = [
    function (async_cb) {
        var options = {
            bucketType: 'counters',
            bucket: 'people',
            key: 'christ_hitchens',
            increment: 10
        };

        client.updateCounter(options, function (err, rslt) {
            throwIfErr(err);
            async_cb();
        });
    },
    function (async_cb) {
        var options = {
            bucketType: 'counters',
            bucket: 'people',
            key: 'joan_rivers',
            increment: 25
        };

        client.updateCounter(options, function (err, rslt) {
            throwIfErr(err);
            async_cb();
        });
    }
];

async.parallel(funcs, function (err, rslts) {
    throwIfErr(err);
});
```

```erlang
ChristopherHitchensCounter = riakc_counter:new(),
HitchensCounter1 = riakc_counter:increment(10, ChristopherHitchensCounter),
JoanRiversCounter = riakc_counter:new(),
RiversCounter1 = riakc_counter:increment(25, JoanRiversCounter),
riakc_pb_socket:update_type(Pid,
                            {<<"counters">>, <<"people">>},
                            <<"chris_hitchens">>,
                            riakc_counter:to_op(HitchensCounter1)),
riakc_pb_socket:update_type(Pid,
                            {<<"counters">>, <<"people">>},
                            <<"joan_rivers">>,
                            riakc_counter:to_op(RiversCounter1)).
```

```curl
# We do not recommend working with Riak Data Types via curl. Try using
# one of our client libraries instead.
```

So now we have two counters, one with a value of 10 and the other with a
value of 25. Let's query to see how many counters have a value greater
than 20, just to be sure:

```java
String index = "scores";
String query = "counter:[20 TO *]";
SearchOperation searchOp = new SearchOperation.Builder(BinaryValue.create(index), query)
        .build();
cluster.execute(searchOp);
SearchOperation.Response results = searchOp.get();
```

```ruby
results = client.search('scores', 'counter:[20 TO *]')
# This should return a Hash with fields like 'num_found' and 'docs'

results['num_found']
# 1
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\FetchObjects($riak))
  ->withIndexName('scores')
  ->withQuery('counter:[20 TO *]')
  ->build()
  ->execute();

$response->getNumFound(); // 1
```

```python
results = client.fulltext_search('scores', 'counter:[20 TO *]')
# This should return a dict with fields like 'num_found' and 'docs'

results['num_found']
# 1
```

```csharp
var search = new RiakSearchRequest("scores", "counter:[20 TO *]");
var rslt = client.Search(search);
RiakSearchResult searchResult = rslt.Value;
Console.WriteLine("Num found: {0}", searchResult.NumFound);
```

```javascript
function search_cb(err, rslt) {
    logger.info("counter numFound: '%d', docs: '%s'",
        rslt.numFound, JSON.stringify(rslt.docs));

    var doc = rslt.docs[0];
    var key = doc['_yz_rk'];
    var bucket = doc['_yz_rb'];
    var bucketType = doc['_yz_rt'];
}

var searchCmd = new Riak.Commands.YZ.Search.Builder()
    .withIndexName('scores')
    .withQuery('counter:[20 TO *]')
    .withCallback(search_cb)
    .build();

client.execute(searchCmd);
```

```erlang
{ok, Results} = riakc_pb_socket:search(Pid, <<"scores">>, <<"counter:[20 TO *]">>),
NumberFound = Results#search_results.num_found.
%% 1
```

```curl
curl "$RIAK_HOST/search/query/scores?wt=json&q=counter:[20 TO *]" | jsonpp
```

And there we are: only one of our two stored sets has a value over 20.
To find out which set that is, we can dig into our results:

```java
// Using the "results" object from above:
int numberFound = results.numResults();
Map<String, List<String>> foundObject = results.getAllResults().get(0);
String key = foundObject.get("_yz_rk").get(0); // "joan_rivers"
String bucket = foundObject.get("_yz_rb").get(0); // "people"
String bucketType = foundObject.get("_yz_rt").get(0); // "counters"
```

```ruby
doc = results['docs'][0]

# The key
doc['_yz_rk'] # 'joan_rivers'

# The bucket
doc['_yz_rb'] # 'people'

# The bucket type
doc['_yz_rt'] # 'counters'
```

```php
$doc = $response->getDocs()[0];

# The key
$doc['_yz_rk'] # 'joan_rivers'

# The bucket
$doc['_yz_rb'] # 'people'

# The bucket type
$doc['_yz_rt'] # 'counters'
```

```python
doc = results['docs'][0]

# The key
doc['_yz_rk'] # 'joan_rivers'

# The bucket
doc['_yz_rb'] # 'people'

# The bucket type
doc['_yz_rt'] # 'counters'
```

```csharp
var search = new RiakSearchRequest("scores", "counter:[20 TO *]");
var rslt = client.Search(search);

RiakSearchResult searchResult = rslt.Value;
Console.WriteLine("Num found: {0}", searchResult.NumFound);

var firstDoc = searchResult.Documents.First();
Console.WriteLine("Key: {0} Bucket: {1} Type: {2}",
    firstDoc.Key, firstDoc.Bucket, firstDoc.BucketType);
```

```javascript
var doc = rslt.docs[0];

var key = doc['_yz_rk'];
var bucket = doc['_yz_rb'];
var bucketType = doc['_yz_rt'];
```

```erlang
Doc = lists:nth(1, Docs),
Key = proplists:get_value(<<"_yz_rk">>, Doc),
Bucket = proplists:get_value(<<"_yz_rb">>, Doc),
BucketType = proplists:get_value(<<"_yz_rt", Doc).
```

```curl
# Use the JSON object from above to locate bucket, key, and bucket type
# information
```

Alternatively, we can see how many counters have values below 15:

```java
String index = "scores";
String query = "counter:[* TO 15]";
SearchOperation searchOp = new SearchOperation
        .Builder(BinaryValue.create("scores"), "counter:[* TO 15]")
        .build();
cluster.execute(searchOp);
SearchOperation.Response results = searchOp.get();
```

```ruby
results = client.search('scores', 'counter:[* TO 15]')
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\FetchObjects($riak))
  ->withIndexName('scores')
  ->withQuery('counter:[* TO 15]')
  ->build()
  ->execute();

$response->getNumFound(); // 1
```

```python
results = client.fulltext_search('scores', 'counter:[* TO 15]')
```

```csharp
var search = new RiakSearchRequest("scores", "counter:[* TO 15]");
var rslt = client.Search(search);
```

```javascript
var searchCmd = new Riak.Commands.YZ.Search.Builder()
    .withIndexName('scores')
    .withQuery('counter:[* TO 15]')
    .withCallback(search_cb)
    .build();

client.execute(searchCmd);
```

```erlang
{ok, Results} = riakc_pb_socket:search(Pid, <<"scores">>, <<"counter:[* TO 15]").
```

```curl
curl "$RIAK_HOST/search/query/scores?wt=json&q=counter:[* TO 15]" | jsonpp
```

Or we can see how many counters have a value of 17 exactly:

```java
// Using the same method as above, just changing the query:
String query = "counter:17";
```

```ruby
results = client.search('scores', 'counter:17')
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\FetchObjects($riak))
  ->withIndexName('scores')
  ->withQuery('counter:17')
  ->build()
  ->execute();
```

```python
results = client.fulltext_search('scores', 'counter:17')
```

```csharp
var search = new RiakSearchRequest("scores", "counter:17");
var rslt = client.Search(search);
```

```javascript
var searchCmd = new Riak.Commands.YZ.Search.Builder()
    .withIndexName('scores')
    .withQuery('counter:17')
    .withCallback(search_cb)
    .build();

client.execute(searchCmd);
```

```erlang
{ok, Results} = riakc_pb_socket:search(Pid, <<"scores">>, <<"counter:17">>).
```

```curl
curl "$RIAK_HOST/search/query/scores?wt=json&q=counter:17" | jsonpp
```

## Sets Example

Let's say that we're storing information about the hobbies of a group of
people in sets. We'll create and activate a [bucket type](/riak/kv/2.0.1/developing/usage/bucket-types) for [storing sets](/riak/kv/2.0.1/developing/data-types/sets) simply called `sets`,
like so:

```bash
riak-admin bucket-type create sets '{"props":{"datatype":"set"}}'
riak-admin bucket-type activate sets
```

Now, we'll create a Search index called `hobbies` that uses the default
schema (as in some of the examples above):

```java
YokozunaIndex hobbiesIndex = new YokozunaIndex("hobbies");
StoreIndex storeIndex =
  new StoreIndex.Builder(hobbiesIndex).build();
client.execute(storeIndex);
```

```ruby
client.create_search_index('hobbies', '_yz_default')
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\StoreIndex($riak))
  ->withName('hobbies')
  ->usingSchema('_yz_default')
  ->build()
  ->execute();
```

```python
client.create_search_index('hobbies', '_yz_default')
```

```csharp
var searchIndex = new SearchIndex("hobbies", "_yz_default");
var rslt = client.PutSearchIndex(searchIndex);
```

```javascript
var options = {
    schemaName: '_yz_default',
    indexName: 'hobbies'
};
client.storeIndex(options, function (err, rslt) {
});
```

```erlang
riakc_pb_socket:create_search_index(Pid, <<"hobbies">>, <<"_yz_default">>).
```

```curl
curl -XPUT $RIAK_HOST/search/index/hobbies \
  -H 'Content-Type: application/json' \
  -d '{"schema": "_yz_default"}'
```

Now, we can modify our `sets` bucket type to associate that bucket type
with our `hobbies` index:

```bash
riak-admin bucket-type update sets '{"props":{"search_index":"hobbies"}}'
```

Now, all of the sets that we store in any bucket with the bucket type
`sets` will be automatically indexed as a set. So let's say that we
store three sets for two different people describing their respective
hobbies, in the bucket `people`:

```java
Namespace peopleBucket = new Namespace("sets", "people");

Location mikeDitkaSet = new Location(peopleBucket, "ditka");
SetUpdate su1 = new SetUpdate()
        .add("football")
        .add("winning");
UpdateSet update1 = new UpdateSet.Builder(mikeDitkaSet, su1).build();

Location ronnieJamesDioSet = new Location(peopleBucket, "dio");
SetUpdate su2 = new SetUpdate()
        .add("wailing")
        .add("rocking")
        .add("winning");
UpdateSet update2 = new UpdateSet.Builder(ronnieJamesDioSet, su2).build();

client.execute(update1);
client.execute(update2);
```

```ruby
bucket = client.bucket('people')

mike_ditka_set = Riak::Crdt::Set.new(bucket, 'ditka', 'sets')
mike_ditka_set.add('football')
mike_ditka_set.add('winning')

ronnie_james_dio_set = Riak::Crdt::Set.new(bucket, 'dio', 'sets')
ronnie_james_dio_set.add('wailing')
ronnie_james_dio_set.add('rocking')
ronnie_james_dio_set.add('winning')
```

```php
$builder = (new \Basho\Riak\Command\Builder\UpdateSet($riak))
    ->add('football')
    ->add('winning')
    ->buildLocation('ditka', 'people', 'counters');

$builder->build->execute();

$builder->add('wailing')
    ->add('rocking')
    ->add('winning')
    ->buildLocation('dio', 'people', 'counters');
    ->build()
    ->execute();
```

```python
from riak.datatypes import Set

bucket = client.bucket_type('sets').bucket('people')

mike_ditka_set = Set(bucket, 'ditka')
mike_ditka_set.add('football')
mike_ditka_set.add('winning')
mike_ditka_set.store()

ronnie_james_dio_set = Set(bucket, 'dio')
ronnie_james_dio_set.add('wailing')
ronnie_james_dio_set.add('rocking')
ronnie_james_dio_set.add('winning')
ronnie_james_dio_set.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Search/SearchDataTypes.cs

var cmd = new UpdateSet.Builder()
    .WithBucketType("sets")
    .WithBucket("people")
    .WithKey("ditka")
    .WithAdditions(new[] { "football", "winning" })
    .Build();
RiakResult rslt = client.Execute(cmd);

cmd = new UpdateSet.Builder()
    .WithBucketType("sets")
    .WithBucket("people")
    .WithKey("dio")
    .WithAdditions(new[] { "wailing", "rocking", "winning" })
    .Build();
rslt = client.Execute(cmd);
```

```javascript
var funcs = [
    function (async_cb) {
        var options = {
            bucketType: 'sets',
            bucket: 'people',
            key: 'ditka',
            additions: ['football', 'winning']
        };

        client.updateSet(options, function (err, rslt) {
            throwIfErr(err);
            async_cb();
        });
    },
    function (async_cb) {
        var options = {
            bucketType: 'sets',
            bucket: 'people',
            key: 'dio',
            additions: ['wailing', 'rocking', 'winning']
        };

        client.updateSet(options, function (err, rslt) {
            throwIfErr(err);
            async_cb();
        });
    }
];

async.parallel(funcs, function (err, rslts) {
    throwIfErr(err);
});
```

```erlang
MikeDitkaSet = riakc_set:new(),
riakc_set:add_element(<<"football">>, MikeDitkaSet),
riakc_set:add_element(<<"winning">>, MikeDitkaSet),
RonnieJamesDioSet = riakc_set:new(),
riakc_set:add_element(<<"wailing">>, RonnieJamesDioSet),
riakc_set:add_element(<<"rocking">>, RonnieJamesDioSet),
riakc_set:add_element(<<"winning">>, RonnieJamesDioSet),

riakc_pb_socket:update_type(Pid,
                            {<<"sets">>, <<"people">>},
                            <<"ditka">>,
                            riakc_set:to_op(MikeDitkaSet)),
riakc_pb_socket:update_type(Pid,
                            {<<"sets">>, <<"people">>},
                            <<"dio">>,
                            riakc_set:to_op(RonnieJamesDioSet)).
```

Now, we can query our `hobbies` index to see if anyone has the hobby
`football`:

```java
// Using the same method explained above, just changing the query:
String query = "set:football";
```

```ruby
results = client.search('hobbies', 'set:football')
# This should return a dict with fields like 'num_found' and 'docs'
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\FetchObjects($riak))
  ->withIndexName('hobbies')
  ->withQuery('set:football')
  ->build()
  ->execute();
```

```python
results = client.fulltext_search('hobbies', 'set:football')
# This should return a dict with fields like 'num_found' and 'docs'
```

```csharp
var search = new RiakSearchRequest("hobbies", "set:football");
var rslt = client.Search(search);

RiakSearchResult searchResult = rslt.Value;
Console.WriteLine("Num found: {0}", searchResult.NumFound);

var firstDoc = searchResult.Documents.First();
Console.WriteLine("Key: {0} Bucket: {1} Type: {2}",
    firstDoc.Key, firstDoc.Bucket, firstDoc.BucketType);
```

```javascript
function search_cb(err, rslt) {
    logger.info("sets numFound: '%d', docs: '%s'",
        rslt.numFound, JSON.stringify(rslt.docs));

    var doc = rslt.docs[0];
    var key = doc['_yz_rk'];
    var bucket = doc['_yz_rb'];
    var bucketType = doc['_yz_rt'];
}

var searchCmd = new Riak.Commands.YZ.Search.Builder()
    .withIndexName('hobbies')
    .withQuery('set:football')
    .withCallback(search_cb)
    .build();

client.execute(searchCmd);
```

```erlang
{ok, Results} = riakc_pb_socket:search(Pid, <<"hobbies">>, <<"set:football">>).
```

```curl
curl "$RIAK_HOST/search/query/hobbies?wt=json&q=set:football" | jsonpp
```

Let's see how many sets contain the element `football`:

```java
// Using the same method explained above for getting search results:
int numberFound = results.numResults(); // 1
```

```ruby
results['num_found']
# 1
```

```php
$response->getNumFound(); // 1
```

```python
results['num_found']
# 1
```

```csharp
RiakSearchResult searchResult = rslt.Value;
Console.WriteLine("Num found: {0}", searchResult.NumFound);
```

```javascript
rslt.numFound;
// 1
```

```erlang
NumberFound = Results#search_results.num_found.
%% 1
```

```curl
```

Success! We stored two sets, only one of which contains the element
`football`. Now, let's see how many sets contain the element `winning`:

```java
// Using the same method explained above, just changing the query:
String query = "set:winning";

// Again using the same method from above:
int numberFound = results.numResults(); // 2
```

```ruby
results = client.search('hobbies', 'set:winning')
results['num_found']
# 2
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\FetchObjects($riak))
  ->withIndexName('hobbies')
  ->withQuery('set:winning')
  ->build()
  ->execute();

$response->getNumFound(); // 2
```

```python
results = client.fulltext_search('hobbies', 'set:winning')
results['num_found']
# 2
```

```csharp
var search = new RiakSearchRequest("hobbies", "set:winning");
```

```javascript
var searchCmd = new Riak.Commands.YZ.Search.Builder()
    .withIndexName('hobbies')
    .withQuery('set:winning')
    .withCallback(search_cb)
    .build();

client.execute(searchCmd);
```

```erlang
{ok, Results} = riakc_pb_socket:search(Pid, <<"hobbies">>, <<"set:winning">>).
NumberFound = Results#search_results.num_found.
%% 2
```

Just as expected, both sets we stored contain the element `winning`.

## Maps Example

This example will build on the example in the [Using Data Types](/riak/kv/2.0.1/developing/data-types)
tutorial. That tutorial walks you through storing CMS-style user data in
Riak [maps](/riak/kv/2.0.1/developing/data-types/maps), and we'd suggest that you
familiarize yourself with that tutorial first. More specifically, user
data is stored in the following fields in each user's map:

* first name in a `first_name` register
* last name in a `last_name` register
* whether the user is an enterprise customer in an `enterprise_customer`
  flag
* the number of times the user has visited the company page in a
  `page_visits` counter
* a list of the user's interests in an `interests` set

First, let's create and activate a bucket type simply called `maps` that
is set up to store Riak maps:

```bash
riak-admin bucket-type create maps '{"props":{"datatype":"map"}}'
riak-admin bucket-type activate maps
```

Now, let's create a search index called `customers` using the default
schema:

```java
YokozunaIndex customersIndex = new YokozunaIndex("customers", "_yz_default");
StoreIndex storeIndex =
  new StoreIndex.Builder(customersIndex).build();
client.execute(storeIndex);
```

```ruby
client.create_search_index('customers', '_yz_default')
```

```php
(new Command\Builder\Search\StoreIndex($riak))
  ->withName('customers')
  ->usingSchema('_yz_default')
  ->build()
  ->execute();
```

```python
client.create_search_index('customers', '_yz_default')
```

```csharp
var searchIndex = new SearchIndex("customers", "_yz_default");
var rslt = client.PutSearchIndex(searchIndex);
```

```javascript
var options = {
    schemaName: '_yz_default',
    indexName: 'customers'
};
client.storeIndex(options, function (err, rslt) {
});
```

```erlang
riakc_pb_socket:create_search_index(Pid, <<"customers">>, <<"_yz_default">>).
```

```curl
curl -XPUT $RIAK_HOST/search/index/customers \
  -H 'Content-Type: application/json' \
  -d '{"schema":"_yz_default"}'
```

With our index created, we can associate our new `customers` index with
our `maps` bucket type:

```bash
riak-admin bucket-type update maps '{"props":{"search_index":"customers"}}'
```

Now we can create some maps along the lines suggested above:

```java
Namespace customersBucket = new Namespace("maps", "customers");

Location idrisElbaMap = new Location(customersBucket, "idris_elba");
MapUpdate mu = new MapUpdate()
        .update("first_name", new RegisterUpdate("Idris"))
        .update("last_name", new RegisterUpdate("Elba"))
        .update("enterprise_customer", new FlagUpdate(false))
        .update("page_visits", new CounterUpdate(10))
        .update("interests", new SetUpdate().add("acting", "being Stringer Bell"));

Location joanJettMap = new Location(customersBucket, "joan_jett");
MapUpdate mu2 = new MapUpdate()
        .update("first_name", new RegisterUpdate("Joan"))
        .update("last_name", new RegisterUpdate("Jett"))
        // Joan Jett is not an enterprise customer, so we don't need to
        // explicitly disable the "enterprise_customer" flag, as all
        // flags are disabled by default
        .update("page_visits", new CounterUpdate(25))
        .update("interests", new SetUpdate().add("loving rock and roll").add("being in the Blackhearts"));

UpdateMap update1 = new UpdateMap.Builder(idrisElbaMap, mu1).build();
UpdateMap update2 = new UpdateMap.Builder(joanJettMap, mu2).build();
client.execute(update1);
client.execute(update2);
```

```ruby
bucket = client.bucket('customers')

idris_elba = Riak::Crdt::Map.new(bucket, 'idris_elba', 'maps')

idris_elba.batch do |ie|
  ie.registers['first_name'] = 'Idris'
  ie.registers['last_name'] = 'Elba'
  ie.flags['enterprise_customer'] = true
  ie.counters['page_visits'].increment(10)
  ['acting', 'being Stringer Bell'].each do |interest|
    ie.sets['interests'].add(interest)
  end
end

joan_jett = Riak::Crdt::Map.new(bucket, 'joan_jett', 'maps')
joan_jett.batch do |jj|
  jj.registers['first_name'] = 'Joan'
  jj.registers['last_name'] = 'Jett'
  ## Joan Jett is not an enterprise customers, so we don't need to
  ## explicitly disable this flag, as all flags are disabled by default
  jj.counters['page_visits'].increment(25)
  ['loving rock and roll', 'being in the Blackhearts'].each do |interest|
    jj.sets['interests'].add(interest)
  end
end
```

```php
$counterBuilder = (new \Basho\Riak\Command\Builder\IncrementCounter($riak))
  ->withIncrement(10);

$setBuilder = (new \Basho\Riak\Command\Builder\UpdateSet($riak));
  
foreach(['acting', 'being Stringer Bell'] as $interest) {
  $setBuilder->add($interest);
}

(new \Basho\Riak\Command\Builder\UpdateMap($riak))
  ->updateRegister('first_name', 'Idres')
  ->updateRegister('last_name', 'Elba')
  ->updateFlag('enterprise_customer', true)
  ->updateSet('interests', $setBuilder)
  ->updateCounter('page_visits', $counterBuilder)
  ->buildLocation('idris_elba', 'customers', 'maps')
  ->build()
  ->execute();

$setBuilder = (new \Basho\Riak\Command\Builder\UpdateSet($riak));
  
foreach(['loving rock and roll', 'being in the Blackhearts'] as $interest) {
  $setBuilder->add($interest);
}

(new \Basho\Riak\Command\Builder\UpdateMap($riak))
  ->updateRegister('first_name', 'Joan')
  ->updateRegister('last_name', 'Jett')
  ->updateSet('interests', $setBuilder)
  ->updateCounter('page_visits', $counterBuilder->withIncrement(25))
  ->buildLocation('joan_jett', 'customers', 'maps')
  ->build()
  ->execute();
```

```python
bucket = client.bucket_type('maps').bucket('customers')

idris_elba = Map(bucket, 'idris_elba')
idris_elba.registers['first_name'].assign('Idris')
idris_elba.registers['last_name'].assign('Elba')
idris_elba.flags['enterprise_customer'].enable()
idris_elba.counters['page_visits'].increment(10)
for interest in ['acting', 'being Stringer Bell']:
    idris_elba.sets['interests'].add(interest)
idris_elba.store()

joan_jett = Map(bucket, 'joan_jett')
joan_jett.registers['first_name'].assign('Joan')
joan_jett.registers['last_name'].assign('Jett')
# Joan Jett is not an enterprise customers, so we don't need to
# explictly disable this flag, as all flags are disabled by default
idris_elba.counters['page_visits'].increment(25)
for interest in ['loving rock and roll', 'being in the Blackhearts']:
    joan_jett.sets['interests'].add(interest)
joan_jett.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Search/SearchDataTypes.cs

// Note: similar code for Joan Jett

const string firstNameRegister = "first_name";
const string lastNameRegister = "last_name";
const string enterpriseCustomerFlag = "enterprise_customer";
const string pageVisitsCounter = "page_visits";
const string interestsSet = "interests";

var idrisAdds = new[] { "acting", "being Stringer Bell" };

var mapOp = new UpdateMap.MapOperation()
    .SetRegister(firstNameRegister, "Idris")
    .SetRegister(lastNameRegister, "Elba")
    .SetFlag(enterpriseCustomerFlag, false)
    .IncrementCounter(pageVisitsCounter, 10)
    .AddToSet(interestsSet, idrisAdds);

var cmd = new UpdateMap.Builder()
    .WithBucketType("maps")
    .WithBucket("customers")
    .WithKey("idris_elba")
    .WithMapOperation(mapOp)
    .Build();

RiakResult rslt = client.Execute(cmd);
```

```javascript
var funcs = [
    function (async_cb) {
        var options = {
            bucketType: 'maps',
            bucket: 'customers',
            key: 'idris_elba'
        };

        var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
        mapOp.setRegister('first_name', 'Idris');
        mapOp.setRegister('last_name', 'Elba');
        mapOp.setFlag('enterprise_customer', false);
        mapOp.incrementCounter('page_visits', 10);
        mapOp.addToSet('interests', 'acting');
        mapOp.addToSet('interests', 'being Stringer Bell');

        options.op = mapOp;

        client.updateMap(options, function (err, rslt) {
            throwIfErr(err);
            async_cb();
        });
    },
    function (async_cb) {
        var options = {
            bucketType: 'maps',
            bucket: 'customers',
            key: 'joan_jett'
        };

        var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
        mapOp.setRegister('first_name', 'Joan');
        mapOp.setRegister('last_name', 'Jett');
        mapOp.setFlag('enterprise_customer', false);
        mapOp.incrementCounter('page_visits', 25);
        mapOp.addToSet('interests', 'loving rock and roll');
        mapOp.addToSet('interests', 'being in the Blackhearts');

        options.op = mapOp;

        client.updateMap(options, function (err, rslt) {
            throwIfErr(err);
            async_cb();
        });
    }
];

async.parallel(funcs, function (err, rslts) {
    throwIfErr(err);
});
```

### Searching Counters Within Maps

We now have two maps stored in Riak that we can query. Let's query to
see how many users have page visit counters above 15. Unlike the
counters example above, we have to specify _which_ counter we're
querying:

```java
// Using the same method explained above, just changing the query:
String query = "page_visits_counter:[15 TO *]";

// Again using the same method from above:
int numberFound = results.numResults(); // 1
```

```ruby
results = client.search('customers', 'page_visits_counter:[15 TO *]')
results['num_found']
# 1
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\FetchObjects($riak))
  ->withIndexName('customers')
  ->withQuery('page_visits_counter:[15 TO *]')
  ->build()
  ->execute();

$response->getNumFound(); // 1
```

```python
results = client.fulltext_search('customers', 'page_visits_counter:[15 TO *]')
results['num_found']
# 1
```

```csharp
var search = new RiakSearchRequest("customers", "page_visits_counter:[15 TO *]");
var rslt = client.Search(search);
```

```javascript
function search_cb(err, rslt) {
    logger.info("numFound: '%d', docs: '%s'",
        rslt.numFound, JSON.stringify(rslt.docs));

    var doc = rslt.docs[0];
    var key = doc['_yz_rk'];
    var bucket = doc['_yz_rb'];
    var bucketType = doc['_yz_rt'];
}

var searchCmd = new Riak.Commands.YZ.Search.Builder()
    .withIndexName('customers')
    .withQuery('page_visits_counter:[15 TO *]')
    .withCallback(search_cb)
    .build();

client.execute(searchCmd);
```

As expected, one of our two stored maps has a `page_visits` counter
above 15. Let's make sure that we have the right result:

```java
// Using the same method from above:
String query = "page_visits_counter:[15 TO *]";

// Again using the same method from above:
String registerValue =
  results.getAllResults().get(0).get("first_name_register").get(0); // Joan
```

```ruby
results['docs'][0]['first_name_register']
# 'Joan'
```

```php
$response->getDocs()[0]->first_name_register']; // Joan
```

```python
results['docs'][0]['first_name_register']
# u'Joan'
```

```csharp
var search = new RiakSearchRequest("customers", "page_visits_counter:[15 TO *]");
var rslt = client.Search(search);
var firstDoc = searchResult.Documents.First();
```

```javascript
var doc = rslts.docs[0];
doc.page_visits_register;
```

Success! Now we can test out searching sets.

### Searching Sets Within Maps

Each of the maps we stored thus far had an `interests` set. First, let's
see how many of our maps even _have_ sets called `interests` using a
wildcard query:

```java
// Using the same method from above:
String query = "interests_set:*";
```

```ruby
results = client.search('customers', 'interests_set:*')
# 2
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\FetchObjects($riak))
  ->withIndexName('customers')
  ->withQuery('interests_set:*')
  ->build()
  ->execute();

$response->getNumFound(); // 2
```

```python
results = client.fulltext_search('customers', 'interests_set:*')
results['num_found']
# 2
```

```csharp
var search = new RiakSearchRequest("customers", "interests_set:*");
var rslt = client.Search(search);
```

```javascript
var searchCmd = new Riak.Commands.YZ.Search.Builder()
    .withIndexName('customers')
    .withQuery('interests_set:*')
    .withCallback(search_cb)
    .build();

client.execute(searchCmd);
```

As expected, both stored maps have an `interests` set. Now let's see how
many maps have items in `interests` sets that begin with `loving`:

```java
// Using the same method from above:
String query = "interests_set:loving*";

// Again using the same method from above:
int numberFound = results.numResults(); // 1
String registerValue =
  results.getAllResults().get(0).get("first_name_register").get(0); // Joan
```

```ruby
results = client.search('customers', 'interests_set:loving*')
results['num_found'] # 1
results['docs'][0]['first_name_register'] # 'Joan'
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\FetchObjects($riak))
  ->withIndexName('customers')
  ->withQuery('interests_set:loving*')
  ->build()
  ->execute();

$response->getDocs()[0]->first_name_register']; // Joan
```

```python
results = client.fulltext_search('customers', 'interests_set:loving*')
results['num_found'] # 1
results['docs'][0]['first_name_register'] # u'Joan'
```

```csharp
var search = new RiakSearchRequest("customers", "interests_set:loving*");
var rslt = client.Search(search);
```

```javascript
var searchCmd = new Riak.Commands.YZ.Search.Builder()
    .withIndexName('customers')
    .withQuery('interests_set:loving*')
    .withCallback(search_cb)
    .build();

client.execute(searchCmd);
```

As expected, only our Joan Jett map has one item in its `interests` set
that starts with `loving`.

### Searching Maps Within Maps

Before we can try to search maps within maps, we need to actually store
some. Let's add a `alter_ego` map to both of the maps we've stored thus
far. Each person's alter ego will have a first name only.

```java
Location idrisElbaMap = new Location(customersBucket, "idris_elba");
MapUpdate alterEgoUpdateName = new MapUpdate()
        .update("name", new RegisterUpdate("John Luther"));
MapUpdate alterEgoUpdate = new MapUpdate()
        .update("alter_ego", alterEgoUpdateName);
UpdateMap addSubMap = new UpdateMap.Builder(idrisElbaMap, alterEgoUpdate);
client.execute(addSubMap);
```

```ruby
idris_elba.maps['alter_ego'].registers['name'] = 'John Luther'

joan_jett.maps['alter_ego'].registers['name'] = 'Robert Plant'
```

```php
$mapBuilder = (new \Basho\Riak\Command\Builder\UpdateMap($riak))
  ->updateRegister('name', 'John Luther')

(new \Basho\Riak\Command\Builder\UpdateMap($riak))
  ->updateMap('alter_ego', $mapBuilder)
  ->buildLocation('idris_elba', 'customers', 'maps')
  ->build()
  ->execute();

$mapBuilder->updateRegister('name', 'Robert Plant')

(new \Basho\Riak\Command\Builder\UpdateMap($riak))
  ->updateMap('alter_ego', $mapBuilder)
  ->buildLocation('joan_jett', 'customers', 'maps')
  ->build()
  ->execute();
```

```python
idris_elba.maps['alter_ego'].registers['name'].assign('John Luther')
idris_elba.store()

joan_jett.maps['alter_ego'].registers['name'].assign('Robert Plant')
joan_jett.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Search/SearchDataTypes.cs

const string nameRegister = "name";
const string alterEgoMap = "alter_ego";

var mapOp = new UpdateMap.MapOperation();
mapOp.Map(alterEgoMap).SetRegister(nameRegister, "John Luther");

var cmd = new UpdateMap.Builder()
    .WithBucketType("maps")
    .WithBucket("customers")
    .WithKey("idris_elba")
    .WithMapOperation(mapOp)
    .Build();

RiakResult rslt = client.Execute(cmd);
```

```javascript
var funcs = [
    function (async_cb) {
        var options = {
            bucketType: 'maps',
            bucket: 'customers',
            key: 'idris_elba'
        };

        var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
        var alterEgoMap = mapOp.map('alter_ego');
        alterEgoMap.setRegister('name', 'John Luther');

        options.op = mapOp;

        client.updateMap(options, function (err, rslt) {
            throwIfErr(err);
            async_cb();
        });
    },
    function (async_cb) {
        var options = {
            bucketType: 'maps',
            bucket: 'customers',
            key: 'joan_jett'
        };

        var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
        var alterEgoMap = mapOp.map('alter_ego');
        alterEgoMap.setRegister('name', 'Robert Plant');

        options.op = mapOp;

        client.updateMap(options, function (err, rslt) {
            throwIfErr(err);
            async_cb();
        });
    }
];

async.parallel(funcs, function (err, rslts) {
    throwIfErr(err);
});
```

Querying maps within maps involves construct queries that separate the
different levels of depth with a single dot. Here's an example query for
finding maps that have a `name` register embedded within an `alter_ego`
map:

```java
// Using the same method from above:
String query = "alter_ego_map.name_register:*";

// Again using the same method from above:
int numberFound = results.numResults(); // 2
```

```ruby
results = client.search('customers', 'alter_ego_map.name_register:*')
results['num_found'] # 2
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\FetchObjects($riak))
  ->withIndexName('customers')
  ->withQuery('alter_ego_map.name_register:*')
  ->build()
  ->execute();

$response->getNumFound(); // 2
```

```python
results = client.fulltext_search('customers', 'alter_ego_map.name_register:*')
results['num_found'] # 2
```

```csharp
var search = new RiakSearchRequest("customers", "alter_ego_map.name_register:*");
var rslt = client.Search(search);
```

```javascript
var searchCmd = new Riak.Commands.YZ.Search.Builder()
    .withIndexName('customers')
    .withQuery('alter_ego_map.name_register:*')
    .withCallback(search_cb)
    .build();

client.execute(searchCmd);
```

Once we know how to query embedded fields like this, we can query those
just like any other. Let's find out which maps have an `alter_ego`
sub-map that contains a `name` register that ends with `PLant`, and
display that customer's first name:

```java
// Using the same method from above:
String query = "alter_ego_map.name_register:*Plant";

// Again using the same method from above:
int numberFound = results.numResults(); // 1
String registerValue =
  results.getAllResults().get(0).get("first_name_register").get(0); // Joan
```

```ruby
results = client.search('customers', 'alter_ego_map.name_register:*Plant')
results['num_found'] # 1
results['docs'][0]['first_name_register'] # 'Joan'
```

```php
$response = (new \Basho\Riak\Command\Builder\Search\FetchObjects($riak))
  ->withIndexName('customers')
  ->withQuery('alter_ego_map.name_register:*Plant')
  ->build()
  ->execute();

$response->getNumFound(); // 1
$response->getDocs()[0]->first_name_register']; // Joan
```

```python
results = client.fulltext_search('customers', 'alter_ego_map.name_register:*Plant')
results['num_found'] # 1
results['docs'][0]['first_name_register'] # u'Joan
```

```csharp
var search = new RiakSearchRequest("customers", "alter_ego_map.name_register:*Plant");
var rslt = client.Search(search);
```

```javascript
var searchCmd = new Riak.Commands.YZ.Search.Builder()
    .withIndexName('customers')
    .withQuery('alter_ego_map.name_register:*Plant')
    .withCallback(search_cb)
    .build();

client.execute(searchCmd);
```

Success! We've now queried not just maps but also maps within maps.
