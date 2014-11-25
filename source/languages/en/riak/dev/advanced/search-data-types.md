---
title: Riak Data Types and Search
project: riak
version: 2.0.0+
document: tutorials
audience: advanced
keywords: [developers, search, crdts, data-types]
---

Although [[Riak Data Types|Data Types]] function differently from other
Riak objects in some respects, when you're using Search you can think of
them as normal Riak objects with special metadata attached (metadata
that you don't need to worry about as a user). Riak's [[counters|Data
Types#Counters]], [[sets|Data Types#Sets]], and [[maps|Data Types#Maps]]
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
indexed, which is covered in the [[examples|Using
Search#Riak-Data-Types-and-Search]] section below.

As mentioned above, there are no default schemas available for maps.
This is because maps are essentially carriers for the other Data Types.
Even when maps are embedded within other maps, all of the data that you
might wish to index and search is contained in counters, sets,
registers, and flags.

The sections immediately below provide the default schemas for each Riak
Data Type. Because you will not need to manipulate these default schemas
to search Data Types, they are provided only for reference.

### Top-level Schemas

The default schema for [[counters|Data Types#Counters]] indexes each
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

The schema for [[sets|Data Types#Sets]] indexes each element of a set as
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

For searching within [[maps|Data Types#Maps]], there are four schemas
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
knowledge. We'll create and activate a [[bucket type|Using Bucket
Types]] for [[storing counters|Using Data Types#Counters]] simply called
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

```python
client.create_search_index('scores', '_yz_default')
```

```erlang
riakc_pb_socket:create_search_index(Pid, <<"famous">>, <<"_yz_default">>, []).
```

```bash
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
bucket `players`, while the key for each counter will be the username of
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
# 2
```

```python
results = client.fulltext_search('scores', 'counter:[20 TO *]')
# This should return a dict with fields like 'num_found' and 'docs'

results['num_found']
# 1
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

```python
doc = results['docs'][0]

# The key
doc['_yz_rk'] # 'joan_rivers'

# The bucket
doc['_yz_rb'] # 'people'

# The bucket type
doc['_yz_rt'] # 'counters'
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

```python
results = client.fulltext_search('scores', 'counter:[* TO 15]')
```

Or we can see how many counters have a value of 17 exactly:

```java
// Using the same method as above, just changing the query:
String query = "counter:17";
```

```ruby
results = client.search('scores', 'counter:17')
```

```python
results = client.fulltext_search('scores', 'counter:17')
```

## Sets Example

Let's say that we're storing information about the hobbies of a group of
people in sets. We'll create and activate a [[bucket type|Using Bucket
Types]] for [[storing sets|Using Data Types#Sets]] simply called `sets`,
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

```bash
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

```python
results = client.fulltext_search('hobbies', 'set:football')
# This should return a dict with fields like 'num_found' and 'docs'
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

```python
results['num_found']
# 1
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

```python
results = client.fulltext_search('hobbies', 'set:winning')
results['num_found']
# 2
```

Just as expected, both sets we stored contain the element `winning`.

## Maps Example

This example will build on the example in the [[Using Data Types]]
tutorial. That tutorial walks you through storing CMS-style user data in
Riak [[maps|Using Data Types#Maps]], and we'd suggest that you
familiarize yourself with that tutorial first. More specifically, user
data is stored in the following fields in each users's map:

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

```python
client.create_search_index('customers', '_yz_default')
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
  jj.counters['page_visits'].increment(10)
  ['loving rock and roll', 'being in the Blackhearts'].each do |interest|
    jj.sets['interests'].add(interest)
  end
end
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

```python
results = client.fulltext_search('customers', 'page_visits_counter:[15 TO *]')
results['num_found']
# 1
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

```python
results['docs'][0]['first_name_register']
# u'Joan'
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

```python
results = client.fulltext_search('customers', 'interests_set:*')
results['num_found']
# 2
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

```python
results = client.fulltext_search('customers', 'interests_set:loving*')
results['num_found'] # 1
results['docs'][0]['first_name_register'] # u'Joan'
```

As expected, only our Joan Jett map has one item in its `interests` set
that starts with `loving`.

### Searching Maps Within Maps

Before we can try to search maps within maps, we need to actually store
some. Let's add a `alter_ego` map to both of the maps we've stored thus
far. Each person's alter ego will have a first name only.

```java
Location idrisElbaMap = new Location(customersBucket, "idris_elba");
MapUpdate alterEgoUpdate = new MapUpdate()
        .update("name", new RegisterUpdate("John Luther"));
UpdateMap addSubMap = new UpdateMap.Builder(idrisElbaMap, alterEgoUpdate);
client.execute(addSubMap);
```

```ruby
idris_elba.maps['alter_ego'].registers['name'] = 'John Luther'

joan_jett.maps['alter_ego'].registers['name'] = 'Robert Plant'
```

```python
idris_elba.maps['alter_ego'].registers['name'].assign('John Luther')
idris_elba.store()

joan_jett.maps['alter_ego'].registers['name'].assign('Robert Plant')
joan_jett.store()
```

Querying maps within maps involves construct queries that separate the
different levels of depth with a single dot. Here's an example query for
finding maps that have a `name` register embedded within an `alter_ego`
map:

```java
// Using the same method from above:
String query = "alter_ego_map.name_register:*";

// Again using the same method from above:
int numberFound = results.numResults(); // 1
```

```ruby
results = client.search('customers', 'alter_ego_map.name_register:*')
results['num_found'] # 1
```

```python
results = client.fulltext_search('customers', 'alter_ego_map.name_register:*')
results['num_found'] # 1
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

```python
results = client.fulltext_search('customers', 'alter_ego_map.name_register:*Plant')
results['num_found'] # 1
results['docs'][0]['first_name_register'] # u'Joan
```

Success! We've now queried not just maps but also maps within maps.
