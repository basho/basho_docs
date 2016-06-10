---
title: "Data Types"
description: ""
project: "riak_kv"
project_version: "2.0.6"
menu:
  riak_kv-2.0.6:
    name: "Data Types"
    identifier: "developing_data_types"
    weight: 102
    parent: "developing"
toc: true
aliases:
  - /riak/2.0.6/dev/using/data-types
  - /riak/kv/2.0.6/dev/using/data-types
canonical_link: "https://docs.basho.com/riak/kv/latest/developing/data-types"
---

In versions 2.0 and greater, Riak users can make use of a variety of
Riak-specific data types inspired by research on convergent replicated
data types, more commonly known as **CRDTs**. For a more theoretical
treatment of how CRDTs work in Riak, see our [Data Types](/riak/kv/2.0.6/developing/data-types/) doc.

While Riak was originally built as a mostly data-agnostic key/value
store, Riak Data Types enable you to use Riak as a _data-aware_ system
in which you can perform a variety of transactions on five CRDT-inspired
data types: flags, registers, [counters](#counters),
[sets](#sets), and [maps](#maps).

Of those five types, counters, sets, and maps can be used as
bucket-level data types, i.e. types that you can interact with directly.
Flags and registers, however, must be embedded in maps (more on that
[below](/riak/kv/2.0.6/developing/data-types/maps).

<div class="note">
<div class="title">Note on counters in earlier versions of Riak</div>
Counters are the one CRDT that was available in Riak prior to 2.0
(introduced in version 1.4). The implementation of counters in version 2.0
has been almost completely revamped, so if you are using Riak version
2.0 or later we strongly recommend that you follow the usage
documentation here rather than documentation for the older version of
counters.
</div>

## Setting Up Buckets to Use Riak Data Types

In order to use Riak Data Types, you must first create a [bucket type](/riak/kv/2.0.6/developing/usage/bucket-types) that sets the `datatype` bucket parameter to
either `counter`, `map`, or `set`.

The following would create a separate bucket type for each of the three
bucket-level data types:

```bash
riak-admin bucket-type create maps '{"props":{"datatype":"map"}}'
riak-admin bucket-type create sets '{"props":{"datatype":"set"}}'
riak-admin bucket-type create counters '{"props":{"datatype":"counter"}}'
```

**Note**: The names `maps`, `sets`, and `counters` are _not_ reserved
terms. You are always free to name bucket types whatever you like, with
the exception of `default`.

Once you've created a Riak-Data-Type-specific bucket type, you can check
to make sure that the bucket property configuration associated with that
type is correct. This can be done through the `riak-admin` interface.

```bash
riak-admin bucket-type status maps
```

This will return a list of bucket properties and their associated values
in the form of `property: value`. If our `maps` bucket type has been set
properly, we should see the following pair in our console output:

```
datatype: map
```

If a bucket type has been properly constructed, it needs to be activated
to be usable in Riak. This can also be done using the `bucket-type`
command interface:

```bash
riak-admin bucket-type activate maps
```

To check whether activation has been successful, simply use the same
`bucket-type status` command shown above.

## Data Types and Search

Riak Data Types can be searched just like any other object, but with the
added benefit that you Data Type is indexed as a different type by Solr,
the search platform undergirding Riak Search. In our Search
documentation we offer a [full tutorial](/riak/kv/2.0.6/developing/usage/searching-data-types) as well as a variety of
[examples](/riak/kv/2.0.6/developing/usage/search/#data-types-and-search-examples), including code
samples from each of our official client libraries.

## Usage Examples

The examples below show you how to use Riak Data Types at the
application level using each of Basho's officially supported Riak
clients. All examples will use the bucket type names from above
(`counters`, `sets`, and `maps`). You're free to substitute your own
bucket type names if you wish.

## Counters

Counters are a bucket-level Riak Data Type that can be used either by
themselves, i.e. associated with a bucket/key pair, or [within a map](#maps). The examples in this section will show you
how to use counters on their own.

First, we need to point our client to the bucket type/bucket/key
location that will house our counter. We'll keep it simple and use the
`counters` bucket type created and activated above and a bucket called
`counters`.

```java
// In the Java client, a bucket/bucket type combination is specified
// using a Namespace object. To specify bucket, bucket type, and key,
// use a Location object that incorporates the Namespace object, as is
// done below.
Namespace countersBucket = new Namespace("counters", "counters");
Location location = new Location(countersBucket, "<insert_key_here>");
```

```ruby
bucket = client.bucket_type('counters').bucket('counters')
```

```php
$bucket = new \Basho\Riak\Bucket('counters', 'counters');
```

```python
bucket = client.bucket_type('counters').bucket('counters')
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

// You can either use the appropriate Options class or the Builder

// Options:
var options = new FetchCounterOptions("counters", "counters", "<insert_key_here>");

// Builder:
FetchCounter cmd = new FetchCounter.Builder()
    .WithBucketType("counters")
    .WithBucket("counters")
    .WithKey("<insert_key_here>")
    .Build();
```

```javascript
// The following can be passed as options to FetchCounter
var options = {
    bucketType: 'counters',
    bucket: 'counters',
    key: '<insert_key_here>'
};
```

```erlang
%% Buckets are simply named binaries in the Erlang client. See the
%% examples below for more information
```

```curl
curl http://localhost:8098/types/counters/buckets/counters/datatypes/<key>

# Note that this differs from the URL structure for non-Data-Type
# requests, which end in /keys/<key>
```

> **Getting started with Riak clients**
>
> If you are connecting to Riak using one of Basho's official [client libraries](/riak/kv/2.0.6/developing/client-libraries), you can find more information about getting started with your client in our [Developing with Riak KV: Getting Started](/riak/kv/2.0.6/developing/getting-started) section.

To create a counter, you need to specify a bucket/key pair to hold that
counter. Here is the general syntax for doing so:

```java
// Here, we'll use the Namespace object that we created above and
// incorporate it into a Location object that includes the key (as yet
// unspecified) for our counter

// Using the countersBucket Namespace object from above:
Location counter = new Location(countersBucket, "<key>");

// Or we can specify the Location all at once:
Location counter = new Location(new Namespace("counters", "counters"), "<key>");
```

```ruby
counter = Riak::Crdt::Counter.new(bucket, key, bucket_type)

# Or you can specify a bucket and bucket type all at once and pass that
# into the constructor
bucket = client.bucket_type(bucket_type).bucket(bucket)
counter = Riak::Crdt::Counter.new(bucket, key)
```

```php
# using the $bucket var created earlier
$location = new \Basho\Riak\Location('key', $bucket);
```

```python
# The client detects the bucket-type's Data Type and automatically
# returns the right datatype for you, in this case a counter
counter = bucket.new(key)

# This way is also acceptable:
from riak.datatypes import Counter

counter = Counter(bucket, key)
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var fetchCounterOptions = new FetchCounterOptions("counters", "counters", "<key>");
FetchCounter cmd = new FetchCounter(fetchCounterOptions);
RiakResult rslt = client.Execute(cmd);
CounterResponse response = cmd.Response;
```

```javascript
// The following can be passed as options to the *Counter methods on the
// Node.js Client object
var options = {
    bucketType: 'counters',
    bucket: 'counters',
    key: '<insert_key_here>'
};
```

```erlang
%% Counters are not encapsulated with the bucket/key in the Erlang
%% client. See the examples below for more information.
```

```curl
# This will create a counter with an initial value of 0

curl -XPOST http://localhost:8098/types/counters/buckets/<bucket>/datatypes/<key> \
  -H "Content-Type: application/json" \
  -d '{"increment": 0}'
```

Let's say that we want to create a counter called `traffic_tickets` in
our `counters` bucket to keep track of our legal misbehavior. We can
create this counter and ensure that the `counters` bucket will use our
`counters` bucket type like this:

```java
// Using the countersBucket Namespace object from above:

Location trafficTickets = new Location(countersBucket, "traffic_tickets");
```

```ruby
bucket = client.bucket_type('counters').bucket('counters')
counter = Riak::Crdt::Counter.new(bucket, 'traffic_tickets')

# Alternatively, the Ruby client enables you to set a bucket type as
# being globally associated with a Riak Data Type. The following would
# set all counter buckets to use the counters bucket type:

Riak::Crdt::DEFAULT_BUCKET_TYPES[:counter] = 'counters'

# This would enable us to create our counter without specifying a bucket type
bucket = client.bucket('counters')
counter = Riak::Crdt::Counter.new(bucket, 'traffic_tickets')
```

```php
# using the $bucket var created earlier
$location = new \Basho\Riak\Location('traffic_tickets', $bucket);
```

```python
bucket = client.bucket_type('counters').bucket('traffic_tickets')
counter = bucket.new('traffic_tickets')
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var fetchCounterOptions = new FetchCounterOptions("counters", "counters", "traffic_tickts");
FetchCounter cmd = new FetchCounter(fetchCounterOptions);
RiakResult rslt = client.Execute(cmd);
CounterResult = cmd.Result;
```

```javascript
// Using the options from above:

var options = {
    bucketType: 'counters',
    bucket: 'counters',
    key: 'traffic_tickets'
};
```

```erlang
Counter = riakc_counter:new().

%% Counters in the Erlang client are opaque data structures that collect
%% operations as you mutate them. We will associate the data structure
%% with a bucket type, bucket, and key later on.
```

```curl
curl -XPOST http://localhost:8098/types/counters/buckets/counters/datatypes/traffic_tickets \
  -H "Content-Type: application/json" \
  -d '{"increment": 0}'
```

Now that our client knows which bucket/key pairing to use for our
counter, `traffic_tickets` will start out at 0 by default. If we happen
to get a ticket that afternoon, we would need to increment the counter:

```java
// Using the "trafficTickets" Location from above:

CounterUpdate cu = new CounterUpdate(1);
UpdateCounter update = new UpdateCounter.Builder(trafficTickets, cu)
    .build();
client.execute(update);
```

```ruby
counter.increment

# This will increment the counter both on the application side and in
Riak
```

```php
(new \Basho\Riak\Command\Builder\IncrementCounter($riak))
    ->withIncrement(1)
    ->atLocation($location)
    ->build()
    ->execute();
```

```python
counter.increment()

# Updates are staged locally and have to be explicitly sent to Riak
# using the store() method.
counter.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

UpdateCounter updateCmd = new UpdateCounter.Builder(increment: 1)
    .WithBucketType("counters")
    .WithBucket("counters")
    .WithKey("traffic_tickets")
    .WithReturnBody(true)
    .Build();

RiakResult rslt = client.Execute(updateCmd);
CounterResponse response = updateCmd.Response;
// response.Value will be 1
```

```javascript
// Using the options from above:

var options = {
    bucketType: 'counters',
    bucket: 'counters',
    key: 'traffic_tickets',
    increment: 1
};
client.updateCounter(options,
    function (err, rslt) {
        if (err) {
            throw new Error(err);
        }
    });
```

```erlang
Counter1 = riakc_counter:increment(Counter).
```

```curl
curl -XPOST http://localhost:8098/types/counters/buckets/counters/datatypes/traffic_tickets \
  -H "Content-Type: application/json" \
  -d '{"increment": 1}'
```

The default value of an increment operation is 1, but you can increment
by more than one if you'd like (but always by an integer). Let's say
that we decide to spend an afternoon flaunting traffic laws and manage
to rack up five tickets:

```java
// Using the "trafficTickets" Location from above:
CounterUpdate cu = new CounterUpdate(5);
UpdateCounter update = new UpdateCounter.Builder(trafficTickets, cu)
    .build();
client.execute(update);
```

```ruby
counter.increment(5)
```

```php
(new \Basho\Riak\Command\Builder\IncrementCounter($riak))
    ->withIncrement(5)
    ->atLocation($location)
    ->build()
    ->execute();
```

```python
counter.increment(5)
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var builder = new UpdateCounter.Builder(5)
    .WithBucketType("counters")
    .WithBucket("counters")
    .WithKey("traffic_tickets")
    .WithReturnBody(true);

UpdateCounter updateCmd = builder.Build();

rslt = client.Execute(updateCmd);
CounterResponse response = updateCmd.Response;
// response.Value is 5 more than before

// To decrement:
// Modify the builder's increment, then construct a new command
builder.WithIncrement(-5);
updateCmd = builder.Build();

rslt = client.Execute(updateCmd);
CheckResult(rslt);

response = updateCmd.Response;
// response.Value is 5 less than before
```

```javascript
var options = {
    bucketType: 'counters',
    bucket: 'counters',
    key: 'traffic_tickets',
    increment: 5
};
client.updateCounter(options,
    function (err, rslt) {
        if (err) {
            throw new Error(err);
        }
    });
```

```erlang
Counter2 = riakc_counter:increment(5, Counter1).
```

```curl
curl -XPOST http://localhost:8098/types/counters/buckets/counters/datatypes/traffic_tickets \
  -H "Content-Type: application/json" \
  -d '{"increment": 5}'
```

If we're curious about how many tickets we have accumulated, we can
simply retrieve the value of the counter at any time:

```java
// Using the "trafficTickets" Location from above:
FetchCounter fetch = new FetchCounter.Builder(trafficTickets)
    .build();
FetchCounter.Response response = client.execute(fetch);
RiakCounter counter = response.getDatatype();
Long ticketsCount = counter.view();
```

```ruby
counter.value
# Output will always be an integer
```

```php
$trafficTickets = (new \Basho\Riak\Command\Builder\FetchCounter($riak))
    ->atLocation($location)
    ->build()
    ->execute()
    ->getCounter();

$trafficTickets->getData(); # returns an integer
```

```python
counter.dirty_value

# The value fetched from Riak is always immutable, whereas the "dirty
# value" takes into account local modifications that have not been
# sent to the server. For example, whereas the call above would return
# 6, the call below will return 0' since we started with an empty
# counter:

counter.value

# To fetch the value stored on the server, use the call below. Note
# that this will clear any changes to the counter that have not yet been
# sent to Riak
counter.reload()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var fetchCounterOptions = new FetchCounterOptions("counters", "counters", "traffic_tickts");
FetchCounter cmd = new FetchCounter(fetchCounterOptions);
RiakResult rslt = client.Execute(cmd);
CounterResponse response = cmd.Response;
// response.Value has the counter value
```

```javascript
var options = {
    bucketType: 'counters',
    bucket: 'counters',
    key: 'traffic_tickets'
};
client.fetchCounter(options,
    function (err, rslt) {
        if (err) {
            throw new Error(err);
        }

        if (rslt.notFound) {
            logger.error("bt: %s, b: %s, k: %s, counter: NOT FOUND",
                options.bucketType, options.bucket, options.key);
        } else {
            logger.info("bt: %s, b: %s, k: %s, counter: %d",
                options.bucketType, options.bucket, options.key,
                rslt.counterValue);
        }
    }
);
```

```erlang
riakc_counter:dirty_value(Counter2).

%% The value fetched from Riak is always immutable, whereas the "dirty
%% value" takes into account local modifications that have not been
%% sent to the server. For example, whereas the call above would return
%% '6', the call below will return '0' since we started with an empty
%% counter:

riakc_counter:value(Counter2).

%% To fetch the value stored on the server, use the call below:

{ok, CounterX} = riakc_pb_socket:fetch_type(Pid,
                                            {<<"counters">>, <<"counters">>},
                                            <<"traffic_tickets">>).
```

```curl
curl http://localhost:8098/types/counters/buckets/counters/datatypes/traffic_tickets

# Response:
{"type":"counter", "value": <value>}
```

For a counter to be useful, you need to be able to decrement it in
addition to incrementing it. Riak counters enable you to do precisely
that. Let's say that we hire an expert lawyer who manages to get one of
our traffic tickets stricken from our record:

```java
// Using the "trafficTickets" Location from above:
CounterUpdate cu = new CounterUpdate(-1);
UpdateCounter update = new UpdateCounter.Builder(trafficTickets, cu)
        .build();
client.execute(update);
```

```ruby
counter.decrement

# Just like incrementing, you can also decrement by more than one, e.g.:
counter.decrement(3)
```

```php
(new \Basho\Riak\Command\Builder\IncrementCounter($riak))
    ->withIncrement(-3)
    ->atLocation($location)
    ->build()
    ->execute();
```

```python
counter.decrement()

# Just like incrementing, you can also decrement by more than one, e.g.:
counter.decrement(3)
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var updateCmd = new UpdateCounter.Builder(-3)
    .WithBucketType("counters")
    .WithBucket("counters")
    .WithKey("traffic_tickets")
    .Build();

rslt = client.Execute(updateCmd);
response = updateCmd.Response;
// response.Value is three less than before
```

```javascript
var options = {
    bucketType: 'counters',
    bucket: 'counter',
    key: 'traffic_tickets',
    increment: -1
};

// As with incrementing, you can also decrement by more than one, e.g.:
var options = {
    bucketType: 'counters',
    bucket: 'counter',
    key: 'traffic_tickets',
    increment: -3
};
```

```erlang
Counter3 = riakc_counter:decrement(Counter2).

%% As with incrementing, you can also decrement by more than one:

Counter4 = riakc_counter:decrement(3, Counter3).

%% At some point, we'll want to send our local updates to the server
%% so they get recorded and are visible to others. Extract the update
%% using the to_op/1 function, then pass it to
%% riakc_pb_socket:update_type/4,5.

riakc_pb_socket:update_type(Pid, {<<"counters">>,<<"counters">>},
                            <<"traffic_tickets">>,
                            riakc_counter:to_op(Counter4)).
```

```curl
curl -XPOST http://localhost:8098/types/counters/buckets/counters/datatypes/traffic_tickets \
  -H "Content-Type: application/json" \
  -d '{"decrement": 3}'
```

## Sets

As with counters (and maps, as shown below), using sets involves setting
up a bucket/key pair to house a set and running set-specific operations
on that pair.

Here is the general syntax for setting up a bucket type/bucket/key
combination to handle a set:

```java
// In the Java client, a bucket/bucket type combination is specified
// using a Namespace object. To specify bucket, bucket type, and key,
// use a Location object that incorporates the Namespace object, as is
// done below.

Location set =
  new Location(new Namespace("<bucket_type>", "<bucket>"), "<key>");
```

```ruby
# Note: both the Riak Ruby Client and Ruby the language have a class
# called Set. Make sure that you refer to the Ruby version as ::Set and
# the Riak client version as Riak::Crdt::Set

bucket = client.bucket_type('bucket_type_name').bucket('bucket_name')
set = Riak::Crdt::Set.new(bucket, key)
```

```php
$location = new \Basho\Riak\Location('key', new \Basho\Riak\Bucket('bucket_name', 'bucket_type'));
```

```python
# Note: The Python standard library `collections` module has an abstract
# base class called Set, which the Riak Client version subclasses as
# `riak.datatypes.Set`. These classes are not directly interchangeable.
# In addition to the base methods, `riak.datatypes.Set` also
# implements the `add` and `discard` methods from
# `collections.MutableSet`, but does not implement the rest of its
# API. Be careful when importing, or simply use the instances returned
# by `RiakBucket.get()` and `RiakBucket.new()` instead of directly
# importing the class.

set = bucket.new(key)

# or

from riak.datatypes import Set
set = Set(bucket, key)
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

// As with counters, with the Riak .NET Client you interact with sets
// by building an Options object or using a Builder
var builder = new FetchSet.Builder()
    .WithBucketType("sets")
    .WithBucket("travel")
    .WithKey("cities");

// NB: builder.Options will only be set after Build() is called.
FetchSet fetchSetCommand = builder.Build();

FetchSetOptions options = new FetchSetOptions("sets", "travel", "cities");

// These two options objects are equal
Assert.AreEqual(options, builder.Options);
```

```javascript
// As with counters, with the Riak Node.js Client you interact with sets on the
// basis of the set's location in Riak, as specified by an options object.
// Below is an example:
var options = {
    bucketType: 'sets',
    bucket: 'travel',
    key: 'cities'
};
```

```erlang
%% Like counters, sets are not encapsulated in a
%% bucket/key in the Erlang client. See below for more
%% information.
```

```curl
curl http://localhost:8098/types/<bucket_type>/buckets/<bucket>/datatypes/<key>

# Note that this differs from the URL structure for non-Data Type requests,
# which end in /keys/<key>
```

Let's say that we want to use a set to store a list of cities that we
want to visit. Let's create a Riak set stored in the key `cities` in the
bucket `travel` (using the `sets` bucket type we created in the previous
section):

```java
// In the Java client, you specify the location of Data Types
// before you perform operations on them:

Location citiesSet =
  new Location(new Namespace("sets", "travel"), "cities");
```

```ruby
travel = client.bucket_type('sets').bucket('travel')
cities_set = Riak::Crdt::Set.new(travel, 'cities')

# Alternatively, the Ruby client enables you to set a bucket type as
# being globally associated with a Riak Data Type. The following would
# set all set buckets to use the sets bucket type:

Riak::Crdt::DEFAULT_BUCKET_TYPES[:set] = 'sets'

# This would enable us to create our set without specifying a bucket
# type:
travel = client.bucket('travel')
cities_set = Riak::Crdt::Set.new(travel, 'cities')
```

```php
$location = new \Basho\Riak\Location('cities', 'travel', 'sets');
```

```python
travel = client.bucket_type('sets').bucket('travel')

# The client detects the bucket type's Data Type and automatically
# returns the right Data Type for you, in this case a Riak set.
cities_set = travel.new('cities')

# You can also create a reference to a set explicitly:
from riak.datatypes import Set

cities_set = Set(travel, 'cities')
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

// Now we'll create a Builder object for the set with which we want to
// interact:
var builder = new FetchSet.Builder()
    .WithBucketType("sets")
    .WithBucket("travel")
    .WithKey("cities");
```

```javascript
// Now we'll create a options object for the set with which we want to
// interact:
var options = {
    bucketType: 'sets',
    bucket: 'travel',
    key: 'cities'
};
```

```erlang
CitiesSet = riakc_set:new().

%% Sets in the Erlang client are opaque data structures that
%% collect operations as you mutate them. We will associate the data
%% structure with a bucket type, bucket, and key later on.
```

```curl
# You cannot create an empty set through the HTTP interface. Sets can
# only be created when an element is added to them, as in the examples
# below.
```

Upon creation, our set is empty. We can verify that it is empty at any
time:

```java
// Using our "cities" Location from above:

FetchSet fetch = new FetchSet.Builder(citiesSet)
    .build();
FetchSet.Response response = client.execute(fetch);
RiakSet set = response.getDatatype();
boolean isEmpty = set.viewAsSet().isEmpty();
```

```ruby
cities_set.empty?
```

```php
# use $location from earlier
$set = (new \Basho\Riak\Command\Builder\FetchSet($riak))
    ->atLocation($location)
    ->build()
    ->execute()
    ->getSet();

count($set->getData());
```

```python
len(cities_set) == 0
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var builder = new FetchSet.Builder()
    .WithBucketType("sets")
    .WithBucket("travel")
    .WithKey("cities");

FetchSet fetchSetCommand = builder.Build();
RiakResult rslt = client.Execute(fetchSetCommand);
SetResponse response = fetchSetCommand.Response;
// response.Value will be null
```

```javascript
var options = {
    bucketType: 'sets',
    bucket: 'travel',
    key: 'cities'
};
client.fetchSet(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }

    if (rslt.notFound) {
        logger.info("set 'cities' is not found!");
    }
});
```

```erlang
riakc_set:size(CitiesSet) == 0.

%% Query functions like size/1, is_element/2, and fold/3 operate over
%% the immutable value fetched from the server. In the case of a new
%% set that was not fetched, this is an empty collection, so the size
%% is 0.
```

```curl
curl http://localhost:8098/types/sets/buckets/travel/datatypes/cities

# Response
not found
```

But let's say that we read a travel brochure saying that Toronto and
Montreal are nice places to go. Let's add them to our `cities` set:

```java
// Using our "cities" Location from above:

SetUpdate su = new SetUpdate()
        .add("Toronto")
        .add("Montreal");
UpdateSet update = new UpdateSet.Builder(citiesSet, su)
        .build();
client.execute(update);
```

```ruby
cities_set.add('Toronto')
cities_set.add('Montreal')
```

```php
# use $location from earlier
$response = (new \Basho\Riak\Command\Builder\UpdateSet($riak))
    ->add('Toronto')
    ->add('Montreal')
    ->atLocation($location)
    ->withParameter('returnbody', 'true')
    ->build()
    ->execute();
```

```python
cities_set.add('Toronto')
cities_set.add('Montreal')
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var adds = new[] { "Toronto", "Montreal" };

var builder = new UpdateSet.Builder()
    .WithBucketType("sets")
    .WithBucket("travel")
    .WithKey("cities")
    .WithAdditions(adds);

UpdateSet cmd = builder.Build();
RiakResult rslt = client.Execute(cmd);
SetResponse response = cmd.Response;

Assert.Contains("Toronto", response.AsStrings.ToArray());
Assert.Contains("Montreal", response.AsStrings.ToArray());
```

```javascript
var options = {
    bucketType: 'sets',
    bucket: 'travel',
    key: 'cities'
};
var cmd = new Riak.Commands.CRDT.UpdateSet.Builder()
    .withBucketType(options.bucketType)
    .withBucket(options.bucket)
    .withKey(options.key)
    .withAdditions(['Toronto', 'Montreal'])
    .withCallback(
        function (err, rslt) {
            if (err) {
                throw new Error(err);
            }
        }
    )
    .build();
client.execute(cmd);
```

```erlang
CitiesSet1 = riakc_set:add_element(<<"Toronto">>, CitiesSet),
CitiesSet2 = riakc_set:add_element(<<"Montreal">>, CitiesSet1).
```

```curl
curl -XPOST http://localhost:8098/types/sets/buckets/travel/datatypes/cities \
  -H "Content-Type: application/json" \
  -d '{"add_all":["Toronto", "Montreal"]}'
```

Later on, we hear that Hamilton and Ottawa are nice cities to visit in
Canada, but if we visit them, we won't have time to visit Montreal, so
we need to remove it from the list. It needs to be noted here that
removing an element from a set is a bit trickier than adding elements. In
order to remove an item (or multiple items), we need to first fetch the
set, which provides our client access to the set's [causal context](/riak/kv/2.0.6/learn/concepts/causal-context).
Once we've fetched the set, we can remove the element `Montreal` and
store the set.

```java
// Using our "citiesSet" Location from above

// First, we get a response
FetchSet fetch = new FetchSet.Builder(citiesSet).build();
FetchSet.Response response = client.execute(fetch);

// Then we can fetch the set's causal context
Context ctx = response.getContext();

// Now we build a SetUpdate operation
SetUpdate su = new SetUpdate()
        .remove("Montreal")
        .add("Hamilton")
        .add("Ottawa");

// Finally, we update the set, specifying the context
UpdateSet update = new UpdateSet.Builder(citiesSet, su)
        .withContext(ctx)
        .build();
client.execute(update);

// More information on using causal context with the Java client can be
// found at the bottom of this document
```

```ruby
cities_set.remove('Montreal')
cities_set.add('Hamilton')
cities_set.add('Ottawa')
```

```php
# use $location & $response from earlier
(new \Basho\Riak\Command\Builder\UpdateSet($riak))
    ->add('Hamilton')
    ->add('Ottawa')
    ->remove('Montreal')
    ->atLocation($location)
    ->withContext($response->getSet()->getContext())
    ->build()
    ->execute();
```

```python
cities_set.discard('Montreal')
cities_set.add('Hamilton')
cities_set.add('Ottawa')
cities_set.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var removes = new[] { "Montreal" };
var adds = new[] { "Hamilton", "Ottawa" };

// Note:
// using the builder from above
// using the Context member from the above response
builder
    .WithAdditions(adds)
    .WithRemovals(removes)
    .WithContext(response.Context);

UpdateSet cmd = builder.Build();
RiakResult rslt = client.Execute(cmd);
SetResponse response = cmd.Response;

// using System.Linq
var responseStrings = response.AsStrings.ToArray();

Assert.Contains("Toronto", responseStrings);
Assert.Contains("Hamilton", responseStrings);
Assert.Contains("Ottawa", responseStrings);
```

```javascript
var options = {
    bucketType: 'sets',
    bucket: 'travel',
    key: 'cities'
};
client.fetchSet(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }

    // NB: clone package https://www.npmjs.com/package/clone
    var update_opts = clone(options);
    update_opts.context = rslt.context;
    update_opts.additions = ['Hamilton', 'Ottawa'];
    update_opts.removals = ['Montreal', 'Ottawa'];

    client.updateSet(update_opts, function (err, rslt) {
        if (err) {
            throw new Error(err);
        }
    });
});
```

```erlang
CitiesSet3 = riakc_set:del_element(<<"Montreal">>, CitiesSet2),
CitiesSet4 = riakc_set:add_element(<<"Hamilton">>, CitiesSet3),
CitiesSet5 = riakc_set:add_element(<<"Ottawa">>, CitiesSet4).
```

```curl
curl -XPOST http://localhost:8098/types/sets/buckets/travel/datatypes/cities \
  -H "Content-Type: application/json" \
  -d '{"remove": "Montreal","add_all":["Hamilton", "Ottawa"]}'
```

Now, we can check on which cities are currently in our set:

```java
// Using our "cities" Location from above:

FetchSet fetch = new FetchSet.Builder(citiesSet)
        .build();
FetchSet.Response response = client.execute(fetch);
Set<BinaryValue> binarySet = response.getDatatype().view();
for (BinaryValue city : binarySet) {
  System.out.println(city.toStringUtf8());
}
```

```ruby
cities_set.members

#<Set: {"Hamilton", "Ottawa", "Toronto"}>
```

```php
# use $location from earlier
$set = (new \Basho\Riak\Command\Builder\FetchSet($riak))
    ->atLocation($location)
    ->build()
    ->execute()
    ->getSet();

var_dump($set->getData());
```

```python
cities_set.dirty_value

# The value fetched from Riak is always immutable, whereas the "dirty
# value" takes into account local modifications that have not been
# sent to the server. For example, where the call above would return
# frozenset(['Toronto', 'Hamilton', 'Ottawa']), the call below would
# return frozenset([]).

cities_set.value

# To fetch the value stored on the server, use the call below. Note
# that this will clear any unsent additions or deletions.
cities_set.reload()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

foreach (var value in setResponse.AsStrings)
{
    Console.WriteLine("Cities Set Value: {0}", value);
}

// Output:
// Cities Set Value: Hamilton
// Cities Set Value: Ottawa
// Cities Set Value: Toronto
```

```javascript
var options = {
    bucketType: 'sets',
    bucket: 'travel',
    key: 'cities'
};
client.fetchSet(options, function(err, rslt) {
    if (err) {
        throw new Error(err);
    }

    logger.info("cities set values: '%s'",
        rslt.values.join(', '));
});

// Output:
// info: cities set values: 'Hamilton, Ottawa, Toronto'
```

```erlang
riakc_set:dirty_value(CitiesSet5).

%% The value fetched from Riak is always immutable, whereas the "dirty
%% value" takes into account local modifications that have not been
%% sent to the server. For example, where the call above would return
%% [<<"Hamilton">>, <<"Ottawa">>, <<"Toronto">>], the call below would
%% return []. These are essentially ordsets:

riakc_set:value(CitiesSet5).

%% To fetch the value stored on the server, use the call below:

{ok, SetX} = riakc_pb_socket:fetch_type(Pid,
                                        {<<"sets">>,<<"travel">>},
                                         <<"cities">>).
```

```curl
curl http://localhost:8098/types/sets/buckets/travel/datatypes/cities

# Response

{"type":"set","value":["Hamilton", "Ottawa", "Toronto"],"context":"SwGDUAAAAER4ActgymFgYGDMYMoFUhxHgzZyBzMfsU9kykISZg/JL8rPK8lHEkKoZMzKAgDwJA+e"}

# You can also fetch the value of the set without the context included:
curl http://localhost:8098/types/sets/buckets/travel/datatypes/cities?include_context=false

# Response
{"type":"set","value":["Hamilton", "Ottawa", "Toronto"]}
```

Or we can see whether our set includes a specific member:

```java
// Using our "citiesSet" from above:

System.out.println(citiesSet.contains(("Vancouver"));
System.out.println(citiesSet.contains("Ottawa"));
```

```ruby
cities_set.include? 'Vancouver'
# false

cities_set.include? 'Ottawa'
# true
```

```php
in_array('Vancouver', $set->getData()); # false

in_array('Ottawa', $set->getData()); # true
```

```python
'Vancouver' in cities_set
# False

'Ottawa' in cities_set
# True
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

using System.Linq;

bool includesVancouver = response.AsStrings.Any(v => v == "Vancouver");
bool includesOttawa = response.AsStrings.Any(v => v == "Ottawa");
```

```javascript
// Use standard javascript array method indexOf()

var cities_set = result.values;
cities_set.indexOf('Vancouver'); // if present, index is >= 0
cities_set.indexOf('Ottawa'); // if present, index is >= 0
```

```erlang
%% At this point, Set5 is the most "recent" set from the standpoint
%% of our application.

riakc_set:is_element(<<"Vancouver">>, CitiesSet5).
riakc_set:is_element(<<"Ottawa">>, CitiesSet5).
```

```curl
# With the HTTP interface, this can be determined from the output of
# a fetch command like the one displayed in the example above
```

We can also determine the size of the set:

```java
// Using our "citiesSet" from above:

int numberOfCities = citiesSet.size();
```

```ruby
cities_set.members.length
```

```php
count($set->getData());
```

```python
len(cities_set)
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

using System.Linq;

// Note: this enumerates the IEnumerable
setResponse.Values.Count();
```

```javascript
// Use standard javascript array property length

var cities_set_size = result.values.length;
```

```erlang
riakc_set:size(CitiesSet5).
```

```curl
# With the HTTP interface, this can be determined from the output of
# a fetch command like the one displayed in the example above
```

## Maps

The map is in many ways the richest of the Riak Data Types because all
of the other Data Types can be embedded within them, _including maps
themselves_, to create arbitrarily complex custom Data Types out of a
few basic building blocks.

The semantics of dealing with counters, sets, and maps within maps are
usually very similar to working with those types at the bucket level,
and so usage is usually very intuitive.

The general syntax for creating a Riak map is directly analogous to the
syntax for creating other data types:

```java
// In the Java client, a bucket/bucket type combination is specified
// using a Namespace object. To specify bucket, bucket type, and key,
// use a Location object that incorporates the Namespace object, as is
// done below.

Location map =
  new Location(new Namespace("<bucket_type>", "<bucket>"), "<key>");
```

```ruby
bucket = client.bucket_type('bucket_type_name').bucket('bucket_name')
map = Riak::Crdt::Map.new(bucket, key)
```

```php
$location = new \Basho\Riak\Location('key', 'bucket', 'bucket_type');
```

```python
# The client detects the bucket-type's datatype and automatically
# returns the right datatype for you, in this case a Map.
map = bucket.new(key)

# This way is also acceptable:
from riak.datatypes import Map
map = Map(bucket, key)
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var builder = new UpdateMap.Builder()
    .WithBucketType("<bucket_type>")
    .WithBucket("<bucket>")
    .WithKey("<key>");
```

```javascript
// Options to pass to the various map methods
var options = {
    bucketType: '<bucket_type>',
    bucket: '<bucket>',
    key: '<key>'
};
```

```erlang
%% Maps in the Erlang client are opaque data structures that
%% collect operations as you mutate them. We will associate the data
%% structure with a bucket type, bucket, and key later on.
```

```curl
curl http://localhost:8098/types/<bucket_type>/buckets/<bucket>/datatypes/<key>

# Note that this differs from the URL structure for non-Data Type requests,
# which end in /keys/<key>
```

Let's say that we want to use Riak to store information about our
company's customers. We'll use the bucket `customers` to do so. Each
customer's data will be contained in its own key in the `customers`
bucket. Let's create a map for the user Ahmed (`ahmed_info`) (we'll use the
`maps` bucket type from above):

```java
// In the Java client, you specify the location of Data Types
// before you perform operations on them:

Location ahmedMap =
  new Location(new Namespace("maps", "customers"), "ahmed_info");
```

```ruby
customers = client.bucket_type('maps').bucket('customers')
map = Riak::Crdt::Map.new(customers, 'ahmed_info')

# Alternatively, the Ruby client enables you to set a bucket type as being
# globally associated with a Riak Data Type. The following would set all
# map buckets to use the maps bucket type:

Riak::Crdt::DEFAULT_BUCKET_TYPES[:map] = 'maps'

# This would enable us to create our map without specifying a bucket type:

customers = client.bucket('customers')
map = Riak::Crdt::Map.new(customers, 'ahmed_info')
```

```php
$location = new \Basho\Riak\Location('ahmed_info', 'customers', 'maps');
```

```python
customers = client.bucket_type('map_bucket').bucket('customers')
map = customers.net('ahmed_info')
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var builder = new UpdateMap.Builder()
    .WithBucketType("maps")
    .WithBucket("customers")
    .WithKey("ahmed_info");
```

```javascript
var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info'
};
```

```erlang
Map = riakc_map:new().

%% Maps in the Erlang client are opaque data structures that
%% collect operations as you mutate them. We will associate the data
%% structure with a bucket type, bucket, and key later on.
```

```curl
# You cannot create an empty map through the HTTP interface. Maps can only
# be created when a field is added to them, as in the examples below.
```

### Registers and Flags

Registers and flags cannot be used on their own in Riak. You cannot use
a bucket/key pair as a register or flag directly.

#### Registers Within Maps

The first piece of info we want to store in our map is Ahmed's name and
phone number, both of which are best stored as registers:

```java
// Using our "ahmedMap" location from above:

RegisterUpdate ru1 = new RegisterUpdate("Ahmed");
RegisterUpdate ru2 = new RegisterUpdate("5551234567");
MapUpdate mu = new MapUpdate()
        .update("first_name", ru1)
        .update("phone_number", ru2);
UpdateMap update = new UpdateMap.Builder(ahmedMap, mu)
          .build();
client.execute(update);
```

```ruby
# The Ruby client enables you to batch operations together if you're
# performing them on one Data Type.

map.batch do |m|
  m.registers['first_name'] = 'Ahmed'
  m.registers['phone_number'] = '5551234567'
end

# Integers need to be stored as strings and then converted back when
# the data is retrieved. The following would work as well:
map.registers['phone_number'] = 5551234567.to_s
```

```php
(new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateRegister('first_name', 'Ahmed')
    ->updateRegister('phone_number', '5551234567')
    ->atLocation($location)
    ->build()
    ->execute();
```

```python
map.registers['first_name'].assign('Ahmed')
map.registers['phone_number'].assign('5551234567')

# Integers need to be stored as strings and then converted back when the
# data is retrieved. The following would work as well:
map.registers['phone_number'].assign(str(5551234567))

map.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var builder = new UpdateMap.Builder()
    .WithBucketType("maps")
    .WithBucket("customers")
    .WithKey("ahmed_info");

var mapOperation = new UpdateMap.MapOperation();

// Ahmed's first name
mapOperation.SetRegister("first_name", "Ahmed");

// Ahmed's phone number
mapOperation.SetRegister("phone_number", "5551234567");

builder.WithMapOperation(mapOperation);

UpdateMap cmd = builder.Build();
RiakResult rslt = client.Execute(cmd);
MapResponse response = cmd.Response;
PrintMap(response.Value);
// Output as JSON:
// Map: {"Counters":{},"Sets":{},"Registers":{"first_name":"Ahmed","phone_number":"5551234567"},"Flags":{},"Maps":{}}
```

```javascript
var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
mapOp.setRegister('first_name', new Buffer('Ahmed'));
mapOp.setRegister('phone_number', new Buffer('5551234567'));

var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info',
    op: mapOp
};

client.updateMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
Map1 = riakc_map:update({<<"first_name">>, register},
                        fun(R) -> riakc_register:set(<<"Ahmed">>, R) end,
                        Map),
Map2 = riakc_map:update({<<"phone_number">>, register},
                        fun(R) -> riakc_register:set(<<"5551234567">>, R) end,
                        Map1).
```

```curl
# Updates can be performed all at once. The following will create two new
# registers in the map and also set the value of those registers to the
# desired values

curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "first_name_register": "Ahmed",
      "phone_number_register": "5551234567"
    }
  }'
```

This will work even though registers `first_name` and `phone_number` did
not previously exist, as Riak will create those registers for you.

#### Flags Within Maps

Now let's say that we add an Enterprise plan to our pricing model. We'll
create an `enterprise_customer` flag to track whether Ahmed has signed
up for the new plan. He hasn't yet, so we'll set it to `false`:

```java
// Using our "ahmedMap" location from above:

MapUpdate mu = new MapUpdate()
        .update("enterprise_customer", new FlagUpdate(false));
UpdateMap update = new UpdateMap.Builder(ahmedMap, mu)
        .build();
client.execute(update);
```

```ruby
map.flags['enterprise_customer'] = false
```

```php
(new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateFlag('enterprise_customer', false)
    ->atLocation($location)
    ->build()
    ->execute();
```

```python
map.flags['enterprise_customer'].disable()
map.store()
```


```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

// Using our builder from above:

mapOperation = new UpdateMap.MapOperation();
mapOperation.SetFlag("enterprise_customer", false);

builder.WithMapOperation(mapOperation);
cmd = builder.Build();
rslt = client.Execute(cmd);

response = cmd.Response;

// response.Value as JSON:
// Map: {"Counters":{},"Sets":{},
         "Registers":{"first_name":"Ahmed","phone_number":"5551234567"},
         "Flags":{"enterprise_customer":false},"Maps":{}}
```

```javascript
var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
mapOp.setFlag('enterprise_customer', false);

var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info',
    op: mapOp
};

client.updateMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
Map4 = riakc_map:update({<<"enterprise_customer">>, flag},
                        fun(F) -> riakc_flag:disable(F) end,
                        Map3).
```

```curl
curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "enterprise_customer_flag": "disable"
    }
  }'
```

We can retrieve the value of that flag at any time:

```java
// Using our "ahmedMap" location from above:

FetchMap fetch = new FetchMap.Builder(ahmedMap).build();
FetchMap.Response response = client.execute(fetch);
RiakMap map = response.getDatatype();
System.out.println(map.getFlag("enterprise_customer").view());
```

```ruby
map.flags['enterprise_customer']

# false
```

```php
$map = (new \Basho\Riak\Command\Builder\FetchMap($riak))
    ->atLocation($location)
    ->build()
    ->execute()
    ->getMap();

echo $map->getFlag('enterprise_customer'); // false
```

```python
map.reload().flags['enterprise_customer'].value
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

Map ahmedMap = response.Value;
ahmedMap.Flags["enterprise_customer"]
```

```javascript
var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info'
};

client.fetchMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }

    console.log("fetched map: %s", JSON.stringify(rslt));
});
```

```erlang
%% The value fetched from Riak is always immutable, whereas the "dirty
%% value" takes into account local modifications that have not been
%% sent to the server.

riakc_map:dirty_value(Map4).
```

```curl
curl http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info
```

#### Counters Within Maps

We also want to know how many times Ahmed has visited our website. We'll
use a `page_visits` counter for that and run the following operation
when Ahmed visits our page for the first time:

```java
// Using our "ahmedMap" location from above:

MapUpdate mu = new MapUpdate()
        .update("page_visits", cu);
UpdateMap update = new UpdateMap.Builder(ahmedMap, new CounterUpdate(1))
        .build();
client.execute(update);
```

```ruby
map.counters['page_visits'].increment

# This operation may return false even if successful
```

```php
$updateCounter = (new \Basho\Riak\Command\Builder\IncrementCounter($riak))
    ->withIncrement(1);

(new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateCounter('page_visits', $updateCounter)
    ->atLocation($location)
    ->build()
    ->execute();
```

```python
map.counters['page_visits'].increment()
map.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var mapOperation = new UpdateMap.MapOperation();
mapOperation.IncrementCounter("page_visits", 1);

builder.WithMapOperation(mapOperation);
UpdateMap cmd = builder.Build();
RiakResult rslt = client.Execute(cmd);

MapResponse response = cmd.Response;
// Map: {"Counters":{"page_visits":3},
         "Sets":{},
         "Registers":{"first_name":"Ahmed","phone_number":"5551234567"},
         "Flags":{"enterprise_customer":false},
         "Maps":{}}
```

```javascript
var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
mapOp.incrementCounter('page_visits', 1);

var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info',
    op: mapOp
};

client.updateMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
Map3 = riakc_map:update({<<"page_visits">>, counter},
                        fun(C) -> riakc_counter:increment(1, C) end,
                        Map2).
```

```curl
# The following will create a new counter and increment it by 1

curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "page_visits_counter": 1
    }
  }'
```

Even though the `page_visits` counter did not exist previously, the
above operation will create it (with a default starting point of 0) and
the increment operation will bump the counter up to 1.

#### Sets Within Maps

We'd also like to know what Ahmed's interests are so that we can better
design a user experience for him. Through his purchasing decisions, we
find out that Ahmed likes robots, opera, and motorcycles. We'll store
that information in a set inside of our map:

```java
// Using our "ahmedMap" location from above:

SetUpdate su = new SetUpdate()
        .add("robots")
        .add("opera")
        .add("motorcycles");
MapUpdate mu = new MapUpdate()
        .update("interests", su);
UpdateMap update = new UpdateMap.Builder(ahmedMap, mu)
        .build();
client.execute(update);
```

```ruby
map.batch do |m|
  %{ robots opera motorcycles }.each do |interest|
    m.sets['interests'].add(interest)
  end
end
```

```php
$updateSet = (new \Basho\Riak\Command\Builder\UpdateSet($riak))
    ->add('robots')
    ->add('opera')
    ->add('motorcycles');

(new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateSet('interests', $updateSet)
    ->atLocation($location)
    ->build()
    ->execute();
```

```python
for interest in ['robots', 'opera', 'motorcycles']:
    map.sets['interests'].add(interest)
map.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var interestsAdds = new[] { "robots", "opera", "motorcycles" };

var mapOperation = new UpdateMap.MapOperation();
mapOperation.AddToSet("interests", interestsAdds);

builder.WithMapOperation(mapOperation);
UpdateMap cmd = builder.Build();
RiakResult rslt = client.Execute(cmd);
MapResponse response = cmd.Response;

// Map: {"Counters":{"page_visits":3},
         "Sets":{"interests":["motorcycles","opera","robots"]},
         "Registers":{"first_name":"Ahmed","phone_number":"5551234567"},
         "Flags":{"enterprise_customer":false},
         "Maps":{}}
```

```javascript
var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
mapOp.addToSet('interests', 'robots');
mapOp.addToSet('interests', 'opera');
mapOp.addToSet('interests', 'motorcycles');

var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info',
    op: mapOp
};

client.updateMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
Map4 = riakc_map:update({<<"interests">>, set},
                        fun(S) -> riakc_set:add_element(<<"robots">>, S) end, Map3),
Map5 = riakc_map:update({<<"interests">>, set},
                        fun(S) -> riakc_set:add_element(<<"opera">>, S) end,
                        Map4),
Map6 = riakc_map:update({<<"interests">>, set},
                        fun(S) -> riakc_set:add_element(<<"motorcycles">>, S) end,
                        Map4).
```

```curl
curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "interests_set": {
        "add_all": [
          "robots",
          "opera",
          "motorcycles"
        ]
      }
    }
  }'
```

We can then verify that the `interests` set includes these three
interests:

```java
// Using our "ahmedMap" location from above:

FetchMap fetch = new FetchMap.Builder(ahmedMap)
    .build();
FetchMap.Response response = client.execute(fetch);
RiakMap map = response.getDatatype();
RiakSet interestSet = map.getSet("interests");
Set<BinaryValue> interests = interestSet.view();
System.out.println(interests.contains(BinaryValue.create("robots")));

// Checking for "opera" and "motorcycles" works the same way
```

```ruby
map.batch do |m|
  %w{ robots opera motorcycles }.each do |interest|
    m.sets['interests'].include? interest
  end
end

# This will return three Boolean values
```

```php
$map = (new \Basho\Riak\Command\Builder\FetchMap($riak))
    ->atLocation($location)
    ->build()
    ->execute()
    ->getMap();

$sets = $map->getSet('interests');
var_dump($sets->getData());
```

```python
reloaded_map = map.reload()
for interest in ['robots', 'opera', 'motorcycles']:
    interest in reloaded_map.sets['interests'].value
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

Map ahmedMap = response.Value;

// All of the following return true:
ahmedMap.Sets.GetValue("interests").Contains("robots");
ahmedMap.Sets.GetValue("interests").Contains("opera");
ahmedMap.Sets.GetValue("interests").Contains("motorcycles");
```

```javascript
var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info'
};

client.fetchMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }

    assert(rslt.map.sets['interests'].indexOf('robots') !== -1);
});
```

```erlang
riakc_map:dirty_value(Map6).
```

```curl
curl http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info?include_context=false
```

We learn from a recent purchasing decision that Ahmed actually doesn't
seem to like opera. He's much more keen on indie pop. Let's change the
`interests` set to reflect that:

```java
// Using our "ahmedMap" location from above:

SetUpdate su = new SetUpdate()
        .remove("opera")
        .add("indie pop");
MapUpdate mu = new MapUpdate()
        .update("interests", su);
UpdateMap update = new UpdateMap.Builder(ahmedMap, mu)
        .build();
client.execute(update);
```

```ruby
map.batch do |m|
  m.sets['interests'].remove('opera')
  m.sets['interests'].add('indie pop')
end
```

```php
$updateSet = (new \Basho\Riak\Command\Builder\UpdateSet($riak))
    ->add('indie pop')
    ->remove('opera');

(new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateSet('interests', $updateSet)
    ->atLocation($location)
    ->withContext($map->getContext())
    ->build()
    ->execute();
```

```python
map.sets['interests'].discard('opera')
map.sets['interests'].add('indie pop')
map.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var mapOperation = new UpdateMap.MapOperation();
mapOperation.AddToSet("interests", "indie pop");
mapOperation.RemoveFromSet("interests", "opera");

builder
    .WithMapOperation(mapOperation)
    .WithContext(response.Context);

UpdateMap cmd = builder.Build();
RiakResult rslt = client.Execute(cmd);

MapResponse response = cmd.Response;
Map ahmedMap = response.Value;

// This is false
ahmedMap.Sets.GetValue("interests").Contains("opera");

// These are true
ahmedMap.Sets.GetValue("interests").Contains("indie pop");
ahmedMap.Sets.GetValue("interests").Contains("robots");
ahmedMap.Sets.GetValue("interests").Contains("motorcycles");
```

```javascript
var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info'
};

client.fetchMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }

    var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
    mapOp.removeFromSet('interests', 'opera');
    mapOp.addToSet('interests', 'indie pop');

    options.context = rslt.context;
    options.op = mapOp;

    client.updateMap(options, function (err, rslt) {
        if (err) {
            throw new Error(err);
        }
    });
});
```

```erlang
Map7 = riakc_map:update({<<"interests">>, set},
                        fun(S) -> riakc_set:del_element(<<"opera">>, S) end, Map6),
Map8 = riakc_map:update({<<"interests">>, set},
                        fun(S) -> riakc_set:add_element(<<"indie pop">>, S) end,
                        Map7).
```

```curl
curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "interests_set": {
        "remove": "opera",
        "add": "indie pop"
      }
    }
  }
  '
```

#### Maps Within Maps (Within Maps?)

We've stored a wide of variety of information---of a wide variety of
types---within the `ahmed_info` map thus far, but we have yet to explore
recursively storing maps within maps (which can be nested as deeply as
you wish).

Our company is doing well and we have lots of useful information about
Ahmed, but now we want to store information about Ahmed's contacts as
well. We'll start with storing some information about Ahmed's colleague
Annika inside of a map called `annika_info`.

First, we'll store Annika's first name, last name, and phone number in
registers:

```java
// Using our "ahmedMap" location from above:

RegisterUpdate ru1 = new RegisterUpdate("Annika");
RegisterUpdate ru2 = new RegisterUpdate("Weiss");
RegisterUpdate ru3 = new RegisterUpdate("5559876543");

MapUpdate annikaUpdate = new MapUpdate()
        .update("first_name", ru1)
        .update("last_name", ru2)
        .update("phone_number", ru3);
MapUpdate ahmedUpdate = new MapUpdate()
        .update("annika_info", annikaUpdate);
UpdateMap update = new UpdateMap.Builder(ahmedMap, ahmedUpdate)
        .build();
client.execute(update);
```

```ruby
map.maps['annika_info'].batch do |m|
  m.registers['first_name'] = 'Annika'
  m.registers['last_name'] = 'Weiss'
  m.registers['phone_number'] = 5559876543.to_s
end
```

```php
$annikaMap = (new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateRegister('first_name', 'Annika')
    ->updateRegister('last_name', 'Weiss')
    ->updateRegister('phone_number', '5559876543');

$response = (new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateMap('annika_info', $annikaMap)
    ->atLocation($location)
    ->withParameter('returnbody', 'true')
    ->build()
    ->execute();
```

```python
map.maps['annika_info'].registers['first_name'].assign('Annika')
map.maps['annika_info'].registers['last_name'].assign('Weiss')
map.maps['annika_info'].registers['phone_number'].assign(str(5559876543))
map.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var mapOperation = new UpdateMap.MapOperation();

var annikaInfoOperation = mapOperation.Map("annika_info");
annikaInfoOperation.SetRegister("first_name", "Annika");
annikaInfoOperation.SetRegister("last_name", "Weiss");
annikaInfoOperation.SetRegister("phone_number", "5559876543");

builder.WithMapOperation(mapOperation);
UpdateMap cmd = builder.Build();
client.Execute(cmd);
```

```javascript
var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info'
};

var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
mapOp.map('annika_info')
    .setRegister('first_name', 'Annika')
    .setRegister('last_name', 'Weiss')
    .setRegister('phone_number', '5559876543');

options.op = mapOp;

client.updateMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
Map12 = riakc_map:update(
    {<<"annika_info">>, map},
    fun(M) -> riakc_map:update(
        {<<"first_name">>, register},
        fun(R) -> riakc_register:set(<<"Annika">>, R) end, M) end,
    Map11),
Map13 = riakc_map:update(
    {<<"annika_info">>, map},
    fun(M) -> riakc_map:update(
        {<<"last_name">>, register},
        fun(R) -> riakc_register:set(<<"Weiss">>, R) end, M) end,
    Map12),
Map14 = riakc_map:update(
    {<<"annika_info">>, map},
    fun(M) -> riakc_map:update(
        {<<"phone_number">>, register},
        fun(R) -> riakc_register:set(<<"5559876543">>, R) end, M) end,
    Map13).
```

```curl
curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "annika_info_map": {
        "update": {
          "first_name_register": "Annika",
          "last_name_register": "Weiss",
          "phone_number_register": "5559876543"
        }
      }
    }
  }
  '
```

The value of a register in a map can be obtained without a special
method:

```java
// Using our "ahmedMap" location from above:

FetchMap fetch = new FetchMap.Builder(ahmedMap).build();
FetchMap.Response response = client.execute(fetch);
String annikaFirstName = response.getDatatype()
        .getMap("annika_info")
        .getRegister("first_name")
        .view()
        .toString();
```

```ruby
map.maps['annika_info'].registers['first_name']

# "Annika"
```

```php
# with param 'returnbody' = 'true', we can fetch the map from our last response
$map->getMap();

echo $map->getMap('annika_info')->getRegister('first_name'); // Annika
```

```python
map.reload().maps['annika_info'].registers['first_name'].value
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

ahmedMap = response.Value;
ahmedMap.Maps["annika_info"].Registers.GetValue("first_name");
```

```javascript
var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info'
};

client.fetchMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }

    var annikaFirstName =
        rslt.map.maps['annika_info'].registers['first_name'].toString('utf8');
});
```

```erlang
riakc_map:dirty_value(Map14).
```

```curl
# Specific values for fields inside of maps (or maps within maps, for that
# matter), cannot be obtained directly through the HTTP interface.
```

Registers can also be removed:

```java
// This example uses our "ahmedMap" location from above. Operations that
// remove fields from maps require that you first fetch the opaque context
// attached to the map and then include the context in the update operation:

FetchMap fetch = new FetchMap.Builder(ahmedMap)
    .build();
FetchMap.Response response = client.execute(fetch);
Context ctx = response.getContext();
MapUpdate annikaUpdate = new MapUpdate()
        .removeRegister("first_name");
MapUpdate ahmedUpdate = new MapUpdate()
        .update("annika_info", annikaUpdate);
UpdateMap update = new UpdateMap.Builder(ahmedMap, ahmedUpdate)
        .withContext(ctx)
        .build();
client.execute(update);
```

```ruby
map.maps['annika_info'].registers.remove('first_name')
```

```php
$annikaMap = (new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->removeRegister('first_name');

(new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateMap('annika_info', $annikaMap)
    ->atLocation($location)
    ->withContext($map->getContext())
    ->build()
    ->execute();
```

```python
del map.maps['annika_info'].registers['first_name']
map.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var mapOperation = new UpdateMap.MapOperation();
mapOperation.Map("annika_info").RemoveRegister("first_name");

// Note: using Context from last response
builder
    .WithMapOperation(mapOperation)
    .WithContext(response.Context);

UpdateMap cmd = builder.Build();
client.Execute(cmd);
```

```javascript
var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info'
};

client.fetchMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }

    var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
    mapOp.map('annika_info').removeRegister('first_name');

    var options = {
        bucketType: 'maps',
        bucket: 'customers',
        key: 'ahmed_info',
        op: mapOp,
        context: rslt.context,
    };

    client.updateMap(options, function (err, rslt) {
        if (err) {
            throw new Error(err);
        }
    });
```

```erlang
Map15 = riakc_map:update({<<"annika_info">>, map},
                         fun(M) -> riakc_map:erase({<<"phone_number">>, register}, M) end,
                         Map14).
```

```curl
curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "annika_info_map": {
        "remove": "phone_number_register"
      }
    }
  }
  '
```

Now, we'll store whether Annika is subscribed to a variety of plans
within the company as well:

```java
// Using our "ahmedMap" location from above:

FetchMap fetch = new FetchMap.Builder(ahmedMap).build();
FetchMap.Response response = client.execute(fetch);
Context ctx = response.getContext();
MapUpdate annikaUpdate = new MapUpdate()
        .update("enterprise_plan", new FlagUpdate((false))
        .update("family_plan", new FlagUpdate(false))
        .update("free_plan", new FlagUpdate(true));
MapUpdate ahmedUpdate = new MapUpdate()
        .update("annika_info", annikaUpdate);
UpdateMap update = new UpdateMap.Builder(ahmedMap, ahmedUpdate)
        .withContext(ctx)
        .build();
client.execute(update);
```

```ruby
map.maps['annika_info'].batch do |m|
  m.flags['enterprise_plan'] = false
  m.flags['family_plan'] = false
  m.flags['free_plan'] = true
end
```

```php
$annikaMap = (new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateFlag('enterprise_plan', false)
    ->updateFlag('family_plan', false)
    ->updateFlag('free_plan', true);

$response = (new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateMap('annika_info', $annikaMap)
    ->atLocation($location)
    ->withParameter('returnbody', 'true')
    ->build()
    ->execute();
```

```python
map.maps['annika_info'].flags['enterprise_plan'].disable()
map.maps['annika_info'].flags['family_plan'].disable()
map.maps['annika_info'].flags['free_plan'].enable()
map.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var mapOperation = new UpdateMap.MapOperation();
mapOperation.Map("annika_info")
    .SetFlag("enterprise_plan", false)
    .SetFlag("family_plan", false)
    .SetFlag("free_plan", true);

builder.WithMapOperation(mapOperation);

MapUpdate cmd = builder.Build();
client.Execute(cmd);
```

```javascript
var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info'
};

client.fetchMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }

    var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
    var annika_map = mapOp.map('annika_info');
    annika_map.setFlag('enterprise_plan', false);
    annika_map.setFlag('family_plan', false);
    annika_map.setFlag('free_plan', true);

    var options = {
        bucketType: 'maps',
        bucket: 'customers',
        key: 'ahmed_info',
        op: mapOp,
        context: rslt.context,
    };

    client.updateMap(options, function (err, rslt) {
        if (err) {
            throw new Error(err);
        }
    });
});
```

```erlang
Map16 = riakc_map:update(
    {<<"annika_info">>, map},
    fun(M) -> riakc_map:update(
        {<<"enterprise_plan">>, flag},
        fun(F) -> riakc_flag:disable(F) end,
        M) end,
    Map15),
Map17 = riakc_map:update(
    {<<"annika_info">>, map},
    fun(M) -> riakc_map:update(
        {<<"family_plan">>, flag},
        fun(F) -> riakc_flag:disable(F) end,
        M) end,
    Map16),
Map18 = riakc_map:update(
    {<<"annika_info">>, map},
    fun(M) -> riakc_map:update(
        {<<"free_plan">>, flag},
        fun(F) -> riakc_flag:enable(F) end,
        M) end,
    Map17).
```

```curl
curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "annika_info_map": {
        "update": {
          "enterprise_plan_flag": "disable",
          "family_plan_flag": "disable",
          "free_plan_flag": "enable"
        }
      }
    }
  }
  '
```

The value of a flag can be retrieved at any time:

```java
// Using our "ahmedMap" location from above:

FetchMap fetch = new FetchMap.Builder(ahmedMap).build();
FetchMap.Response response = client.execute(fetch);
boolean enterprisePlan = response.getDatatype()
        .getMap("annika_info")
        .getFlag("enterprise_plan")
        .view();
```

```ruby
map.maps['annika_info'].flags['enterprise_plan']

# false
```

```php
# with param 'returnbody' = 'true', we can fetch the map from our last response
$map->getMap();

echo $map->getMap('annika_info')->getFlag('enterprise_plan'); // false
```

```python
map.reload().maps['annika_info'].flags['enterprise_plan'].value
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

ahmedMap = response.Value;
ahmedMap.Maps["annika_info"].Flags["enterprise_plan"];
```

```javascript
var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info'
};

client.fetchMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }

    var enterprisePlan =
        rslt.map.maps.annika_info.flags.enterprise_plan;
});
```

```erlang
riakc_map:dirty_value(Map18).
```

```curl
# Specific values for fields inside of maps (or maps within maps, for that
# matter), cannot be obtained directly through the HTTP interface.
```

It's also important to track the number of purchases that Annika has
made with our company. Annika just made her first widget purchase:

```java
// Using our "ahmedMap" location from above:

MapUpdate annikaUpdate = new MapUpdate()
        .update("widget_purchases", new CounterUpdate(1));
MapUpdate ahmedUpdate = new MapUpdate()
        .update("annika_info", annikaUpdate);
UpdateMap update = new UpdateMap.Builder(ahmedMap, ahmedUpdate)
        .build();
client.execute(update);
```

```ruby
map.maps['annika_info'].counters['widget_purchases'].increment
```

```php
$updateCounter = (new \Basho\Riak\Command\Builder\IncrementCounter($riak))
    ->withIncrement(1);

$annikaMap = (new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateCounter('widget_purchases', $updateCounter);

(new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateMap('annika_info', $annikaMap)
    ->atLocation($location)
    ->build()
    ->execute();
```

```python
map.maps['annika_info'].counters['widget_purchases'].increment()
map.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var mapOperation = new UpdateMap.MapOperation();
mapOperation.Map("annika_info").IncrementCounter("widget_purchases", 1);

builder.WithMapOperation(mapOperation);

UpdateMap cmd = builder.Build();
client.Execute(cmd);
```

```javascript
var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
mapOp.map('annika_info').incrementCounter('widget_purchases', 1);

var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info',
    op: mapOp
};

client.updateMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
Map19 = riakc_map:update(
    {<<"annika_info">>, map},
    fun(M) -> riakc_map:update(
        {<<"widget_purchases">>, counter},
        fun(C) -> riakc_counter:increment(1, C) end,
        M) end,
    Map18).
```

```curl
curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "annika_info_map": {
        "update": {
          "widget_purchases_counter": 1
        }
      }
    }
  }
  '
```

Now let's store Annika's interests in a set:

```java
// Using our "ahmedMap" location from above:

SetUpdate su = new SetUpdate().add("tango dancing");
MapUpdate annikaUpdate = new MapUpdate()
        .update("interests", su);
MapUpdate ahmedUpdate = new MapUpdate()
        .update("annika_info", annikaUpdate);
UpdateMap update = new UpdateMap.Builder(ahmedMap, ahmedUpdate)
        .build();
client.execute(update);
```

```ruby
map.maps['annika_info'].sets['interests'].add('tango dancing')
```

```php
$updateSet = (new \Basho\Riak\Command\Builder\UpdateSet($riak))
    ->add('tango dancing');

$annikaMap = (new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateSet('interests', $updateSet);

$response = (new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateMap('annika_info', $annikaMap)
    ->atLocation($location)
    ->withParameter('returnbody', 'true')
    ->build()
    ->execute();
```

```python
map.maps['annika_info'].sets['interests'].add('tango dancing')
map.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var mapOperation = new UpdateMap.MapOperation();
mapOperation.Map("annika_info").AddToSet("interests", "tango dancing");

builder.WithMapOperation(mapOperation);
client.Execute(builder.Build());
```

```javascript
var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
var annika_map = mapOp.map('annika_info');
annika_map.addToSet('interests', 'tango dancing');

var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info',
    op: mapOp
};

client.updateMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
Map20 = riakc_map:update(
    {<<"annika_info">>, map},
    fun(M) -> riakc_map:update(
        {<<"interests">>, set},
        fun(S) -> riakc_set:add_element(<<"tango dancing">>, S) end,
        M) end,
    Map19).
```

```curl
curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "annika_info_map": {
        "update": {
          "interests_set": {
            "add": "tango dancing"
          }
        }
      }
    }
  }
  '
```

We can remove that interest in just the way that we would expect:

```java
// Using our "ahmedMap" location from above:

SetUpdate su = new SetUpdate().remove("tango dancing");
MapUpdate annikaUpdate = new MapUpdate()
        .update("interests", su);
MapUpdate ahmedUpdate = new MapUpdate()
        .update("annika_info", annikaUpdate);
UpdateMap update = new UpdateMap.Builder(ahmedMap, ahmedUpdate)
        .withUpdate(ahmedUpdate)
        .build();
client.execute(update);
```

```ruby
map.maps['annika_info'].sets['interests'].remove('tango dancing')
```

```php
$updateSet = (new \Basho\Riak\Command\Builder\UpdateSet($riak))
    ->remove('tango dancing');

$annikaMap = (new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateSet('interests', $updateSet);

(new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateMap('annika_info', $annikaMap)
    ->atLocation($location)
    ->withContext($response->getMap()->getContext())
    ->build()
    ->execute();
```

```python
map.maps['annika_info'].sets['interests'].discard('tango dancing')
map.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var mapOperation = new UpdateMap.MapOperation();
mapOperation.Map("annika_info").RemoveFromSet("interests", "tango dancing");

// Note: using Context from previous response
builder
    .WithMapOperation(mapOperation)
    .WithContext(response.Context);
client.Execute(builder.Build());
```

```javascript
var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info'
};

client.fetchMap(options, function (err, rslt) {
    var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
    var annika_map = mapOp.map('annika_info');
    annika_map.removeFromSet('interests', 'tango dancing');

    options = {
        bucketType: 'maps',
        bucket: 'customers',
        key: 'ahmed_info',
        op: mapOp,
        context: rslt.context
    };

    client.updateMap(options, function (err, rslt) {
        if (err) {
            throw new Error(err);
        }
    });
});
```

```erlang
Map21 = riakc_map:update(
    {<<"annika_info">>, map},
    fun(M) -> riakc_map:update(
        {<<"interests">>, set},
        fun(S) -> riakc_set:del_element(<<"tango dancing">>, S) end,
        M) end,
    Map20).
```

```curl
curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "annika_info_map": {
        "interests_set": {
          "remove": "tango dancing"
        }
      }
    }
  }
  '
```

If we wanted to add store information about one of Annika's specific
purchases, we could do so within a map:

```java
// Using our "ahmedMap" location from above:

MapUpdate purchaseUpdate = new MapUpdate()
        .update("first_purchase", new FlagUpdate(true)
        .update("amount", new RegisterUpdate("1271"))
        .update("items", new SetUpdate().add("large widget"));
MapUpdate annikaUpdate = new MapUpdate()
        .update("purchase", purchaseUpdate);
MapUpdate ahmedUpdate = new MapUpdate()
        .update("annika_info", annikaUpdate);
UpdateMap update = new UpdateMap.Builder(ahmedMap, ahmedUpdate)
        .withUpdate(ahmedUpdate)
        .build();
client.execute(update);
```

```ruby
map.maps['annika_info'].maps['purchase'].batch do |m|
  m.flags['first_purchase'] = true
  m.register['amount'] = 1271.to_s
  m.sets['items'].add('large widget')
end
```

```php
$updateSet = (new \Basho\Riak\Command\Builder\UpdateSet($riak))
    ->add('large widget');

$purchaseMap = (new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateFlag('first_purchase', true)
    ->updateRegister('amount', '1271')
    ->updateSet('items', $updateSet);

$annikaMap = (new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateMap('purchase', $purchaseMap);

$response = (new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateMap('annika_info', $annikaMap)
    ->atLocation($location)
    ->withParameter('returnbody', 'true')
    ->build()
    ->execute();
```

```python
map.maps['annika_info'].maps['purchase'].flags['first_purchase'].enable()
map.maps['annika_info'].maps['purchase'].register['amount'].assign(str(1271))
map.maps['annika_info'].maps['purchase'].sets['items'].add('large widget')
# and so on
map.store()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var mapOperation = new UpdateMap.MapOperation();
mapOperation.Map("annika_info").Map("purchase")
    .SetFlag("first_purchase", true)
    .SetRegister("amount", "1271")
    .AddToSet("items", "large widget");

builder.WithMapOperation(mapOperation);
client.Execute(builder.Build());
```

```javascript
var mapOp = new Riak.Commands.CRDT.UpdateMap.MapOperation();
var annika_map = mapOp.map('annika_info');
var annika_purchase_map = annika_map.map('purchase');
annika_purchase_map.setFlag('first_purchase', true);
annika_purchase_map.setRegister('amount', '1271');
annika_purchase_map.addToSet('items', 'large widget');

var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info',
    op: mapOp
};

client.updateMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
Map22 = riakc_map:update(
    {<<"annika_info">>, map},
    fun(M) -> riakc_map:update(
        {<<"purchase">>, map},
        fun(M) -> riakc_map:update(
            {<<"first_purchase">>, flag},
            fun(R) -> riakc_flag:enable(R) end,
        M) end,
    M) end,
    Map21
).
```

```curl
curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "annika_info_map": {
        "update": {
          "purchase_map": {
            "update": {
              "first_purchase_flag": "enable",
              "amount_register": "1271",
              "items_set": {
                "add": "large widget"
              }
            }
          }
        }
      }
    }
  }
  '
```

## Data Types and Context

When performing normal key/value updates in Riak, we advise that you use
[causal context](/riak/kv/2.0.6/learn/concepts/causal-context), which enables Riak to make intelligent decisions
behind the scenes about which object values should be considered more
causally recent than others in cases of conflict. In some of the
examples above, you saw references to **context** metadata included with
each Data Type stored in Riak.

Data Type contexts are similar to [causal context](/riak/kv/2.0.6/learn/concepts/causal-context) in that they are
opaque (i.e. not readable by humans) and also perform a similar function
to that of causal context, i.e. they inform Riak which version of the
Data Type a client is attempting to modify. This information is required
by Riak when making decisions about convergence.

In the example below, we'll fetch the context from the user data map we
created for Ahmed, just to see what it looks like:

```java
// Using the "ahmedMap" Location from above:

FetchMap fetch = new FetchMap.Builder(ahmedMap).build();
FetchMap.Response response = client.execute(fetch);
Context ctx = response.getContext();
System.out.prinntln(ctx.getValue().toString())

// An indecipherable string of Unicode characters should then appear
```

```ruby
bucket = client.bucket('users')
ahmed_map = Riak::Crdt::Map.new(bucket, 'ahmed_info', 'maps')
ahmed_map.instance_variable_get(:@context)

# => "\x83l\x00\x00\x00\x01h\x02m\x00\x00\x00\b#\t\xFE\xF9S\x95\xBD3a\x01j"
```

```php
$map = (new \Basho\Riak\Command\Builder\FetchMap($riak))
    ->atLocation($location)
    ->build()
    ->execute()
    ->getMap();

echo $map->getContext(); // g2wAAAACaAJtAAAACLQFHUkv4m2IYQdoAm0AAAAIxVKxCy5pjMdhCWo=
```

```python
bucket = client.bucket_type('maps').bucket('users')
ahmed_map = Map(bucket, 'ahmed_info')
ahmed_map.context

# g2wAAAABaAJtAAAACCMJ/vlTlb0zYQFq
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

// Note: using a previous UpdateMap or FetchMap result
Console.WriteLine(format: "Context: {0}", args: Convert.ToBase64String(result.Context));

// Output:
// Context: g2wAAAACaAJtAAAACLQFHUkv4m2IYQdoAm0AAAAIxVKxCy5pjMdhCWo=
```

```javascript
var options = {
    bucketType: 'maps',
    bucket: 'customers',
    key: 'ahmed_info'
};

client.fetchMap(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }

    logger.info("context: '%s'", rslt.context.toString('base64'));
});

// Output:
// context: 'g2wAAAACaAJtAAAACLQFHUmjDf4EYTBoAm0AAAAIxVKxC6F1L2dhSWo='
```

```erlang
%% You cannot fetch a Data Type's context directly using the Erlang
%% client. This is actually quite all right, as the client automatically
%% manages contexts when making updates.
```

<div class="note">
<div class="title">Context with the Ruby, Python, and Erlang clients</div>
In the Ruby, Python, and Erlang clients, you will not need to manually
handle context when making Data Type updates. The clients will do it all
for you. The one exception amongst the official clients is the Java
client. We'll explain how to use Data Type contexts with the Java client
directly below.
</div>

#### Context with the Java and PHP Clients

With the Java and PHP clients, you'll need to manually fetch and return Data Type
contexts for the following operations:

* Disabling a flag within a map
* Removing an item from a set (whether the set is on its own or within a
  map)
* Removing a field from a map

Without context, these operations simply will not succeed due to the
convergence logic driving Riak Data Types. The example below shows you
how to fetch a Data Type's context and then pass it back to Riak. More
specifically, we'll remove the `paid_account` flag from the map:

```java
// This example uses our "ahmedMap" location from above:

FetchMap fetch = new FetchMap.Builder(ahmedMap)
    .build();
FetchMap.Response response = client.execute(fetch);
Context ctx = response.getContext();
MapUpdate removePaidAccountField = new MapUpdate()
        .removeFlag("paid_account");
UpdateMap update = new UpdateMap.Builder(ahmedMap, removePaidAccountField)
        .withContext(ctx)
        .build();
client.execute(update);
```


```php
$map = (new \Basho\Riak\Command\Builder\FetchMap($riak))
    ->atLocation($location)
    ->build()
    ->execute()
    ->getMap();

$updateSet = (new \Basho\Riak\Command\Builder\UpdateSet($riak))
    ->remove('opera');

(new \Basho\Riak\Command\Builder\UpdateMap($riak))
    ->updateSet('interests', $updateSet)
    ->atLocation($location)
    ->withContext($map->getContext())
    ->build()
    ->execute();
```

