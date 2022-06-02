---
title_supertext: "Developing with Riak KV"
title: "Data Types: Counters"
description: ""
project: "riak_kv"
project_version: 2.9.8
menu:
  riak_kv-2.9.8:
    name: "Counters"
    identifier: "data_types_counters"
    weight: 100
    parent: "developing_data_types"
toc: true
aliases:
  - /riak/2.9.8/dev/using/data-types/counters
  - /riak/kv/2.9.8/dev/using/data-types/counters
  - /riak/2.9.8/dev/data-modeling/data-types/counters
  - /riak/kv/2.9.8/dev/data-modeling/data-types/counters
---

Counters are a bucket-level Riak data type that can be used by themselves, associated with a bucket/key pair, or used [within a map](../maps#counters-within-maps). A counter's value can only be a positive integer, negative integer, or zero.

The examples in this section will show you how to use counters on their own.

## Set Up a Bucket Type

> If you've already created and activated a bucket type with the `datatype` parameter set to `counter`, skip to the [next section](#client-setup).

Start by creating a bucket type with the `datatype` parameter set to `counter`:

```bash
riak-admin bucket-type create counters '{"props":{"datatype":"counter"}}'
```

> **Note**
>
> The `counters` bucket type name provided above is an example and is not required to be `counters`. You are free to name bucket types whatever you like, with the exception of `default`.

After creating a bucket with a Riak data type, confirm the bucket property configuration associated with that type is correct:

```bash
riak-admin bucket-type status counters
```

This returns a list of bucket properties and their values
in the form of `property: value`.

If our `counters` bucket type has been set properly we should see the following pair in our console output:

```bash
datatype: counter
```

Once we have confirmed the bucket type is properly configured, we can activate the bucket type to be used in Riak KV:

```bash
riak-admin bucket-type activate counters
```

We can check if activation has been successful by using the same `bucket-type status` command shown above:

```bash
riak-admin bucket-type status counters
```

After creating and activating our new `counters` bucket type, we can setup our client to start using the bucket type as detailed in the next section.

## Client Setup

First, we need to direct our client to the bucket type/bucket/key
location that contains our counter.

For this example we'll use the `counters` bucket type created and activated above and a bucket called `counters`:

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

## Create a Counter

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
# The client detects the bucket type's data type and automatically
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
# being globally associated with a Riak data type. The following would
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

## Increment a Counter

Now that our client knows which bucket/key pairing to use for our
counter, `traffic_tickets` will start out at 0 by default. If we happen
to get a ticket that afternoon, we can increment the counter:

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

## Increment a Counter by More Than 1

The default value of an increment operation is 1, but you can increment
by more than 1 (but always by an integer).

Continuing with our `traffic_tickets` example, let's say we receive 5 tickets in a single day:

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

## Retrieve Counter Value

We can retrieve the value of the counter and view how many tickets have accumulated:

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

## Decrement a Counter

Counters enable you to decrement values in addition to incrementing them as seen above.

For example, let's say we hire an expert lawyer who gets one of the traffic tickets stricken from our record:

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




