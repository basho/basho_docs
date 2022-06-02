---
title_supertext: "Developing with Riak KV"
title: "Data Types: Sets"
description: ""
project: "riak_kv"
project_version: 2.9.4
menu:
  riak_kv-2.9.4:
    name: "Sets"
    identifier: "data_types_sets"
    weight: 101
    parent: "developing_data_types"
toc: true
aliases:
  - /riak/2.9.4/dev/using/data-types/sets
  - /riak/kv/2.9.4/dev/using/data-types/sets
  - /riak/2.9.4/dev/data-modeling/data-types/sets
  - /riak/kv/2.9.4/dev/data-modeling/data-types/sets
---

Sets are a bucket-level Riak data type that can be used by themselves, associated with a bucket/key pair, or used [within a map](../maps#sets-within-maps).

Sets are collections of unique binary values (such as strings). All of
the values in a set are unique.

For example, if you attempt to add the element `shovel` to a set that already contains `shovel`, the operation will be ignored by Riak KV.

## Set Up a Bucket Type

> If you've already created and activated a bucket type with `set` as the `datatype` parameter, skip to the [next section](#client-setup).

Start by creating a bucket type with the `datatype` parameter `set`:

```bash
riak-admin bucket-type create sets '{"props":{"datatype":"set"}}'
```

> **Note**
>
> The `sets` bucket type name provided above is an example and is not required to be `sets`. You are free to name bucket types whatever you like, with the exception of `default`.

After creating a bucket with a Riak data type, confirm the bucket property configuration associated with that type is correct:

```bash
riak-admin bucket-type status sets
```

This returns a list of bucket properties and their values
in the form of `property: value`.

If our `sets` bucket type has been set properly we should see the following pair in our console output:

```
datatype: set
```

Once we have confirmed the bucket type is properly configured, we can activate the bucket type to be used in Riak KV:

```bash
riak-admin bucket-type activate sets
```

We can check if activation has been successful by using the same `bucket-type status` command shown above:

```bash
riak-admin bucket-type status sets
```

After creating and activating our new `sets` bucket type, we can setup our client to start using the bucket type as detailed in the next section.

## Client Setup

Using sets involves creating a bucket/key pair to house a set and running set-specific operations on that pair.

Here is the general syntax for creating a bucket type/bucket/key
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

# Note that this differs from the URL structure for non-data type requests,
# which end in /keys/<key>
```

## Create a Set

For the following example, we will use a set to store a list of cities that we
want to visit. Let's create a Riak set stored in the key `cities` in the bucket `travel` using the `sets` bucket type created previously:

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
# being globally associated with a Riak data type. The following would
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

# The client detects the bucket type's data type and automatically
# returns the right data type for you, in this case a Riak set.
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
{"type":"set","error":"notfound"}
```

## Add to a Set

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

## Remove from a Set

Later on, we hear that Hamilton and Ottawa are nice cities to visit in
Canada, but if we visit them, we won't have time to visit Montreal, so
we need to remove it from the list.

Note that removing an element from a set is trickier than adding elements. In
order to remove an item (or multiple items), we need to first fetch the
set, which provides our client access to the set's [causal context](../../../learn/concepts/causal-context).

Once we've fetched the set, we can remove the element `Montreal` and
store the set:

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
curl http://localhost:8098/types/sets/buckets/travel/datatypes/cities

# Response
{"type":"set","value":["Montreal","Toronto"],"context":"g2wAAAABaAJtAAAADCMJ/vn7tg36AAAAAWECag=="}

curl -XPOST http://localhost:8098/types/sets/buckets/travel/datatypes/cities \
  -H "Content-Type: application/json" \
  -d '{"remove": "Montreal","add_all":["Hamilton", "Ottawa"],"context":"g2wAAAABaAJtAAAADCMJ/vn7tg36AAAAAWECag=="}'
```

## Retrieve a Set

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
{"type":"set","value":["Hamilton","Ottawa","Toronto"],"context":"g2wAAAABaAJtAAAADCMJ/vn7tg36AAAAAWEEag=="}

# You can also fetch the value of the set without the context included:
curl http://localhost:8098/types/sets/buckets/travel/datatypes/cities?include_context=false

# Response
{"type":"set","value":["Hamilton", "Ottawa", "Toronto"]}
```

## Find Set Member

Or we can see whether our set includes a specific member:

```java
// Using our "citiesSet" from above:

FetchSet fetch = new FetchSet.Builder(citiesSet)
        .build();
FetchSet.Response response = client.execute(fetch);
Set<BinaryValue> binarySet = response.getDatatype().view();

System.out.println(binarySet.contains(BinaryValue.createFromUtf8("Vancouver")));
System.out.println(binarySet.contains(BinaryValue.createFromUtf8("Ottawa")));
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

## Size of Set

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

