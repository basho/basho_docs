---
title_supertext: "Developing with Riak KV"
title: "Data Types: GSets"
description: ""
project: "riak_kv"
project_version: "2.5.0"
menu:
  riak_kv-2.5.0:
    name: "GSets"
    identifier: "data_types_gsets"
    weight: 101
    parent: "developing_data_types"
toc: true
aliases:
  - /riak/2.5.0/dev/using/data-types/gsets
  - /riak/kv/2.5.0/dev/using/data-types/gsets
  - /riak/2.5.0/dev/data-modeling/data-types/gsets
  - /riak/kv/2.5.0/dev/data-modeling/data-types/gsets
---

GSets are a bucket-level Riak data type that can be used by themselves or associated with a bucket/key pair.

GSets are collections of unique binary values (such as strings). All of the values in a gset are unique.

For example, if you attempt to add the element `shovel` to a gset that already contains `shovel`, the operation will be ignored by Riak KV.

## Set Up a Bucket Type

> If you've already created and activated a bucket type with `gset` as the `datatype` parameter, skip to the [next section](#client-setup).

Start by creating a bucket type with the `datatype` parameter `gset`:

```bash
riak-admin bucket-type create gsets '{"props":{"datatype":"gset"}}'
```

> **Note**
>
> The `gsets` bucket type name provided above is an example and is not required to be `gsets`. You are free to name bucket types whatever you like, with the exception of `default`.

After creating a bucket with a Riak data type, confirm the bucket property configuration associated with that type is correct:

```bash
riak-admin bucket-type status gsets
```

This returns a list of bucket properties and their values
in the form of `property: value`.

If our `gsets` bucket type has been gset properly we should see the following pair in our console output:

```
datatype: gset
```

Once we have confirmed the bucket type is properly configured, we can activate the bucket type to be used in Riak KV:

```bash
riak-admin bucket-type activate gsets
```

We can check if activation has been successful by using the same `bucket-type status` command shown above:

```bash
riak-admin bucket-type status gsets
```

After creating and activating our new `gsets` bucket type, we can setup our client to start using the bucket type as detailed in the next section.

## Client Setup

Using gsets involves creating a bucket/key pair to house a gset and running gset-specific operations on that pair.

Here is the general syntax for creating a bucket type/bucket/key
combination to handle a gset:

```java
// In the Java client, a bucket/bucket type combination is specified
// using a Namespace object. To specify bucket, bucket type, and key,
// use a Location object that incorporates the Namespace object, as is
// done below.

Location gset =
  new Location(new Namespace("<bucket_type>", "<bucket>"), "<key>");
```

```ruby
bucket = client.bucket_type('bucket_type_name').bucket('bucket_name')
gset = Riak::Crdt::GSet.new(bucket, key)
```

```php
$location = new \Basho\Riak\Location('key', new \Basho\Riak\Bucket('bucket_name', 'bucket_type'));
```

```python
gset = bucket.new(key)

# or

from riak.datatypes import GSet
gset = GSet(bucket, key)
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

// As with counters, with the Riak .NET Client you interact with gsets
// by building an Options object or using a Builder
var builder = new FetchGSet.Builder()
    .WithBucketType("gsets")
    .WithBucket("travel")
    .WithKey("cities");

// NB: builder.Options will only be set after Build() is called.
FetchGSet fetchGSetCommand = builder.Build();

FetchGSetOptions options = new FetchGSetOptions("gsets", "travel", "cities");

// These two options objects are equal
Assert.AreEqual(options, builder.Options);
```

```javascript
// As with counters, with the Riak Node.js Client you interact with gsets on the
// basis of the gset's location in Riak, as specified by an options object.
// Below is an example:
var options = {
    bucketType: 'gsets',
    bucket: 'travel',
    key: 'cities'
};
```

```erlang
%% Like counters, gsets are not encapsulated in a
%% bucket/key in the Erlang client. See below for more
%% information.
```

```curl
curl http://localhost:8098/types/<bucket_type>/buckets/<bucket>/datatypes/<key>

# Note that this differs from the URL structure for non-data type requests,
# which end in /keys/<key>
```

## Create a GSet

For the following example, we will use a gset to store a list of cities that we
want to visit. Let's create a Riak gset stored in the key `cities` in the bucket `travel` using the `gsets` bucket type created previously:

```java
// In the Java client, you specify the location of Data Types
// before you perform operations on them:

Location citiesGSet =
  new Location(new Namespace("gsets", "travel"), "cities");
```

```ruby
travel = client.bucket_type('gsets').bucket('travel')
cities_gset = Riak::Crdt::GSet.new(travel, 'cities')

# Alternatively, the Ruby client enables you to set a bucket type as
# being globally associated with a Riak data type. The following would
# set all gset buckets to use the gsets bucket type:

Riak::Crdt::DEFAULT_BUCKET_TYPES[:gset] = 'gsets'

# This would enable us to create our gset without specifying a bucket
# type:
travel = client.bucket('travel')
cities_gset = Riak::Crdt::GSet.new(travel, 'cities')
```

```php
$location = new \Basho\Riak\Location('cities', 'travel', 'gsets');
```

```python
travel = client.bucket_type('gsets').bucket('travel')

# The client detects the bucket type's data type and automatically
# returns the right data type for you, in this case a Riak gset.
cities_gset = travel.new('cities')

# You can also create a reference to a gset explicitly:
from riak.datatypes import GSet

cities_gset = GSet(travel, 'cities')
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

// Now we'll create a Builder object for the gset with which we want to
// interact:
var builder = new FetchGSet.Builder()
    .WithBucketType("gsets")
    .WithBucket("travel")
    .WithKey("cities");
```

```javascript
// Now we'll create a options object for the gset with which we want to
// interact:
var options = {
    bucketType: 'gsets',
    bucket: 'travel',
    key: 'cities'
};
```

```erlang
CitiesGSet = riakc_gset:new().

%% GSets in the Erlang client are opaque data structures that
%% collect operations as you mutate them. We will associate the data
%% structure with a bucket type, bucket, and key later on.
```

```curl
# You cannot create an empty gset through the HTTP interface. GSets can
# only be created when an element is added to them, as in the examples
# below.
```

Upon creation, our gset is empty. We can verify that it is empty at any
time:

```java
// Using our "cities" Location from above:

FetchGSet fetch = new FetchGSet.Builder(citiesGSet)
    .build();
FetchGSet.Response response = client.execute(fetch);
RiakGSet gset = response.getDatatype();
boolean isEmpty = gset.viewAsGSet().isEmpty();
```

```ruby
cities_gset.empty?
```

```php
# use $location from earlier
$gset = (new \Basho\Riak\Command\Builder\FetchGSet($riak))
    ->atLocation($location)
    ->build()
    ->execute()
    ->getGSet();

count($gset->getData());
```

```python
len(cities_gset) == 0
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var builder = new FetchGSet.Builder()
    .WithBucketType("gsets")
    .WithBucket("travel")
    .WithKey("cities");

FetchGSet fetchGSetCommand = builder.Build();
RiakResult rslt = client.Execute(fetchGSetCommand);
GSetResponse response = fetchGSetCommand.Response;
// response.Value will be null
```

```javascript
var options = {
    bucketType: 'gsets',
    bucket: 'travel',
    key: 'cities'
};
client.fetchGSet(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }

    if (rslt.notFound) {
        logger.info("gset 'cities' is not found!");
    }
});
```

```erlang
riakc_gset:size(CitiesGSet) == 0.

%% Query functions like size/1, is_element/2, and fold/3 operate over
%% the immutable value fetched from the server. In the case of a new
%% gset that was not fetched, this is an empty collection, so the size
%% is 0.
```

```curl
curl http://localhost:8098/types/gsets/buckets/travel/datatypes/cities

# Response
{"type":"gset","error":"notfound"}
```

## Add to a GSet

But let's say that we read a travel brochure saying that Toronto and
Montreal are nice places to go. Let's add them to our `cities` gset:

```java
// Using our "cities" Location from above:

GSetUpdate gsu = new GSetUpdate()
        .add("Toronto")
        .add("Montreal");
UpdateGSet update = new UpdateGSet.Builder(citiesGSet, gsu)
        .build();
client.execute(update);
```

```ruby
cities_gset.add('Toronto')
cities_gset.add('Montreal')
```

```php
# use $location from earlier
$response = (new \Basho\Riak\Command\Builder\UpdateGSet($riak))
    ->add('Toronto')
    ->add('Montreal')
    ->atLocation($location)
    ->withParameter('returnbody', 'true')
    ->build()
    ->execute();
```

```python
cities_gset.add('Toronto')
cities_gset.add('Montreal')
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var adds = new[] { "Toronto", "Montreal" };

var builder = new UpdateGSet.Builder()
    .WithBucketType("gsets")
    .WithBucket("travel")
    .WithKey("cities")
    .WithAdditions(adds);

UpdateGSet cmd = builder.Build();
RiakResult rslt = client.Execute(cmd);
GSetResponse response = cmd.Response;

Assert.Contains("Toronto", response.AsStrings.ToArray());
Assert.Contains("Montreal", response.AsStrings.ToArray());
```

```javascript
var options = {
    bucketType: 'gsets',
    bucket: 'travel',
    key: 'cities'
};
var cmd = new Riak.Commands.CRDT.UpdateGSet.Builder()
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
CitiesGSet1 = riakc_gset:add_element(<<"Toronto">>, CitiesGSet),
CitiesGSet2 = riakc_gset:add_element(<<"Montreal">>, CitiesGSet1).
```

```curl
curl -XPOST http://localhost:8098/types/gsets/buckets/travel/datatypes/cities \
  -H "Content-Type: application/json" \
  -d '{"add_all":["Toronto", "Montreal"]}'
```

## Remove from a GSet

As the name indicates, GSets are grow-only sets and do not provide facilities 
for removing elements.  Should your application need to remove elements from 
your set, you should use the [Set](./sets.md) datatype


## Retrieve a GSet

Now, we can check on which cities are currently in our gset:

```java
// Using our "cities" Location from above:

FetchGSet fetch = new FetchGSet.Builder(citiesGSet)
        .build();
FetchGSet.Response response = client.execute(fetch);
GSet<BinaryValue> binaryGSet = response.getDatatype().view();
for (BinaryValue city : binaryGSet) {
  System.out.println(city.toStringUtf8());
}
```

```ruby
cities_gset.members

#<GSet: {"Hamilton", "Ottawa", "Toronto"}>
```

```php
# use $location from earlier
$gset = (new \Basho\Riak\Command\Builder\FetchGSet($riak))
    ->atLocation($location)
    ->build()
    ->execute()
    ->getGSet();

var_dump($gset->getData());
```

```python
cities_gset.dirty_value

# The value fetched from Riak is always immutable, whereas the "dirty
# value" takes into account local modifications that have not been
# sent to the server. For example, where the call above would return
# frozengset(['Toronto', 'Hamilton', 'Ottawa']), the call below would
# return frozengset([]).

cities_gset.value

# To fetch the value stored on the server, use the call below. Note
# that this will clear any unsent additions or deletions.
cities_gset.reload()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

foreach (var value in gsetResponse.AsStrings)
{
    Console.WriteLine("Cities GSet Value: {0}", value);
}

// Output:
// Cities GSet Value: Hamilton
// Cities GSet Value: Ottawa
// Cities GSet Value: Toronto
```

```javascript
var options = {
    bucketType: 'gsets',
    bucket: 'travel',
    key: 'cities'
};
client.fetchGSet(options, function(err, rslt) {
    if (err) {
        throw new Error(err);
    }

    logger.info("cities gset values: '%s'",
        rslt.values.join(', '));
});

// Output:
// info: cities gset values: 'Hamilton, Ottawa, Toronto'
```

```erlang
riakc_gset:dirty_value(CitiesGSet5).

%% The value fetched from Riak is always immutable, whereas the "dirty
%% value" takes into account local modifications that have not been
%% sent to the server. For example, where the call above would return
%% [<<"Hamilton">>, <<"Ottawa">>, <<"Toronto">>], the call below would
%% return []. These are essentially ordsets:

riakc_gset:value(CitiesGSet5).

%% To fetch the value stored on the server, use the call below:

{ok, GSetX} = riakc_pb_socket:fetch_type(Pid,
                                        {<<"gsets">>,<<"travel">>},
                                         <<"cities">>).
```

```curl
curl http://localhost:8098/types/gsets/buckets/travel/datatypes/cities

# Response
{"type":"gset","value":["Hamilton","Ottawa","Toronto"],"context":"g2wAAAABaAJtAAAADCMJ/vn7tg36AAAAAWEEag=="}

# You can also fetch the value of the gset without the context included:
curl http://localhost:8098/types/gsets/buckets/travel/datatypes/cities?include_context=false

# Response
{"type":"gset","value":["Hamilton", "Ottawa", "Toronto"]}
```

## Find GSet Member

Or we can see whether our gset includes a specific member:

```java
// Using our "citiesGSet" from above:

System.out.println(citiesGSet.contains(("Vancouver"));
System.out.println(citiesGSet.contains("Ottawa"));
```

```ruby
cities_gset.include? 'Vancouver'
# false

cities_gset.include? 'Ottawa'
# true
```

```php
in_array('Vancouver', $gset->getData()); # false

in_array('Ottawa', $gset->getData()); # true
```

```python
'Vancouver' in cities_gset
# False

'Ottawa' in cities_gset
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

var cities_gset = result.values;
cities_gset.indexOf('Vancouver'); // if present, index is >= 0
cities_gset.indexOf('Ottawa'); // if present, index is >= 0
```

```erlang
%% At this point, GSet5 is the most "recent" gset from the standpoint
%% of our application.

riakc_gset:is_element(<<"Vancouver">>, CitiesGSet5).
riakc_gset:is_element(<<"Ottawa">>, CitiesGSet5).
```

```curl
# With the HTTP interface, this can be determined from the output of
# a fetch command like the one displayed in the example above
```

## Size of GSet

We can also determine the size of the gset:

```java
// Using our "citiesGSet" from above:

int numberOfCities = citiesGSet.size();
```

```ruby
cities_gset.members.length
```

```php
count($gset->getData());
```

```python
len(cities_gset)
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

using System.Linq;

// Note: this enumerates the IEnumerable
gsetResponse.Values.Count();
```

```javascript
// Use standard javascript array property length

var cities_gset_size = result.values.length;
```

```erlang
riakc_gset:size(CitiesGSet5).
```

```curl
# With the HTTP interface, this can be determined from the output of
# a fetch command like the one displayed in the example above
```
