---
title_supertext: "Developing with Riak KV"
title: "Data Types:GSets"
description: ""
project: "riak_kv"
project_version: 2.9.7
menu:
  riak_kv-2.9.7:
    name: "GSets"
    identifier: "data_types_gsets"
    weight: 101
    parent: "developing_data_types"
toc: true
aliases:
  - /riak/2.9.7/dev/using/data-types/gsets
  - /riak/kv/2.9.7/dev/using/data-types/gsets
  - /riak/2.9.7/dev/data-modeling/data-types/gsets
  - /riak/kv/2.9.7/dev/data-modeling/data-types/gsets
---

GSets are a bucket-level Riak data type that can be used by themselves or associated with a bucket/key pair. They do not yet have the ability to be used [within a map like regular sets](../maps#sets-within-maps).

GSets are collections of unique binary values (such as strings). All of the values in a gset are unique and are automatically sorted alphabetically irresepective of the order they were added.

For example, if you attempt to add the element `shovel` to a set that already contains `shovel`, the operation will be ignored by Riak KV.

Unlike sets, elements can only be added and no element modification or deletion is possible.

> **Known Issue**
>
> Unlike other data types, gsets require other data to be present in the cluster before they can be created. If you are unable to create a gset on a new cluster, please try [creating a set](../sets#set-up-a-bucket-type) first and then retrying with your gset. Please see [issue #950](https://github.com/basho/riak_core/issues/950) for details.

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

If our `gsets` bucket type has been set properly we should see the following pair in our console output:

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

Using sets involves creating a bucket/key pair to house a gset and running gset-specific operations on that pair.

Here is the general syntax for creating a bucket type/bucket/key
combination to handle a gset:

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
gset = bucket.new('2019-11-17')

# or

from riak.datatypes import GSet
gset = GSet('account-12345678', '2019-11-17')
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

// As with counters, with the Riak .NET Client you interact with gsets
// by building an Options object or using a Builder
var builder = new FetchGSet.Builder()
    .WithBucketType("gsets")
    .WithBucket("account-12345678")
    .WithKey("2019-11-17");

// NB: builder.Options will only be set after Build() is called.
FetchGSet fetchGSetCommand = builder.Build();

FetchGSetOptions options = new FetchGSetOptions("gsets", "account-12345678", "2019-11-17");

// These two options objects are equal
Assert.AreEqual(options, builder.Options);
```

```javascript
// As with counters, with the Riak Node.js Client you interact with gsets on the
// basis of the gset's location in Riak, as specified by an options object.
// Below is an example:
var options = {
    bucketType: 'gsets',
    bucket: 'account-12345678',
    key: '2019-11-17'
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

## Create a GSet

For the following example, we will use a set to store a list of transactions that occur for an account number on a specific date.
Let's create a Riak gset stored in the key `cities` in the bucket `travel` using the `gsets` bucket type created previously:

```java
// In the Java client, you specify the location of Data Types
// before you perform operations on them:

Location citiesSet =
  new Location(new Namespace("gsets", "travel"), "cities");
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
$location = new \Basho\Riak\Location('2019-11-17', 'account-12345678', 'gsets');
```

```python
bucket = client.bucket_type('gsets').bucket('account-12345678')

# The client detects the bucket type's data type and automatically
# returns the right data type for you, in this case a Riak set.
gset = bucket.new('2019-11-17')

# You can also create a reference to a set explicitly:
from riak.datatypes import GSet

gset = GSet('account-12345678', '2019-11-17')
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

// Now we'll create a Builder object for the gset with which we want to
// interact:
var builder = new FetchGSet.Builder()
    .WithBucketType("gsets")
    .WithBucket("account-12345678")
    .WithKey("2019-11-17");
```

```javascript
// Now we'll create a options object for the gset with which we want to
// interact:
var options = {
    bucketType: 'gsets',
    bucket: 'account-12345678',
    key: '2019-11-17'
};
```

```erlang
20191177Gset = riakc_gset:new().

%% GSets in the Erlang client are opaque data structures that
%% collect operations as you mutate them. We will associate the data
%% structure with a bucket type, bucket, and key later on.
```

```curl
# You cannot create an empty gset through the HTTP interface. GSets can
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
$gset = (new \Basho\Riak\Command\Builder\FetchSet($riak))
    ->atLocation($location)
    ->build()
    ->execute()
    ->getSet();

count($gset->getData());
```

```python
len(gset) == 0
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var builder = new FetchGSet.Builder()
    .WithBucketType("gsets")
    .WithBucket("account-12345678")
    .WithKey("2019-11-17");

FetchGSet fetchGSetCommand = builder.Build();
RiakResult rslt = client.Execute(fetchGSetCommand);
GSetResponse response = fetchGSetCommand.Response;
// response.Value will be null
```

```javascript
var options = {
    bucketType: 'gsets',
    bucket: 'account-12345678',
    key: '2019-11-17'
};
client.fetchSet(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }

    if (rslt.notFound) {
        logger.info("gset '2019-11-17' is not found!");
    }
});
```

```erlang
riakc_gset:size(20191117Gset) == 0.

%% Query functions like size/1, is_element/2, and fold/3 operate over
%% the immutable value fetched from the server. In the case of a new
%% gset that was not fetched, this is an empty collection, so the size
%% is 0.
```

```curl
curl http://localhost:8098/types/gsets/buckets/account-12345678/datatypes/2019-11-17

# Response
{"type":"set","error":"notfound"}
```

## Add to a GSet

But let's say that a pair of transactions occurred today. Let's add them to our `2019-11-17` set:

```java
// Using our "cities" Location from above:

GSetUpdate su = new GSetUpdate()
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
    ->add('transaction a')
    ->add('transaction b')
    ->atLocation($location)
    ->withParameter('returnbody', 'true')
    ->build()
    ->execute();
```

```python
gset.add('transaction a')
gset.add('transaction b')
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

var adds = new[] { "transaction a", "transaction b" };

var builder = new UpdateGSet.Builder()
    .WithBucketType("gsets")
    .WithBucket("account-12345678")
    .WithKey("2019-11-17")
    .WithAdditions(adds);

UpdateGSet cmd = builder.Build();
RiakResult rslt = client.Execute(cmd);
GSetResponse response = cmd.Response;
Assert.Contains("transaction a", response.AsStrings.ToArray());
Assert.Contains("transaction b", response.AsStrings.ToArray());
```

```javascript
var options = {
    bucketType: 'gsets',
    bucket: 'account-1234578',
    key: '2019-11-17'
};
var cmd = new Riak.Commands.CRDT.UpdateGSet.Builder()
    .withBucketType(options.bucketType)
    .withBucket(options.bucket)
    .withKey(options.key)
    .withAdditions(['transaction a', 'transaction b'])
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
20191117Gset1 = riakc_gset:add_element(<<"transaction a">>, 20191117Gset),
20191117Gset2 = riakc_gset:add_element(<<"transaction b">>, 20191117Gset1).
```

```curl
curl -XPOST http://localhost:8098/types/gsets/buckets/account-12345678/datatypes/2019-11-17 \
  -H "Content-Type: application/json" \
  -d '{"add_all":["transaction a", "transaction b"]}'
```

## Remove from a GSet

Removal from a GSet is not possible.

## Retrieve a GSet

Now, we can check on which transactions are currently in our gset:

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
$gset = (new \Basho\Riak\Command\Builder\FetchSet($riak))
    ->atLocation($location)
    ->build()
    ->execute()
    ->getSet();

var_dump($gset->getData());
```

```python
gset.dirty_value

# The value fetched from Riak is always immutable, whereas the "dirty
# value" takes into account local modifications that have not been
# sent to the server. For example, where the call above would return
# frozenset(['Transaction a', 'Transaction b']), the call below would
# return frozenset([]).

gset.value

# To fetch the value stored on the server, use the call below. Note
# that this will clear any unsent additions.
gset.reload()
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

foreach (var value in GSetResponse.AsStrings)
{
    Console.WriteLine("2019-11-17 Transactions: {0}", value);
}

// Output:
// 2019-11-17 Transactions: transaction a
// 2019-11-17 Transactions: transaction b
```

```javascript
var options = {
    bucketType: 'gsets',
    bucket: 'account-12345678',
    key: '2019-11-17'
};
client.fetchSet(options, function(err, rslt) {
    if (err) {
        throw new Error(err);
    }

    logger.info("2019-11-17 gset values: '%s'",
        rslt.values.join(', '));
});

// Output:
// info: 2019-11-17 gset values: 'transaction a, transaction b'
```

```erlang
riakc_gset:dirty_value(20191117Gset3).

%% The value fetched from Riak is always immutable, whereas the "dirty
%% value" takes into account local modifications that have not been
%% sent to the server. For example, where the call above would return
%% [<<"Hamilton">>, <<"Ottawa">>, <<"Toronto">>], the call below would
%% return []. These are essentially ordsets:

riakc_gset:value(20191117Gset3).

%% To fetch the value stored on the server, use the call below:

{ok, SetX} = riakc_pb_socket:fetch_type(Pid,
                                        {<<"gsets">>,<<"account-12345678">>},
                                         <<"20191117">>).
```

```curl
curl http://localhost:8098/types/gsets/buckets/account-12345678/datatypes/2019-11-17

# Response
{"type":"set","value":["transaction a","transaction b"]}
```

## Find GSet Member

Or we can see whether our gset includes a specific member:

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
in_array('transaction z', $gset->getData()); # false

in_array('transaction a', $gset->getData()); # true
```

```python
'transaction c' in gset
# False

'transaction a' in gset
# True
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

using System.Linq;

bool includesTransactionZ = response.AsStrings.Any(v => v == "transaction z");
bool includesTransactionA = response.AsStrings.Any(v => v == "transaction a");
```

```javascript
// Use standard javascript array method indexOf()

var 2019-11-17_gset = result.values;
2019-11-17_gset.indexOf('transaction z'); // if present, index is >= 0
2019-11-17_gset.indexOf('transaction a'); // if present, index is >= 0
```

```erlang
%% At this point, GSet3 is the most "recent" set from the standpoint
%% of our application.

riakc_gset:is_element(<<"transaction z">>, 20191117Gset3).
riakc_gset:is_element(<<"transaction a">>, 20191117Gset3).
```

```curl
# With the HTTP interface, this can be determined from the output of
# a fetch command like the one displayed in the example above
```

## Size of GSet

We can also determine the size of the gset:

```java
// Using our "citiesSet" from above:

int numberOfCities = citiesSet.size();
```

```ruby
cities_set.members.length
```

```php
count($gset->getData());
```

```python
len(gset)
```

```csharp
// https://github.com/basho/riak-dotnet-client/blob/develop/src/RiakClientExamples/Dev/Using/DataTypes.cs

using System.Linq;

// Note: this enumerates the IEnumerable
gsetResponse.Values.Count();
```

```javascript
// Use standard javascript array property length

var 2019-11-17_gset_size = result.values.length;
```

```erlang
riakc_gset:size(20191117Gset3).
```

```curl
# With the HTTP interface, this can be determined from the output of
# a fetch command like the one displayed in the example above
```




