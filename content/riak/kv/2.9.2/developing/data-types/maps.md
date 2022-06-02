---
title_supertext: "Developing with Riak KV"
title: "Data Types: Maps"
description: ""
project: "riak_kv"
project_version: 2.9.2
menu:
  riak_kv-2.9.2:
    name: "Maps"
    identifier: "data_types_maps"
    weight: 102
    parent: "developing_data_types"
toc: true
aliases:
  - /riak/2.9.2/dev/using/data-types/maps
  - /riak/kv/2.9.2/dev/using/data-types/maps
  - /riak/2.9.2/dev/data-modeling/data-types/maps
  - /riak/kv/2.9.2/dev/data-modeling/data-types/maps
---

Maps are the most versatile of the Riak data types because all other data types can be embedded within them, _including maps themselves_. This enables the creation of complex, custom data types from a few basic building blocks.

Using counters, sets, and maps within maps are similar to working with those types at the bucket level.

## Set Up a Bucket Type

> If you've already created and activated a bucket type with the `datatype` parameter set to `map`, skip to the [next section](#client-setup).

Start by creating a bucket type with the `datatype` parameter set to `map`:

```bash
riak-admin bucket-type create maps '{"props":{"datatype":"map"}}'
```

> **Note**
>
> The `maps` bucket type name provided above is an example and is not required to be `maps`. You are free to name bucket types whatever you like, with the exception of `default`.

After creating a bucket with a Riak data type, confirm the bucket property configuration associated with that type is correct:

```bash
riak-admin bucket-type status maps
```

This returns a list of bucket properties and their values
in the form of `property: value`.

If our `map` bucket type has been set properly we should see the following pair in our console output:

```bash
datatype: map
```

Once we have confirmed the bucket type is properly configured, we can activate the bucket type to be used in Riak KV:

```bash
riak-admin bucket-type activate maps
```

We can check if activation has been successful by using the same `bucket-type status` command shown above:

```bash
riak-admin bucket-type status maps
```

After creating and activating our new `maps` bucket type, we can setup our client to start using the bucket type as detailed in the next section.

## Client Setup

First, we need to direct our client to the bucket type/bucket/key location that contains our map.

The syntax for creating a map is analogous to the
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
# The client detects the bucket type's datatype and automatically
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

# Note that this differs from the URL structure for non-data type requests,
# which end in /keys/<key>
```

## Create a Map

For this example, say we want to use Riak KV to store information about our company's customers. We'll use the `maps` bucket type created and activated previously and a bucket called `customers`. Each customer's data will be contained in its own key in the `customers` bucket.

We can create a map for the user Ahmed (`ahmed_info`) using the `maps` bucket type:

```java
// In the Java client, you specify the location of data types
// before you perform operations on them:

Location ahmedMap =
  new Location(new Namespace("maps", "customers"), "ahmed_info");
```

```ruby
customers = client.bucket_type('maps').bucket('customers')
map = Riak::Crdt::Map.new(customers, 'ahmed_info')

# Alternatively, the Ruby client enables you to set a bucket type as being
# globally associated with a Riak data type. The following would set all
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

## Registers

Registers are essentially named binaries (like strings). Any binary
value can act as the value of a register. Like flags, registers cannot
be used on their own and must be embedded in Riak maps.

### Registers Within Maps

Continuing with our previous `customers` example, let's store some information in our map.

The first piece of information we want to store in our map is Ahmed's name and
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
# performing them on one data type.

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

If a register did not previously exist, Riak KV will create that register for you.

## Flags

Flags behave much like Boolean values, except that instead of `true` or
`false` flags have the values `enable` or `disable`.

Flags cannot be used on their own, i.e. a flag cannot be stored in a bucket/key by itself. Instead, flags can only be stored within maps.

To disable an existing flag, you have to read it or provide [a context](../#data-types-and-context).

### Flags Within Maps

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
curl http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info

# Response
{"type":"map","value":{"first_name_register":"Ahmed","phone_number_register":"5551234567"},"context":"g2wAAAABaAJtAAAADCMJ/vn2jOEXAAAAAWEBag=="}

curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "enterprise_customer_flag": "disable"
    },
    "context" : "g2wAAAABaAJtAAAADCMJ/vn2jOEXAAAAAWEBag=="
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

## Counters Within Maps

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

## Sets Within Maps

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
curl http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info

# Response
{"type":"map","value":{"enterprise_customer_flag":false,"first_name_register":"Ahmed","interests_set":["motorcycles","opera","robots"],"page_visits_counter":1,"phone_number_register":"5551234567"},"context":"g2wAAAABaAJtAAAADCMJ/vn2jOEXAAAAAWEEag=="}

curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "interests_set": {
        "remove": "opera",
        "add": "indie pop"
      }
    },
    "context" : "g2wAAAABaAJtAAAADCMJ/vn2jOEXAAAAAWEEag=="
  }
  '
```

## Maps Within Maps

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
curl http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info

# Response
{"type":"map","value":{"annika_info_map":{"first_name_register":"Annika","last_name_register":"Weiss","phone_number_register":"5559876543"},"enterprise_customer_flag":false,"first_name_register":"Ahmed","interests_set":["indie pop","motorcycles","robots"],"page_visits_counter":1,"phone_number_register":"5551234567"},"context":"g2wAAAABaAJtAAAADCMJ/vn2jOEXAAAAAWEGag=="}

curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "annika_info_map": {
        "remove": ["phone_number_register"]
      }
    },
    "context" : "g2wAAAABaAJtAAAADCMJ/vn2jOEXAAAAAWEGag=="
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
curl http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info

# Response
{"type":"map","value":{"annika_info_map":{"first_name_register":"Annika","last_name_register":"Weiss"},"enterprise_customer_flag":false,"first_name_register":"Ahmed","interests_set":["indie pop","motorcycles","robots"],"page_visits_counter":1,"phone_number_register":"5551234567"},"context":"g2wAAAABaAJtAAAADCMJ/vn2jOEXAAAAAWEHag=="}

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
    },
    "context" : "g2wAAAABaAJtAAAADCMJ/vn2jOEXAAAAAWEHag=="
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
curl http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info

# Response
{"type":"map","value":{"annika_info_map":{"enterprise_plan_flag":false,"family_plan_flag":false,"first_name_register":"Annika","free_plan_flag":true,"interests_set":["tango dancing"],"last_name_register":"Weiss","widget_purchases_counter":1},"enterprise_customer_flag":false,"first_name_register":"Ahmed","interests_set":["indie pop","motorcycles","robots"],"page_visits_counter":1,"phone_number_register":"5551234567"},"context":"g2wAAAABaAJtAAAADCMJ/vn2jOEXAAAAAWEKag=="}

curl -XPOST http://localhost:8098/types/maps/buckets/customers/datatypes/ahmed_info \
  -H "Content-Type: application/json" \
  -d '
  {
    "update": {
      "annika_info_map": {
        "update": {
          "interests_set": {
            "remove": "tango dancing"
          }
        }
      }
    },
    "context" : "g2wAAAABaAJtAAAADCMJ/vn2jOEXAAAAAWEKag=="
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
