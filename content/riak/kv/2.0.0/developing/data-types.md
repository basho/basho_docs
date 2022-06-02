---
title_supertext: "Developing with Riak KV"
title: "Data Types"
description: ""
project: "riak_kv"
project_version: "2.0.0"
menu:
  riak_kv-2.0.0:
    name: "Data Types"
    identifier: "developing_data_types"
    weight: 102
    parent: "developing"
toc: true
aliases:
  - /riak/2.0.0/dev/using/data-types
  - /riak/kv/2.0.0/dev/using/data-types
  - /riak/2.0.0/dev/data-modeling/data-types
  - /riak/kv/2.0.0/dev/data-modeling/data-types
---

[wiki crdt]: https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type#Others
[concept crdt]: ../../learn/concepts/crdts
[ops bucket type]: ../../using/cluster-operations/bucket-types

Riak KV has Riak-specific data types based on [convergent replicated data types (CRDTs)][wiki crdt]. While Riak KV was built as a data-agnostic key/value store, Riak data types enable you to use Riak KV as a data-aware system and perform transactions on 5 CRDT-inspired data types:

- [Flags](./maps#flags)
- [Registers](./maps#registers)
- [Counters](./counters)
- [Sets](./sets)
- [Maps](./maps)

Counters, sets, and maps can be used as bucket-level data types or types that you interact with directly. Flags and registers must be [embedded in maps](./maps).

For more information on how CRDTs work in Riak KV see [Concepts: Data Types][concept crdt].

## Getting Started with Riak Data Types

The following section explains how to set up a bucket that uses Riak data types. To get started using Riak data types:

1. [Create a bucket with the `datatype` parameter set](#creating-a-bucket-with-a-riak-data-type).
2. [Confirm the bucket was properly configured](#confirm-bucket-configuration).
3. [Activate the bucket type](#activate-bucket-type).

### Creating a Bucket with a Riak Data Type

First create a [bucket type][ops bucket type] that sets the `datatype` bucket parameter to either `counter`, `map`, or `set`.

The following would create a separate bucket type for each of the three
bucket-level data types:

```bash
riak-admin bucket-type create maps '{"props":{"datatype":"map"}}'
riak-admin bucket-type create sets '{"props":{"datatype":"set"}}'
riak-admin bucket-type create counters '{"props":{"datatype":"counter"}}'
```

> **Note**
>
> The names `maps`, `sets`, and `counters` are not reserved
terms. You are free to name bucket types whatever you like, with
the exception of `default`.

### Confirm Bucket configuration

Once you've created a bucket with a Riak data type, you can check
to make sure that the bucket property configuration associated with that
type is correct. This can be done through the `riak-admin` interface:

```bash
riak-admin bucket-type status maps
```

This will return a list of bucket properties and their associated values
in the form of `property: value`. If our `maps` bucket type has been set
properly, we should see the following pair in our console output:

```
datatype: map
```

### Activate Bucket type

If a bucket type has been properly constructed, it needs to be activated
to be usable in Riak. This can also be done using the `bucket-type`
command interface:

```bash
riak-admin bucket-type activate maps
```

To check whether activation has been successful, simply use the same
`bucket-type status` command shown above.

See the [Usage Examples](#usage-examples) section for further information on using Riak data types in the context of an application.

## Required Bucket Properties

In order for Riak data types to work the bucket should have the following bucket properties:

- `allow_mult = true`
- `last_write_wins = false`

These settings are set by default and should not be changed.

## Data Types and Context

Data type context is similar to [causal context](../../learn/concepts/causal-context): it tells Riak KV which version of the data type a client is attempting to modify. Context is required by Riak KV when making decisions about convergence.

If no context is given when attempting a remove or remove-like operation, the operation may fail (removing a field that is not present) or succeed and remove more than intended (removing updates unseen by the client).

> **Note**
>
> The counter data type does not use context; Riak KV will return an empty value when the context is requested from a counter.

In the example below we'll fetch the context [from a user data map created for Ahmed](./maps#create-a-map):

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
%% You cannot fetch a data type's context directly using the Erlang
%% client. This is actually quite all right, as the client automatically
%% manages contexts when making updates.
```

> **Context with the Ruby, Python, and Erlang clients**
>
> In the Ruby, Python, and Erlang clients, you will not need to manually
handle context when making data type updates. The clients will do it all
for you. The one exception amongst the official clients is the Java
client. We'll explain how to use data type contexts with the Java client
directly below.

### Context with the Java and PHP Clients

With the Java and PHP clients, you'll need to manually fetch and return data type contexts for the following operations:

* Disabling a flag within a map
* Removing an item from a set (whether the set is on its own or within a
  map)
* Removing a field from a map

Without context, these operations simply will not succeed due to the
convergence logic driving Riak data types. The example below shows you
how to fetch a data type's context and then pass it back to Riak. More
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

## Usage Examples

- [Flags](./maps#flags)
- [Registers](./maps#registers)
- [Counters](./counters)
- [Sets](./sets)
- [Maps](./maps)

The pages listed above detail using Riak data types at the application level using Basho's [officially supported Riak KV clients](../client-libraries). For more on getting started with client libraries check out the [Developing with Riak KV: Getting Started](../getting-started) section.

All the examples use the bucket type names from above (`counters`, `sets`, and `maps`). You're free to substitute your own bucket type names if you wish.

## Data Types and Search

Riak data types can be searched like any other object, but with the
added benefit that your data type is indexed as a different type by Solr,
the search platform behind Riak Search.

In our Search documentation we offer a [full tutorial](../usage/searching-data-types) as well as a variety of [examples](../usage/searching-data-types/), including code
samples from each of our official client libraries.
