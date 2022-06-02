---
title_supertext: "Developing with Riak KV"
title: "Data Types: HyperLogLogs"
description: ""
project: "riak_kv"
project_version: "2.2.6"
menu:
  riak_kv-2.2.6:
    name: "HyperLogLogs"
    identifier: "data_types_hyperloglogs"
    weight: 100
    parent: "developing_data_types"
toc: true
aliases:
  - /riak/2.2.6/dev/using/data-types/hyperloglogs
  - /riak/kv/2.2.6/dev/using/data-types/hyperloglogs
  - /riak/2.2.6/dev/data-modeling/data-types/hyperloglogs
  - /riak/kv/2.2.6/dev/data-modeling/data-types/hyperloglogs
---

The examples in this section will show you how to use hyperloglogs on their own.

## Set Up a Bucket Type

> If you've already created and activated a bucket type with the `datatype` parameter set to `hyperloglog`, skip to the [next section](#client-setup).

Start by creating a bucket type with the `datatype` parameter set to `hyperloglog`:

```bash
riak_admin bucket-type create hlls '{"props":{"datatype":"hll"}}'
```

> **Note**
>
> The `hlls` bucket type name provided above is an example and is not required to be `hlls`. You are free to name bucket types whatever you like, with the exception of `default`.

After creating a bucket with a Riak data type, confirm the bucket property configuration associated with that type is correct:

```bash
riak-admin bucket-type status hlls
```

This returns a list of bucket properties and their values
in the form of `property: value`.

If our `hlls` bucket type has been set properly we should see the following pair in our console output:

```bash
datatype: hll
```

Once we have confirmed the bucket type is properly configured, we can activate the bucket type to be used in Riak KV:

```bash
riak-admin bucket-type activate hlls
```

We can check if activation has been successful by using the same `bucket-type status` command shown above:

```bash
riak-admin bucket-type status hlls
```

After creating and activating our new `hlls` bucket type, we can setup our client to start using the bucket type as detailed in the next section.

## Client Setup

First, we need to direct our client to the bucket type/bucket/key
location that contains our counter.

For this example we'll use the `hlls` bucket type created and activated above and a bucket called `hlls`:

```erlang
%% Buckets are simply named binaries in the Erlang client. See the
%% examples below for more information
```

```java
// In the Java client, a bucket/bucket type combination is specified
// using a Namespace object. To specify bucket, bucket type, and key,
// use a Location object that incorporates the Namespace object, as is
// done below.

Location hllLocation =
  new Location(new Namespace("<bucket_type>", "<bucket>"), "<key>");
```

```python
bucket_type = client.bucket_type('hlls')
bucket = bucket_type.bucket('my_hlls')
hll = bucket.new(key)

# or

from riak.datatypes import Hll
hll = Hll(bucket, key)
```

```go
// Buckets and bucket types are simply strings in the Go client.

// See the examples below for more information, or the full example at
// https://github.com/basho/riak-go-client/blob/master/examples/dev/using/data-types/hyperloglog.go

// We will need the follow imports to run the examples:
import (
	"fmt"
	"os"
	"time"

	riak "github.com/basho/riak-go-client"
	"errors"
)
```

```csharp
// In the C# client, buckets are just string parameters to operations.
// See the examples below for more information.
```

```javascript
// In the Node.js client, buckets are just string parameters to operations.
// See the examples below for more information.
```

```php
$command = (new Command\Builder\FetchHll($riak_client))
    ->buildLocation('<key>', '<bucket>', 'hlls')
    ->build();
```

```ruby
bucket = client.bucket_type('hlls').bucket('my_hlls')
```

```curl
curl http://localhost:8098/types/<bucket_type>/buckets/<bucket>/datatypes/<key>

# Note that this differs from the URL structure for non-Data-Type
# requests, which end in /keys/<key>
```


## Create a HyperLogLog data type

To create a hyperloglog data structure, you need to specify a bucket/key pair to
hold that hyperloglog. Here is the general syntax for doing so:

```erlang
HLL = riakc_hll:new().

%% Hyperloglogs in the Erlang client are opaque data structures that
%% collect operations as you mutate them. We will associate the data
%% structure with a bucket type, bucket, and key later on.
```

```java
// In the Java client, you specify the location of Data Types
// before you perform operations on them:

Location hllLocation =
  new Location(new Namespace("hlls", "hello"), "darkness");

// In the Java client, there is no intermediate "empty" hyperloglog data type.
// Hyperloglogs can be created when an element is added to them, as in the examples below.
```

```python
bucket_type = client.bucket_type('hlls')
bucket = bucket_type.bucket('my_hlls')
hll = bucket.new(key)

# or

from riak.datatypes import Hll
hll = Hll(bucket, key)
```

```go
// In the Go client, there is no intermediate "empty" hyperloglog data type.
// Hyperloglogs can be created when an element is added to them, as in the examples below.
```

```csharp
// In the C# client, there is no intermediate "empty" hyperloglog data type.
// Hyperloglogs can be created when an element is added to them, as in the examples below.
```

```javascript
// In the Node.js client, there is no intermediate "empty" hyperloglog data type.
// Hyperloglogs can be created when an element is added to them, as in the examples below.
```

```php
// Note that "hlls" is just an example HLL bucket type name used
// in these examples

$command = (new Command\Builder\UpdateHll($riak_client))
    ->add('gosabres poked you.')
    ->add('phprocks viewed your profile.')
    ->add('phprocks started following you.')
    ->buildBucket('<bucket>', 'hlls')
    ->build();

$response = $command->execute();
```

```ruby
key = "darkness"
hll = Riak::Crdt::HyperLogLog.new(bucket, key)
```

```curl
# You cannot create an empty hyperloglog data structure through the HTTP
# interface.
# Hyperloglogs can only be created when an element is added to them, as in the
# examples below.
```

Upon creation, our hyperloglog data structure is empty:

```erlang
HLL.

%% which will return:
%% {hll,0,[]}
```

```java
FetchHll fetch = new FetchHll.Builder(hllLocation)
    .build();
RiakHll hll = client.execute(fetch);
boolean isEmpty = hll.getCardinality() == 0;
```

```python
is_empty = hll.value == 0
```

```go
var resp *riak.FetchHllResponse

builder := riak.NewFetchHllCommandBuilder()
cmd, err := builder.WithBucketType("hlls").
  WithBucket("hello").
  WithKey("darkness").
  Build()
if err != nil {
  return err
}
if err = cluster.Execute(cmd); err != nil {
  return err
}
if fc, ok := cmd.(*riak.FetchHllCommand); ok {
  if fc.Response == nil {
    return errors.New("expected non-nil Response")
  }
  resp = fc.Response
}

fmt.Println("Hyperloglog cardinality: ", resp.Cardinality)
fmt.Println("Hyperloglog isNotFound: ", resp.IsNotFound)
return nil
```

```javascript
var options = {
    bucketType: 'hlls',
    bucket: 'hello',
    key: 'darkness'
};

client.fetchHll(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }

    if (rslt.notFound) {
        logger.info("Not Found");
    }
});
// Prints "Not Found" to logger.info.
```

```csharp
 var fetch = new FetchHll.Builder()
                    .WithBucketType("hlls")
                    .WithBucket("hello")
                    .WithKey("darkness")
                    .Build();

RiakResult rslt = client.Execute(fetch);
HllResponse response = fetch.Response;
if (response.NotFound)
{
    Console.WriteLine("Not Found");
}
// Prints "Not Found" to the console.
```

```php
$command = (new Command\Builder\FetchHll($riak_client))
    ->buildLocation('darkness', 'hello', 'hlls')
    ->build();

$response = $command->execute();

$response->getCode() == '404';
```

```ruby
puts hll.cardinality
# Prints "0"
```

```curl
curl http://localhost:8098/types/hlls/buckets/hello/datatypes/darkness

# Response
{"type":"hll","error":"notfound"}
```

## Add elements to a HyperLogLog data type

```erlang
HLL1 = riakc_hll:add_element(<<"Jokes">>, HLL),
RepeatHLL1 = riakc_hll:add_element(<<"Jokes">>, HLL),
HLL2 = riakc_hll:add_elements([<<"Are">>, <<"Better">>, <<"Explained">>], HLL1),

HLL2.

%% which will return:
%% {hll,0,[<<"Are">>,<<"Better">>,<<"Explained">>, <<"Jokes">>]}
```

```java
HllUpdate hllUpdate = new HllUpdate()
                        .add("Jokes")
                        .add("Are")
                        .addAll(Arrays.asList("Better", "Explained", "Jokes"));

hllUpdate.getElementAdds();
// Returns the set of ["Jokes", "Are", "Better", "Explained"]                     
```

```python
bucket_type = client.bucket_type('hlls')
bucket = bucket_type.bucket('my_hlls')
myhll = datatypes.Hll(bucket, 'hll_one')
myhll.add('Jokes')
myhll.add('Are')
myhll.add('Better')
myhll.add('Explained')
myhll.add('Jokes')
myhll.store()
# myhll.value == 4
```

```go
// We will add values in the next example
```

```csharp
// We will add values in the next example
```

```javascript
// We will add values in the next example
```

```php
$command = (new Command\Builder\UpdateHll($riak_client))
    ->add('Jokes')
    ->add('Are')
    ->add('Better')
    ->add('Explained')
    ->add('Jokes')
    ->buildBucket('my_hlls', 'hlls')
    ->build();

$response = $command->execute();
```

```ruby
```

```curl
curl -XPOST http://localhost:8098/types/hlls/buckets/hello/datatypes/darkness \
  -H "Content-Type: application/json" \
  -d '{"add_all":["my", "old", "friend"]}'
```

However, when using a non-HTTP client, the approximate cardinality/value of our
data structure will be 0, locally, until its pushed to the server and then
[fetched](#retrieve-a-hyperloglog-datatype) from the server.

```erlang
riakc_hll:value(HLL2) == 0.

%% which will return:
%% true

Port = 8087,
{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", Port),
Key = <<"Holy Diver">>,
BucketType = <<"hlls">>,
Bucket = {BucketType, <<"rainbow in the dark">>},

ok = riakc_pb_socket:update_type(Pid, Bucket, Key, riakc_hll:to_op(HLL2)).
ok = riakc_pb_socket:update_type(Pid, Bucket, Key, riakc_hll:to_op(RepeatHLL1)).
```

```java
// Using hllUpdate and hllLocation from above examples

UpdateHll update = new UpdateHll.Builder(hllLocation, hllUpdate)
        .build();
client.execute(update);
```

```python
bucket_type = client.bucket_type('hlls')
bucket = bucket_type.bucket('my_hlls')
myhll = datatypes.Hll(bucket, 'hll_one')
myhll.add('Jokes')
myhll.add('Are')
myhll.add('Better')
myhll.add('Explained')
myhll.add('Jokes')
myhll.store()
# myhll.value == 4
```

```go
adds := [][]byte{
  []byte("Jokes"),
  []byte("Are"),
  []byte("Better"),
  []byte("Explained"),
  []byte("Jokes"),
}

builder := riak.NewUpdateHllCommandBuilder()
cmd, err := builder.WithBucketType("hlls").
  WithBucket("hello").
  WithKey("darkness").
  WithAdditions(adds...).
  Build()
if err != nil {
  return err
}

return cluster.Execute(cmd)
```

```javascript
var options = {
    bucketType: 'hlls',
    bucket: 'hello',
    key: 'darkness',
    additions: ['Jokes', 'Are', 'Better', 'Explained', 'Jokes'],
};

client.updateHll(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```csharp
var adds = new HashSet<string> { "Jokes", "Are", "Better", "Explained", "Jokes" };

var update = new UpdateHll.Builder(adds)
    .WithBucketType("hlls")
    .WithBucket("hello")
    .WithKey("darkness")
    .WithReturnBody(true)
    .Build();

RiakResult rslt = client.Execute(update);
```

```php
$command = (new Command\Builder\UpdateHll($riak_client))
    ->add('Jokes')
    ->add('Are')
    ->add('Better')
    ->add('Explained')
    ->add('Jokes')
    ->buildLocation('darkness', 'hello', 'hlls')
    ->build();

$response = $command->execute();
```

```ruby
hll.add('Jokes')
hll.batch do |s|
  s.add 'Are'
  s.add 'Better'
  s.add 'Explained'
  s.add 'Jokes'
end
```

## Retrieve a HyperLogLog data type

Now, we can check the approximate count-of (a.k.a. the cardinality of the elements
added to) our hyperloglog data structure:

```erlang
{ok, HLL3} = riakc_pb_socket:fetch_type(Pid, Bucket, Key),
riakc_hll:value(HLL3) == 4.

%% which would return:
%% true

%% We added <<"Jokes">> twice, but, remember, the algorithm only counts the
%% unique elements we've added to the data structure.
```

```java
FetchHll hllFetchCmd = new FetchHll.Builder(location).build();
RiakHll hll = client.execute(hllFetchCmd);
hll.getCardinality();
// Which returns 4

// We added "Jokes" twice, but, remember, the algorithm only counts the
// unique elements we've added to the data structure.
```

```python
bucket_type = client.bucket_type('hlls')
bucket = bucket_type.bucket('my_hlls')
myhll = bucket.get('hll_one')
# myhll.value == 4
```

```go
var resp *riak.FetchHllResponse

builder := riak.NewFetchHllCommandBuilder()
cmd, err := builder.WithBucketType("hlls").
  WithBucket("hello").
  WithKey("darkness").
  Build()
if err != nil {
  return err
}
if err = cluster.Execute(cmd); err != nil {
  return err
}
if fc, ok := cmd.(*riak.FetchHllCommand); ok {
  if fc.Response == nil {
    return errors.New("expected non-nil Response")
  }
  resp = fc.Response
}

// We added "Jokes" twice, but, remember, the algorithm only counts the
// unique elements we've added to the data structure.
fmt.Println("Hyperloglog cardinality: ", resp.Cardinality)
return nil
```

```javascript
var options = {
    bucketType: 'hlls',
    bucket: 'hello',
    key: 'darkness'
};

client.fetchHll(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }

    if (rslt.notFound) {
        logger.info("Not Found");
    }
    logger.info("Hyperloglog cardinality is: " + rslt.cardinality);
});
// Prints "Hyperloglog cardinality is: 4"
// We added "Jokes" twice, but, remember, the algorithm only counts the
// unique elements we've added to the data structure.
```

```csharp
var fetch = new FetchHll.Builder()
                    .WithBucketType("hlls")
                    .WithBucket("hello")
                    .WithKey("darkness")
                    .Build();

RiakResult rslt = client.Execute(fetch);
Assert.IsTrue(rslt.IsSuccess, rslt.ErrorMessage);

HllResponse response = fetch.Response;
if (response.NotFound)
{
    Console.WriteLine("Not Found");
}
else
{
    Console.WriteLine("Hyperloglog cardinality is: " + response.Cardinality);  
}

// Prints "Hyperloglog cardinality is: 4"
// We added "Jokes" twice, but, remember, the algorithm only counts the
// unique elements we've added to the data structure.
```

```php
$command = (new Command\Builder\FetchHll($riak_client))
    ->buildLocation('darkness', 'hello', 'hlls')
    ->build();

$result = $command->execute();

// Note: as though we are in a PHP unit test
$this->assertTrue(is_int($response->getHll()->getData()));
$this->assertEquals(4, $response->getHll()->getData());

// We added "Jokes" twice, but, remember, the algorithm only counts the
// unique elements we've added to the data structure.
```

```ruby
puts hll.cardinality
# Prints "4"
```

```curl
curl http://localhost:8098/types/hlls/buckets/hello/datatypes/darkness

# Response
{"type":"hll","value":"4"}
```
