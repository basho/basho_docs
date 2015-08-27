---
title: The Basics
project: riak
version: 1.0.0+
document: tutorials
audience: beginner
keywords: [developers]
moved: {
  '1.4.0-': '/tutorials/fast-track/Basic-Riak-API-Operations'
}
---

While Riak offers a wide variety of features and querying options, from
[[Riak Data Types|Using Data Types]] to [[Riak Search|Using Search]] and
beyond, Riak almost always performs best and most predictably when you
use the basic CRUD operations---**C**reate, **R**ead, **U**pdate,
**D**elete---that you'd find in any key/value store. Learning these
operations is a great place to start when learning how to develop
applications that use Riak.

## Object/Key Operations

Riak organizes data into buckets, keys, and values, with [[bucket
types|Using Bucket Types]] acting as an additional namespace in Riak
versions 2.0 and greater. Values (also referred to simply as objects in
this tutorial) are identifiable by a unique key, and each key/value pair
is stored in a bucket. Objects can be any data type you wish, e.g. JSON,
XML, binary data, plaintext, and more.

Buckets are essentially a flat namespace in Riak. You can name them
whatever you'd like, even `bucket` or `a90bf521c` or `___` or `:)`. They
have no intrinsic significance beyond allowing you to store objects with
the same key in different buckets. The same goes for naming keys: many
objects can have the same key as long as they're in different buckets.

[[Bucket types|Using Bucket Types]] enable you to provide common
configurations for buckets (as many buckets as you wish). This means
that you can easily enable buckets to share common configurations, i.e.
identical [[replication properties]] or [[commit hooks|Using Commit
Hooks]].

Many of the interactions you'll have with Riak will involve setting or
retrieving the value of a key. Riak has [[supported client
libraries|Client Libraries]] for Java, Ruby, Python, .NET and Erlang. In
addition, there are [[community-supported projects|Client
Libraries#Community-Libraries]] for Node.js, Python, Perl,
Clojure, Scala, Smalltalk, and many others.

## Reading Objects

You can think of reads in Riak as analogous to HTTP `GET` requests. You
specify a bucket type, bucket, and key, and Riak either returns the
object that's stored there---including its [[siblings|The
Basics#Siblings]] \(more on that later)---or it returns `not found` (the
equivalent of an HTTP `404 Object Not Found`).

Here is the basic command form for retrieving a specific key from a
bucket:

```
GET /types/<type>/buckets/<bucket>/keys/<key>
```

Here is an example of a read performed on the key `rufus` in the bucket
`dogs`, which bears the [[bucket type|Using Bucket Types]] `animals`:

```java
// In the Java client, it is best to specify a bucket type/bucket/key
// Location object that can be used as a reference for further
// operations, as in the example below:
Location myKey = new Location(new Namespace("animals", "dogs"), "rufus");
```

```ruby
bucket = client.bucket_type('animals').bucket('dogs')
obj = bucket.get('rufus')
```

```php
$response = (new \Basho\Riak\Command\Builder\FetchObject($riak))
  ->buildLocation('rufus', 'users', 'animals')
  ->build()
  ->execute();
```

```python
bucket = client.bucket_type('animals').bucket('dogs')
obj = bucket.get('rufus')
```

```csharp
// Using the Riak .NET Client it is best to specify a bucket type/bucket/key
// RiakObjectId object that can be used as a reference for further
// operations
var id = new RiakObjectId("animals", "dogs", "rufus");
```

```javascript
client.fetchValue({ bucketType: 'animals', bucket: 'dogs', key: 'rufus' }, function (err, rslt) {
    assert(rslt.isNotFound);
});
```

```erlang
{ok, Obj} = riakc_pb_socket:get(Pid,
                            {<<"animals">>, <<"dogs">>},
                            <<"rufus">>).
```

```go
cmd, err = riak.NewFetchValueCommandBuilder().
  WithBucketType("animals").
  WithBucket("dogs").
  WithKey("rufus").
  Build()
if err != nil {
  util.ErrLog.Println(err)
  continue
}
```

```curl
curl http://localhost:8098/types/animals/buckets/dogs/keys/rufus
```

<div class="note">
<div class="title">Getting started with Riak clients</div>
If you are connecting to Riak using one of Basho's official [[client
libraries]], you can find more information about getting started with
your client in our [[quickstart guide|Five-Minute
Install#Setting-Up-Your-Riak-Client]].
</div>

At the moment, there's no object stored in the location where we just
attempted a read, which means that we'll get the following response:

```java
java.lang.NullPointerException
```

```ruby
Riak::ProtobuffsFailedRequest: Expected success from Riak but received not_found. The requested object was not found.
```

```php
$response->getStatusCode(); // 404
$response->isSuccess(); // false
```

```python
riak.RiakError: 'no_type'
```

```csharp
result.IsSuccess == false
result.ResultCode == ResultCode.NotFound
```

```javascript
rslt.isNotFound === true;
```

```erlang
{error,notfound}
```

```go
...
```

```curl
not found
```

## Writing Objects

Writes in Riak, i.e. storing or modifying objects, are like HTTP `PUT`
requests. Here is the basic form of writes:

```
PUT /types/<type>/buckets/<bucket>/keys/<key>

# If you're using HTTP to interact with Riak, you can also use POST
```

In the example above, our read was unsuccessful because our Riak cluster
is currently empty. Let's change that by storing an object containing
information about a dog named Rufus. We'll store that object in the
location described above, i.e. in the key `rufus` in the bucket `dogs`,
which bears the `animals` [[bucket type|Using Bucket Types]].

The object we're storing will be very simple, just a basic text snippet
of something that Rufus might say. Let's build the object and then store
it.

```java
String quote = "WOOF!";
Namespace bucket = new Namespace("animals", "dogs");
Location rufusLocation = new Location(bucket, "rufus");
RiakObject rufusObject = new RiakObject()
        .setContentType("text/plain")
        .setValue(BinaryValue.create(quote));
StoreValue storeOp = new StoreValue.Builder(rufusObject)
        .withLocation(rufusLocation)
        .build();
client.execute(storeOp);
```

```ruby
bucket = client.bucket_type('animals').bucket('dogs')
obj = Riak::RObject.new(bucket, 'rufus')
obj.content_type = 'text/plain'
obj.data = 'WOOF!'
obj.store
```

```php
$response = (new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->buildLocation('rufus', 'users', 'animals')
  ->buildObject('WOOF!', 'text/plain')
  ->build()
  ->execute();
```

```python
bucket = client.bucket_type('animals').bucket('dogs')
obj = RiakObject(client, bucket, 'rufus')
obj.content_type = 'text/plain'
obj.data = 'WOOF!'
obj.store()
```

```csharp
var id = new RiakObjectId("animals", "dogs", "rufus")
var obj = new RiakObject(id, "WOOF!", "text/plain");
var result = client.Put(obj);
```

```javascript
var riakObj = new Riak.Commands.KV.RiakObject();
riakObj.setContentType('text/plain');
riakObj.setValue('WOOF!');
client.storeValue({
    bucketType: 'animals', bucket: 'dogs', key: 'rufus',
    value: riakObj
}, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```go
...
```

Notice that we specified both a value for the object, i.e. `WOOF!`, and
a content type, `text/plain`. We'll learn more about content types in
the [[section below|The Basics#Content-Types]].

Now, run the same read operation we attempted in the [[section above|The
Basics#Reading-Objects]]. If the write operation was successful, you
should be able to successfully read the object. Your Riak cluster is no
longer empty!

## Content Types

Riak is a fundamentally content-agnostic database. You can use it to
store anything you want, from JSON to XML to HTML to binaries to images
and beyond. You should always bear in mind that _all_ objects stored in
Riak need to have a specified content type. If you don't specify a
content type, the reaction will vary based on your client library:

```java
// In the Java client, the response when storing an object without
// specifying a content type will depend on what is being stored. If you
// store a Java Map, for example, the client will automatically specify
// that the object is "application/json"; if you store a String, the
// client will specify "application/x-www-form-urlencoded"; POJOs are
// stored as JSON by default, and so on.
```

```ruby
# In the Ruby client, you must always specify a content type. If you
# you don't, you'll see the following error:
ArgumentError: content_type is not defined!
```

```php
# PHP will default to cURLs default content-type for POST & PUT requests:
#   application/x-www-form-urlencoded

# If you use the StoreObject::buildJsonObject() method when building your command, 
# it will store the item with application/json as the content-type
```

```python
# In the Python client, the default content type is "application/json".
# Because of this, you should always make sure to specify the content
# type when storing other types of data.
```

```csharp
// Using the Riak .NET Client, the response when storing an object without
// specifying a content type will depend on what is being stored.
// If you store a Dictionary, for example, the client will
// automatically specify that the object is "application/json";
// POCOs are stored as JSON by default, and so on.
```

```javascript
// In the Node.js client, the default content type is "application/json".
// Because of this, you should always make sure to specify the content
// type when storing other types of data.
```

```erlang
%% In the Erlang client, the response when storing an object without
%% specify8ing a content type will depend on what is being stored. If
%% you store a simple binary, for example, the client will automatically
%% specify that the object is "application/octet-stream"; if you store a
%% string, the client will specify "application/x-erlang-binary"; and so
%% on.
```

```go
...
```

Because content type negotiation varies so widely from client to client,
we recommend consulting the documentation for your preferred client for
more information.

## Updating Objects

When we stored an object in the [[section above|The
Basics#Writing-Objects]], we did so in a location that was empty.
Updating an already-existing object, however, is a bit trickier because
there are some best practices and other issues that you should be aware
of. Consult the [[Object Updates]] doc for more information.

## Read Parameters

Parameter | Default | Description
:---------|:--------|:-----------
`r` | `2` | How many replicas need to agree when retrieving an existing object before the write
`pr` | `0` | How many [[vnodes]] must respond for a read to be deemed successful
`notfound_ok` | If set to `true`, if the first vnode to respond doesn't have a copy of the object, Riak will deem the failure authoritative and immediately return a `notfound` error to the client

Riak also accepts many query parameters, including `r` for setting the
R-value for GET requests (R values describe how many replicas need to
agree when retrieving an existing object in order to return a successful
response).

Here is an example of attempting a read with `r` set to `3`:

```java
// Using the "myKey" location specified above:
FetchValue fetch = new FetchValue.Builder(myKey)
        .withOption(FetchOption.R, new Quorum(3))
        .build();
FetchValue.Response response = client.execute(fetch);
RiakObject obj = response.getValue(RiakObject.class);
System.out.println(obj.getValue());
```

```ruby
bucket = client.bucket_type('animals').bucket('dogs')
obj = bucket.get('rufus', r: 3)
p obj.data
```

```php
$response = (new \Basho\Riak\Command\Builder\FetchObject($riak))
  ->buildLocation('rufus', 'dogs', 'animals')
  ->build()
  ->execute();

var_dump($response->getObject()->getData());
```

```python
bucket = client.bucket_type('animals').bucket('dogs')
obj = bucket.get('rufus', r=3)
print obj.data
```

```csharp
var id = new RiakObjectId("animals", "dogs", "rufus");
var opts = new RiakGetOptions();
opts.SetR(3);
var rslt = client.Get(id, opts);
Debug.WriteLine(Encoding.UTF8.GetString(rslt.Value.Value));
```

```javascript
var fetchOptions = {
    bucketType: 'animals', bucket: 'dogs', key: 'rufus',
    r: 3
};
client.fetchValue(fetchOptions, function (err, rslt) {
    var riakObj = rslt.values.shift();
    var rufusValue = riakObj.value.toString("utf8");
    logger.info("rufus: %s", rufusValue);
});
```

```erlang
{ok, Obj} = riakc_pb_socket:get(Pid,
                                {<<"animals">>, <<"dogs">>},
                                <<"rufus">>,
                                [{r, 3}]).
```

```go
...
```

```curl
curl http://localhost:8098/types/animals/buckets/dogs/keys/rufus?r=3
```

If you're using HTTP, you will most often see the following response
codes:

* `200 OK`
* `300 Multiple Choices`
* `304 Not Modified`

The most common error code:

* `404 Not Found`

<div class="note">
<div class="title">Note</div>
If you're using a Riak client instead of HTTP, these responses will vary
a great deal, so make sure to check the documentation for your specific
client.
</div>

### Store an Object

Your application will often have its own method of generating the keys
for its data, e.g. on the basis of timestamps. If so, storing that data
is easy. The basic request looks like this.

```
PUT /types/TYPE/buckets/BUCKET/keys/KEY

# If you're using HTTP, POST can be used instead of PUT. The only
# difference between POST and PUT is that you should POST in cases where
# you want Riak to auto-generate a key. More on this can be found in the
# examples below.
```

There is no need to intentionally create buckets in Riak. They pop into
existence when keys are added to them, and disappear when all keys have
been removed from them. If you don't specify a bucket's type, the type
`[[default|Using Bucket Types]]` will be applied.

#### Specifying Content Type

For all writes to Riak, you will need to specify a content type, for
example `text/plain` or `application/json`.

```java
Location wildeGeniusQuote = new Location(new Namespace("quotes", "oscar_wilde"), "genius");
BinaryValue text = BinaryValue.create("I have nothing to declare but my genius");
RiakObject obj = new RiakObject()
        .setContentType("text/plain")
        .setValue(text);
StoreValue store = new StoreValue.Builder(myKey, obj)
        .build();
client.execute(store);
```

```ruby
bucket = client.bucket_type('quotes').bucket('oscar_wilde')
obj = Riak::RObject.new(bucket, 'genius')
obj.content_type = 'text/plain'
obj.raw_data = 'I have nothing to declare but my genius'
obj.store
```

```php
$response = (new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->buildLocation('genius', 'oscar_wilde', 'quotes')
  ->buildObject('I have nothing to declare but my genius!', 'text/plain')
  ->build()
  ->execute();
```

```python
bucket = client.bucket_type('quotes').bucket('oscar_wilde')
obj = RiakObject(client, bucket, 'genius')
obj.content_type = 'text/plain'
obj.data = 'I have nothing to declare but my genius'
obj.store()
```

```csharp
var id = new RiakObjectId("quotes", "oscar_wilde", "genius");
var obj = new RiakObject(id, "I have nothing to declare but my genius",
    RiakConstants.ContentTypes.TextPlain);
var rslt = client.Put(obj);
```

```javascript
var riakObj = new Riak.Commands.KV.RiakObject();
riakObj.setContentType('text/plain');
riakObj.setValue('I have nothing to declare but my genius');
client.storeValue({
    bucketType: 'quotes', bucket: 'oscar_wilde', key: 'genius',
    value: riakObj
}, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
Object = riakc_obj:new({<<"quotes">>, <<"oscar_wilde">>},
                       <<"genius">>,
                       <<"I have nothing to declare but my genius">>,
                       <<"text/plain">>).
riakc_pb_socket:put(Pid, Object).
```

```go
...
```

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "I have nothing to declare but my genius" \
  http://localhost:8098/types/quotes/buckets/oscar_wilde/keys/genius

# Please note that POST is also a valid method for writes, for the sake
# of compatibility
```

#### Using Causal Context

If an object already exists under a certain key and you want to write a
new object to that key, Riak needs to know what to do, especially if
multiple writes are happening at the same time. Which of the objects
being written should be deemed correct? These kinds of scenarios can
arise quite frequently in distributed, [[eventually consistent|Eventual
Consistency]] systems.

Riak decides which object to choose in case of conflict using [[causal
context]]. These objects track the causal history of objects.
They are attached to _all_ Riak objects as metadata, and they are not
readable by humans. They may sound complex---and they are fairly complex
behind the scenes---but using them in your application is very simple.

Whenever you perform updates in Riak

Using causal context in an update would involve the following steps;

1. Fetch the object
2. Modify the object's value (without modifying the fetched [[context
   object|Causal Context]])
3. Write the new object to Riak

Step 2 is the most important here. All of Basho's official Riak clients
enable you to modify an object's value without modifying its [[causal
context]]. Although a more detailed tutorial on context objects and
object updates can be found in [[Conflict Resolution]], we'll walk you
through a basic example here.

Let's say that the current NBA champion is the Washington Generals.
We've stored that data in Riak under the key `champion` in the bucket
`nba`, which bears the bucket type `sports`. The value of the object is
a simple text snippet that says `Washington Generals`.

But one day the Harlem Globetrotters enter the league and dethrone the
hapless Generals (forever, as it turns out). Because we want our Riak
database to reflect this new development in the league, we want to make
a new write to the `champion` key. Let's read the object stored there
and modify the value.

```java
Location currentChampion = new Location(new Namespace("sports", "nba"), "champion");
FetchValue fetch = new FetchValue.Builder(currentChampion)
        .build();
FetchValue.Response response = client.execute(fetch);
RiakObject obj = response.getValue(RiakObject.class);
obj.setValue(BinaryValue.create("Harlem Globetrotters"))
```

```ruby
bucket = client.bucket_type('sports').bucket('nba')
obj = bucket.get('champion')
obj.raw_data = 'Harlem Globetrotters'
obj.store
```

```php
$location = new \Basho\Riak\Location('champion', new \Basho\Riak\Bucket('nba', 'sports'));
$object = (new \Basho\Riak\Command\Builder\FetchObject($riak))
  ->withLocation($location)
  ->build()
  ->execute()
  ->getObject();

$object->setData('Harlem Globetrotters');

(new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->withLocation($location)
  ->withObject($object)
  ->build()
  ->execute();
```

```python
bucket = client.bucket_type('sports').bucket('nba')
obj = bucket.get('champion')
obj.data = 'Harlem Globetrotters'
```

```csharp
var id = new RiakObjectId("sports", "nba", "champion");
var obj = new RiakObject(id, "Washington Generals",
    RiakConstants.ContentTypes.TextPlain);
var rslt = client.Put(obj);

rslt = client.Get(id);
obj = rslt.Value;
obj.SetObject("Harlem Globetrotters",
    RiakConstants.ContentTypes.TextPlain);
rslt = client.Put(obj);
```

```javascript
var riakObj = new Riak.Commands.KV.RiakObject();
riakObj.setContentType('text/plain');
riakObj.setValue('Washington Generals');

var options = {
    bucketType: 'sports', bucket: 'nba', key: 'champion',
    value: riakObj
};
client.storeValue(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
    delete options.value;
    client.fetchValue(options, function (err, rslt) {
        if (err) {
            throw new Error(err);
        }
        var fetchedObj = rslt.values.shift();
        fetchedObj.setValue('Harlem Globetrotters');
        options.value = fetchedObj;
        options.returnBody = true;
        client.storeValue(options, function (err, rslt) {
            if (err) {
                throw new Error(err);
            }
            var updatedObj = rslt.values.shift();
            logger.info("champion: %s", updatedObj.value.toString('utf8'));
        });
    });
});
```

```erlang
%% In the Erlang client, you cannot view a context objectdirectly, but it
%% will be included in the output when you fetch an object:

{ok, Obj} = riakc_pb_socket:get(Pid,
                                {<<"sports">>, <<"nba">>},
                                <<"champion">>),
UpdatedObj = riakc_obj:update_value(Obj, <<"Harlem Globetrotters">>),
{ok, NewestObj} = riakc_pb_socket:put(Pid, UpdatedObj, [return_body]).
```

```go
...
```

```curl
# When using curl, the context object is attached to the X-Riak-Vclock header

curl -i http://localhost:8098/types/sports/buckets/nba/keys/champion

# In the resulting output, the header will look something like this:

X-Riak-Vclock: a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=

# When performing a write to the same key, that same header needs to
# accompany the write for Riak to be able to use the context object
```

In the samples above, we didn't need to actually interact with the
context object, as retaining and passing along the context object was
accomplished automatically by the client. If, however, you do need
access to an object's context, the clients enable you to fetch it from
the object:

```java
// Using the RiakObject obj from above:

Vclock vClock = obj.getVclock();
System.out.println(vClock.asString());

// The context object will look something like this:
// a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=
```

```ruby
# Using the RObject obj from above:

obj.vclock

# The context object will look something like this:
# a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=
```

```php
# Using the RObject obj from above:

echo $object->getVclock(); // a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=
```

```python
# Using the RiakObject obj from above:

obj.vclock

# The context object will look something like this:
# a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=
```

```csharp
// Using the RiakObject obj from above:
var vclock = result.Value.VectorClock;
Console.WriteLine(Convert.ToBase64String(vclock));

// The output will look something like this:
// a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=
```

```javascript
// Using the RiakObject fetchedObj from above:
var fetchedObj = rslt.values.shift();
logger.info("vclock: %s", fetchedObj.getVClock().toString('base64'));

// The output will look something like this:
// vclock: a85hYGBgymDKBVIcR4M2cov1HeHKYEpkymNlsE2cfo4PKjXXjuOU+FHdWqAUM1CqECSVBQA=
```

```erlang
%% Using the Obj object from above:

riakc_obj:vclock(Obj).

%% The context object will look something like this in the Erlang shell:
%% <<107,206,97,96,96,96,204,96,202,5,82,28,202,156,255,126,
%% 6,175,157,255,57,131,41,145,49,143,149,225,240,...>>
```

```go
...
```

#### Write Parameters

Similar to how read requests support the `r` query parameter, write
requests also support the following parameters:

Parameter | Default | Description
:---------|:--------|:-----------
`w` | `2` | How many replicas to write to before returning a successful response
`pw` | `0` | How many primary vnodes must respond for a write to be deemed successful
`dw` | `0` | How many replicas to commit to durable storage before returning a successful response
`returnbody` | `false` | Whether to return the contents of the stored object

Here is an example of storing an object (another brief text snippet)
under the key `viper` in the bucket `dodge`, which bears the type
`cars`, with `w` set to `3`:

```java
Location viperKey = new Location(new Namespace("cars", "dodge"), "viper");
BinaryValue text = BinaryValue.create("vroom");
RiakObject obj = new RiakObject()
        .setContentType("text/plain")
        .setValue(text);
StoreValue store = new StoreValue.Builder(myKey, obj)
        .withOption(StoreOption.W, new Quorum(3))
        .build();
client.execute(store);
```

```ruby
bucket = client.bucket_type('cars').bucket('dodge')
obj = Riak::RObject.new(bucket, 'viper')
obj.content_type = 'text/plain'
obj.raw_data = 'vroom'
obj.store(w: 3)
```

```php
(new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->buildLocation('viper', 'dodge', 'cars')
  ->buildObject('vroom', 'text/plain')
  ->withParameter('w', 3)
  ->build()
  ->execute();
```

```python
bucket = client.bucket_type('cars').bucket('dodge')
obj = RiakObject(client, bucket, 'viper')
obj.content_type = 'text/plain'
obj.data = 'vroom'
obj.store(w=3)
```

```csharp
var id = new RiakObjectId("cars", "dodge", "viper");
var obj = new RiakObject(id, "vroom", "text/plain");
var options = new RiakPutOptions();
options.SetW(new Quorum(3));
var result = client.Put(obj, options);
```

```javascript
var riakObj = new Riak.Commands.KV.RiakObject();
riakObj.setContentType('text/plain');
riakObj.setValue('vroom');

var options = {
    bucketType: 'cars', bucket: 'dodge', key: 'viper',
    w: 3, value: riakObj
};
client.storeValue(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
Object = riakc_obj:new({<<"cars">>, <<"dodge">>},
                       <<"viper">>,
                       <<"vroom">>,
                       <<"text/plain">>,
                       [{w, 3}]).
riakc_pb_socket:put(Pid, Object).
```

```go
...
```

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "vroom" \
  http://localhost:8098/types/cars/buckets/dodge/keys/viper?w=3
```

Normal HTTP status codes (responses will vary for client libraries):

* `200 OK`
* `204 No Content`
* `300 Multiple Choices`

#### Return Body

If `returnbody` is set to `true`, any of the response headers expected
from a read request may be present. Like a `GET` request, `300 Multiple
Choices` may be returned if siblings existed or were created as part of
the operation, and the response can be dealt with similarly.

Let's give it a shot, using the same object from above:

```java
Location viperKey = new Location(new Namespace("cars", "dodge"), "viper");
BinaryValue text = BinaryValue.create("vroom");
RiakObject obj = new RiakObject()
        .setContentType("text/plain")
        .setValue(text);
StoreValue store = new StoreValue.Builder(myKey, obj)
        .withOption(StoreOption.W, new Quorum(3))
        .withOption(StoreOption.RETURN_BODY, true)
        .build();
client.execute(store);
```

```ruby
bucket = client.bucket_type('cars').bucket('dodge')
obj = Riak::RObject.new(bucket, 'viper')
obj.content_type = 'text/plain'
obj.raw_data = 'vroom'
obj.store(w: 3, returnbody: true)
```

```php
(new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->buildLocation('viper', 'dodge', 'cars')
  ->buildObject('vroom', 'text/plain')
  ->withParameter('w', 3)
  ->withParameter('returnbody', 'true')
  ->build()
  ->execute();
```

```python
bucket = client.bucket_type('cars').bucket('dodge')
obj = RiakObject(client, bucket, 'viper')
obj.content_type = 'text/plain'
obj.data = 'vroom'
obj.store(w=3, return_body=True)
```

```csharp
var id = new RiakObjectId("cars", "dodge", "viper");
var obj = new RiakObject(id, "vroom", "text/plain");
var options = new RiakPutOptions();
options.SetW(new Quorum(3));
options.SetReturnBody(true);
var result = client.Put(obj, options);
```

```javascript
var riakObj = new Riak.Commands.KV.RiakObject();
riakObj.setContentType('text/plain');
riakObj.setValue('vroom');

var options = {
    bucketType: 'cars', bucket: 'dodge', key: 'viper',
    w: 3, returnBody: true, value: riakObj
};
client.storeValue(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
    var riakObj = rslt.values.shift();
    var viper = riakObj.value;
    logger.info("dodge viper: %s", viper.toString('utf8'));
});
```

```erlang
Object = riakc_obj:new({<<"cars">>, <<"dodge">>},
                       <<"viper">>,
                       <<"vroom">>,
                       <<"text/plain">>).
riakc_pb_socket:put(Pid, Object, [return_body]).
```

```go
...
```

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "vroom" \
  http://localhost:8098/types/cars/buckets/dodge/keys/viper?w=3&returnbody=true
```

### Store a New Object and Assign a Random Key

If your application would rather leave key-generation up to Riak, issue
a `POST` request to the bucket URL instead of a PUT to a bucket/key
pair:

```
POST /types/TYPE/buckets/BUCKET/keys
```

If you don't pass Riak a `key` name after the bucket, it will know to
create one for you.

Supported headers are the same as for bucket/key write requests, though
`X-Riak-Vclock` will never be relevant for these POST requests.
Supported query parameters are also the same as for bucket/key PUT
requests.

Normal status codes:

* `201 Created`

This command will store an object in the bucket `random_user_keys`,
which bears the bucket type `users`.

```java
Namespace locationWithoutKey = new Namespace("users", "random_user_keys");
BinaryValue text = BinaryValue.create("{'user':'data'}");
RiakObject obj = new RiakObject()
        .setContentType("application/json")
        .setValue(text);
StoreValue store = new StoreValue.Builder(locationWithoutKey, obj)
        .build();
String key = client.execute(store).getLocation().getKeyAsString();

// The Java client will assign a random key along the following lines:
"ZPFF18PUqGW9efVou7EHhfE6h8a"
```

```ruby
bucket = client.bucket_type('users').bucket('random_user_keys')
obj = Riak::RObject.new(bucket)
obj.content_type = 'application/json'
obj.raw_data = '{"user":"data"}'

obj.store

# The client will assign a key like the following:
obj.key
"GB8fW6DDZtXogK19OLmaJf247DN"
```

```php
$response = (new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->buildBucket('random_user_keys', 'users')
  ->buildJsonObject(['user'=>'data'])
  ->build()
  ->execute();

echo $response->getLocation()->getKey(); // GB8fW6DDZtXogK19OLmaJf247DN
```

```python
bucket = client.bucket_type('users').bucket('random_user_keys')
obj = RiakObject(client, bucket)
obj.content_type = 'application/json'
obj.data = '{"user":"data"}'
obj.store()

obj.key

# The Python client will assign a random key along the following lines:
'ZPFF18PUqGW9efVou7EHhfE6h8a'
```

```csharp
var id = new RiakObjectId("users", "random_user_keys", null);
var obj = new RiakObject(id, @"{'user':'data'}",
    RiakConstants.ContentTypes.ApplicationJson);
var rslt = client.Put(obj);
Debug.WriteLine(format: "Generated key: {0}", args: rslt.Value.Key);

// The .NET client will output a random key similar to this:
// Generated key: DWDsnpYSqOU363c0Bqe8hCwAM7Q
```

```javascript
var user = {
    user: 'data'
};
var options = {
    bucketType: 'users', bucket: 'random_user_keys',
    returnBody: true, value: user
};
client.storeValue(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
    var riakObj = rslt.values.shift();
    var generatedKey = riakObj.getKey();
    logger.info("Generated key: %s", generatedKey);
});

// The Node.js client will output a random key similar to this:
// info: Generated key: VBAMoX0OOucymVCxeQEYzLzzAh2
```

```erlang
Object = riakc_obj:new({<<"users">>, <<"random_user_keys">>}, undefined, <<"{'user':'data'}">>, <<"application/json">>).
riakc_pb_socket:put(Pid, Object).

%% The key can be retrieved from the output of the above call.
%% It will look something like this:

{ok,{riakc_obj,{<<"users">>,<<"random_user_keys">>},
               <<"EZ7pp4bpdfpZw0fPUdTUafveQjO">>,undefined,[],undefined,
               undefined}}
```

```go
...
```

```curl
curl -i -XPOST \
  -H "Content-Type: text/plain" \
  -d "this is a test" \
  http://localhost:8098/types/users/buckets/random_user_keys/keys

# In the output, you should see a Location header that will give you the
# location of the object in Riak, with the key at the end:

Location: /buckets/test/keys/G7FYUXtTsEdru4NP32eijMIRK3o
```

### Delete an Object

The delete command follows a predictable pattern and looks like this:

```
DELETE /types/TYPE/buckets/BUCKET/keys/KEY
```

The normal HTTP response codes for `DELETE` operations are `204 No
Content` and `404 Not Found`. 404 responses are *normal*, in the sense
that `DELETE` operations are idempotent and not finding the resource has
the same effect as deleting it.

Let's try to delete our `genius` key from the `oscar_wilde` bucket
(which bears the type `quotes`) from above.

```java
Location geniusQuote = new Location(new Namespace("quotes", "oscar_wilde"), "genius");
DeleteValue delete = new DeleteValue.Builder(geniusQuote).build();
client.execute(delete);
```

```ruby
bucket = client.bucket_type('quotes').bucket('oscar_wilde')
bucket.delete('genius')
```

```php
(new \Basho\Riak\Command\Builder\DeleteObject($riak))
  ->buildBucket('oscar_wilde', 'quotes')
  ->build()
  ->execute();
```

```python
bucket = client.bucket_type('quotes').bucket('oscar_wilde')
bucket.delete('genius')
```

```csharp
var id = new RiakObjectId("users", "random_user_keys", null);
var obj = new RiakObject(id, @"{'user':'data'}",
    RiakConstants.ContentTypes.ApplicationJson);
var rslt = client.Put(obj);
string key = rslt.Value.Key;
id = new RiakObjectId("users", "random_user_keys", key);
var del_rslt = client.Delete(id);
```

```javascript
// continuing from above example
options = {
    bucketType: 'users', bucket: 'random_user_keys',
    key: generatedKey
};
client.deleteValue(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
riakc_pb_socket:delete(Pid, {<<"quotes">>, <<"oscar_wilde">>}, <<"genius">>)
```

```go
...
```

```curl
curl -XDELETE http://localhost:8098/types/quotes/buckets/oscar_wilde/keys/genius
```

## Updating Objects

Updating objects tends to be a bit trickier than reading and writing
objects. We recommend checking out our documentation on [[object
updates]] for a full tutorial, including code samples from our [[client
libraries]].

## Bucket Properties and Operations

Buckets are essentially a flat namespace in Riak. They allow the same
key name to exist in multiple buckets and enable you to apply
configurations across keys.

<div class="info">
<div class="title">How Many Buckets Can I Have?</div>
Buckets come with virtually no cost <em>except for when you modify the
default bucket properties</em>. Modified bucket properties are gossiped
around the cluster and therefore add to the amount of data sent around
the network. In other words, buckets using the <tt>default</tt> bucket
type are free. More on that in the next section.
</div>

### Bucket Types

A comprehensive tutorial on bucket types can be found in the [[Using
Bucket Types]] document. Here, we'll discuss some basic parameters that
can be managed using bucket types.

Here are some important bucket properties to be aware of:

Parameter | Description | Default
:---------|:------------|:-------
`n_val` | The number of replicas for objects in a bucket. The `n_val` should be an integer greater than 0 and less than or equal to the number of nodes in the cluster.<br /><br />**Note**: If you change the `n_val` after keys have been added to the bucket, it may result in failed reads, as the new value may not be replicated to all of the appropriate partitions.| `3`
`allow_mult` | With `allow_mult` set to `false`, clients will never be presented with siblings upon read. Though siblings will often be created in Riak during concurrent writes or network partitions even if `allow_mult` is set to `false`, only the most recent object as determined by timestamp will be presented to the client. If this parameter is set to `true`, Riak will present sibling objects to the client, which will then be responsible for resolving the conflict. | `true`
`last_write_wins` | If `allow_mult` is set to `false`, setting `last_write_wins` to `true`, Riak will _always_ overwrite existing objects and will ignore the timestamps associated with those objects. | `false`

<div class="note">
<div class="title">Note</div>
Setting both <code>allow_mult</code> and <code>last_write_wins</code> to
<code>true</code> necessarily leads to unpredictable behavior and should
always be avoided.
</div>

As an example, let's create a bucket type called `n_val_of_5` that sets
the `n_val` to 5:

```bash
riak-admin bucket-type create n_val_of_5 '{"props":{"n_val":5}}'
```

We must activate the type for those parameters to take effect:

```bash
riak-admin bucket-type activate n_val_of_5
```

Once the type is activated, we can see which properties are associated
with our bucket type (and, by extension, any bucket that bears that
type):

```java
// Fetching the bucket properties of a bucket type/bucket combination
// must be done using a RiakCluster object rather than a RiakClient.
Namespace testType = new Namespace("n_val_of_5", "any_bucket_name");
FetchBucketPropsOperation fetchProps = new FetchBucketPropsOperation
        .Builder(testType)
        .build();
cluster.execute(fetchProps);
BucketProperties props = fetchProps.get().getBucketProperties();
```

```ruby
bt = client.bucket_type('n_val_of_5')
bt.props
```

```php
# Bucket type props are not directly fetchable, but bucket props are
$response = (new \Basho\Riak\Command\Builder\FetchBucketProperties($riak))
  ->buildBucket('bucket_name', 'n_val_of_5')
  ->build()
  ->execute();
```

```python
bt = BucketType(client, 'n_val_of_5')
bt.get_properties()
```

```csharp
var rslt = client.GetBucketProperties("n_val_of_5", "any_bucket_name");
RiakBucketProperties props = rslt.Value;
```

```javascript
client.fetchBucketProps({
    bucketType: 'n_val_of_5', bucket: 'any_bucket_name'
}, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
    logger.info("props: %s", JSON.stringify(rslt));
});
```

```erlang
riakc_pb_socket:get_bucket_type(Pid, <<"n_val_of_5">>).
```

```go
...
```

```curl
curl http://localhost:8098/types/n_val_of_5/props
```

This should return JSON of the following form:

```json
{
  "props": {
    "allow_mult": false,
    ...
    "n_val": 5,
    ...
  }
}
```

We can also view this information in our browser at the URL specified
above.

That's the basics of how the most essential Riak key/value operations
work. In addition to this tutorial, we recommend an in-depth reading of
the [[HTTP API]] page, as it will give you details on the headers,
parameters, status, and other details that you should keep in mind even
when using a client library.
