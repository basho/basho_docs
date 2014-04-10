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

Interacting with objects in Riak typically involves the same CRUD (**C**reate, **R**ead, **U**pdate, **D**elete) operations that you'd find in any key/value store.

## Object/Key Operations

Riak organizes data into bucket types, buckets, keys, and values, with [[bucket types|Using Bucket Types]] acting as an additional namespace in Riak versions 2.0 and greater. Values (or objects) are identifiable by a unique key, and each key/value pair is stored in a bucket.

Buckets are essentially a flat namespace in Riak. You can name them whatever you'd like, even `bucket` or `a90bf521c` or `___` or `:)`. They have no intrinsic significance beyond allowing you to store objects with the same key in different buckets (the same goes for naming keys). Buckets do, however, enable you to provide common configurations to the keys and values within them, such as [[replication properties]] and [[commit hooks|Using Commit Hooks]]. Such [[properties|Buckets]] are managed using bucket types.

Most of the interactions you'll have with Riak will involve setting or retrieving the value of a key. Riak has [[supported client libraries|Client Libraries]] for Erlang, Java, PHP, Python, Ruby and C/C++. In addition, there are [[community-supported projects|Client Libraries#Community-Libraries]] for .NET, Node.js, Python, Perl, Clojure, Scala, Smalltalk, and many others.

### Read an Object

Here is the basic command form for retrieving a specific key from a bucket:

```
GET /types/TYPE/buckets/BUCKET/keys/KEY
```

Here is an example of a read performed on the key `rufus` in the bucket `dogs`, which bears the type `animals`:

```ruby
bucket = client.bucket('dogs')
obj = bucket.get('rufus', bucket_type: 'animals')
```

```java
// In the Java client, it is best to specify a bucket type/bucket/key
// Location object that can be used as a reference for further operations.

Location myKey = new Location("dogs")
        .setBucketType("animals")
        .setKey("rufus");
```

```python
bucket = client.bucket('dogs', bucket_type='animals')
obj = bucket.get('rufus')
```

```erlang
{ok, Obj} = riakc_pb_socket(Pid,
                            {<<"animals">>, <<"dogs">>},
                            <<"rufus">>).
```

```curl
curl http://localhost:8098/types/animals/buckets/dogs/keys/rufus
```

If there is no object stored under that particular key, Riak will return a message indicating that the object doesn't exist.

```ruby
Riak::ProtobuffsFailedRequest: Expected success from Riak but received not_found. The requested object was not found.
```

```java
java.lang.NullPointerException
```

```python
riak.RiakError: 'no_type'
```

```erlang
{error,notfound}
```

```curl
not found
```

If you're using HTTP to interact with Riak, as opposed to using a [[client library|Client Libraries]], Riak understands many HTTP-defined headers, such as `Accept` for content-type negotiation, which is relevant when dealing with siblings (see [[the sibling examples for the HTTP API|HTTP Fetch Object#Siblings-examples]]), and `If-None-Match`/`ETag` and `If-Modified-Since`/`Last-Modified` for conditional requests.

#### Read Parameters

Parameter | Default | Description
:---------|:--------|:-----------
`r` | `2` | How many replicas need to agree when retrieving an existing object before the write
`pr` | `0` | How many vnodes must respond for a read to be deemed successful
`notfound_ok` | If set to `true`, if the first vnode to respond doesn't have a copy of the object, Riak will deem the failure authoritative and immediately return a `notfound` error to the client

Riak also accepts many query parameters, including `r` for setting the R-value for GET requests (R values describe how many replicas need to agree when retrieving an existing object in order to return a successful response).

Here is an example of attempting a read with `r` set to `3`:

```ruby
bucket = client.bucket('dogs')
obj = bucket.get('rufus', bucket_type: 'animals', r: 3)
```

```java
// Using the "myKey" location specified above:

FetchValue fetch = new FetchValue.Builder(myKey)
        .withOption(FetchOption.R, new Quorum(3))
        .build();
FetchValue.Response response = client.execute(fetch);
RiakObject obj = response.getValue(RiakObject.class);
```

```python
bucket = client.bucket('dogs', bucket_type='animals')
obj = bucket.get('rufus', r=3)
obj.data
```

```erlang
{ok, Obj} = riakc_pb_socket:get(Pid,
                                {<<"animals">>, <<"dogs">>},
                                <<"rufus">>,
                                [{r, 3}]).
```

```curl
curl http://localhost:8098/types/animals/buckets/dogs/keys/rufus?r=3
```

If you're using HTTP, you will most often see the following response codes:

* `200 OK`
* `300 Multiple Choices`
* `304 Not Modified`

The most common error code:

* `404 Not Found`

<div class="note">
<div class="title">Note</div>
If you're using a Riak client instead of HTTP, these responses will vary a great deal, so make sure to check the documentation for your specific client.
</div>

### Store an Object

Your application will often have its own method of generating the keys for its data, e.g. on the basis of timestamps. If so, storing that data is easy. The basic request looks like this.

```
PUT /types/TYPE/buckets/BUCKET/keys/KEY
```

<div class="note">
<div class="title">Note</div>
If you're using HTTP, <tt>POST</tt> is also a valid method, for compatibility's sake.
</div>

There is no need to intentionally create buckets in Riak. They pop into existence when keys are added to them, and disappear when all keys have been removed from them. If you don't specify a bucket's type, the type `[[default|Using Bucket Types]]` will be applied.

#### Specifying Content Type

For all writes to Riak, you will need to specify a content type, for example `text/plain` or `application/json`.

```ruby
bucket = client.bucket('oscar_wilde')
obj = Riak::RObject.new(bucket, 'genius')
obj.content_type = 'text/plain'
obj.raw_data = 'I have nothing to declare but my genius'
obj.store(bucket_type: 'quotes')
```

```java
Location wildeGeniusQuote = new Location("oscar_wilde")
        .setBucketType("quotes")
        .setKey("genius");
BinaryValue text = BinaryValue.create("I have nothing to declare but my genius");
RiakObject obj = new RiakObject()
        .setContentType("text/plain")
        .setValue(text);
StoreValue store = new StoreValue.Builder(myKey, obj)
        .build();
client.execute(store);
```

```python
bucket = client.bucket('oscar_wilde', bucket_type: 'quotes')
obj = RiakObject(client, bucket, 'genius')
obj.content_type = 'text/plain'
obj.data = 'I have nothing to declare but my genius'
obj.store()
```

```erlang
Object = riakc_obj:new({<<"quotes">>, <<"oscar_wilde">>},
                       <<"genius">>,
                       <<"I have nothing to declare but my genius">>,
                       <<"text/plain">>).
riakc_pb_socket:put(Pid, Object).
```

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "I have nothing to declare but my genius" \
  http://localhost:8098/types/quotes/buckets/oscar_wilde/keys/genius
```

#### Using Vector Clocks

If an object already exists under a certain key and you want to write a new object to that key, Riak needs to know what to do, especially if multiple writes are happening at the same time. Which of the objects being written should be deemed correct? These kinds of scenarios can arise quite frequently in distributed, [[eventually consistent|Eventual Consistency]] systems.

One way of the ways that Riak can make intelligent decisions about which object should be deemed correct is to use [[vector clocks]] to make that decision. If you're writing an object to a key that already exists, you'll need to do the following:

* Read the existing object
* Fetch the vector clock from the object's metadata
* Attach the vector clock to the new object's metadata
* Write the new object

A more detailed tutorial on vector clocks and object updates more generally can be found in the [[Conflict Resolution]] doc. Here, we'll walk you through a basic example.

Let's say that the current NBA champion is the Washington Generals. We've stored that data in Riak under the key `champion` in the bucket `nba`, which bears the bucket type `sports`. The value of the object is a simple text snippet that says `Washington Generals`.

But one day the Harlem Globetrotters enter the league and dethrone the hapless Generals (forever, as it turns out). Because we want our Riak database to reflect this new development in the league, we want to make a new write to the `champion` key. Let's read the object stored there and fetch the vector clock.

```ruby
bucket = client.bucket('nba')
obj = bucket.get('champion', bucket_type: 'sports')
obj.vclock

# The vector clock will look something like this:
# a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=
```

```java
Location currentChampion = new Location("nba")
        .setBucketType("sports")
        .setKey("champion");
FetchValue fetch = new FetchValue.Builder(currentChampion)
        .build();
FetchValue.Response response = client.execute(fetch);
System.out.println(response.getvClock());

// The vector clock will look something like this:
// a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=
```

```python
bucket = client.bucket('nba', bucket_type: 'sports')
obj = bucket.get('champion')
obj.vclock

# The vector clock will look something like this:
# a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=
```

```erlang
%% In the Erlang client, you cannot view a vector clock directly, but it 
%% will be included in the output when you fetch an object:

{ok, Obj} = riakc_pb_socket:get(Pid,
                                {<<"sports">>, <<"nba">>},
                                <<"champion">>).

%% In the resulting object Obj, the vector clock can be seen as a long list
%% of integers, like so:

<<107,206,97,96,96,96,204,...>>
```

```curl
# When using curl, the vector clock is attached to the X-Riak-Vclock header

curl -i http://localhost:8098/types/sports/buckets/nba/keys/champion

# In the resulting output, the header will look something like this:

X-Riak-Vclock: a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=
```

You can then attach the vector clock to the new object and attempt a write. The vector clock will inform Riak which version of the object the client has seen.

Let's update the value of `champion` to `Harlem Globetrotters` using the vector clock that we obtained in the example above:

```ruby
bucket = client.bucket('nba')
obj = Riak::RObject.new(bucket, 'champion')
obj.content_type = 'text/plain'
obj.raw_data = 'Harlem Globetrotters'
obj.vclock = 'a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA='
obj.store(bucket_type: 'sports')
```

```java
Location johnGlennKey = new Location("nba")
        .setBucketType("sports")
        .setKey("champion");
BinaryValue text = BinaryValue.create("Harlem Globetrotters");
RiakObject obj = new RiakObject()
        .setContentType("text/plain")
        .setVtag("a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=")
        .setValue(text);
StoreValue store = new StoreValue.Builder(myKey, obj)
        .build();
client.execute(store);
```

```python
bucket = client.bucket('nba', bucket_type='sports')
obj = RiakObject(client, bucket, 'champion')
obj.content_type = 'text/plain'
obj.data = 'Harlem Globetrotters'
obj.vclock = 'a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA='
obj.store()
```

```erlang
Object = riakc_obj:new({<<"sports">>, <<"nba">>},
                       <<"champion">>,
                       <<"Harlem Globetrotters">>,
                       <<"text/plain">>).
ObjectWithVclock = riakc_obj:set_vclock(Object, <<"a85hYGBgzGDKBVIcWu/1S4OVPaIymBIZ81gZbskuOMOXBQA=">>).
riakc_pb_socket:put(Pid, ObjectWithVclock).
```

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "Harlem Globetrotters" \
  http://localhost:8098/types/spots/buckets/nba/keys/champion
```

Other request headers are optional for writes (and not necessary if using a client library):

* `X-Riak-Meta-YOUR_HEADER` for any additional metadata headers that should be stored with the object (eg. `X-Riak-Meta-FirstName`).

#### Write Parameters

Similar to how read requests support the `r` query parameter, write requests also support the following parameters:

Parameter | Default | Description
:---------|:--------|:-----------
`w` | `2` | How many replicas to write to before returning a successful response
`pw` | `0` | How many primary vnodes must respond for a write to be deemed successful
`dw` | `0` | How many replicas to commit to durable storage before returning a successful response
`returnbody` | `false` | Whether to return the contents of the stored object

Here is an example of storing an object (another brief text snippet) under the key `viper` in the bucket `dodge`, which bears the type `cars`, with `w` set to `3`:

```ruby
bucket = client.bucket('dodge')
obj = Riak::RObject.new(bucket, 'viper')
obj.content_type = 'text/plain'
obj.raw_data = 'vroom'
obj.store(bucket_type: 'cars', r: 3)
```

```java
Location viperKey = new Location("dodge")
        .setBucketType("cars")
        .setKey("viper");
BinaryValue text = BinaryValue.create("vroom");
RiakObject obj = new RiakObject()
        .setContentType("text/plain")
        .setValue(text);
StoreValue store = new StoreValue.Builder(myKey, obj)
        .withOption(StoreOption.W, new Quorum(3))
        .build();
client.execute(store);
```

```python
bucket = client.bucket('dodge', bucket_type='cars')
obj = RiakObject(client, bucket, 'viper')
obj.content_type = 'text/plain'
obj.data = 'vroom'
obj.store(w=3)
```

```erlang
Object = riakc_obj:new({<<"cars">>, <<"dodge">>},
                       <<"viper">>,
                       <<"vroom">>,
                       <<"text/plain">>,
                       [{w, 3}]).
riakc_pb_socket:put(Pid, Object).
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

If `returnbody` is set to `true`, any of the response headers expected from a read request may be present. Like a `GET` request, `300 Multiple Choices` may be returned if siblings existed or were created as part of the operation, and the response can be dealt with similarly.

Let's give it a shot, using the same object from above:

```ruby
bucket = client.bucket('dodge')
obj = Riak::RObject.new(bucket, 'viper')
obj.content_type = 'text/plain'
obj.raw_data = 'vroom'
obj.store(bucket_type: 'cars', r: 3, returnbody: true)
```

```java
Location viperKey = new Location("dodge")
        .setBucketType("cars")
        .setKey("viper");
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

```python
bucket = client.bucket('dodge', bucket_type='cars')
obj = RiakObject(client, bucket, 'viper')
obj.content_type = 'text/plain'
obj.data = 'vroom'
obj.store(w=3, return_body=True)
```

```erlang
Object = riakc_obj:new({<<"cars">>, <<"dodge">>},
                       <<"viper">>,
                       <<"vroom">>,
                       <<"text/plain">>).
riakc_pb_socket:put(Pid, Object, [return_body]).
```

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "vroom" \
  http://localhost:8098/types/cars/buckets/dodge/keys/viper?w=3&returnbody=true
```

### Store a New Object and Assign a Random Key

If your application would rather leave key-generation up to Riak, issue a `POST` request to the bucket URL instead of a PUT to a bucket/key pair:

```
POST /types/TYPE/buckets/BUCKET/keys
```

If you don't pass Riak a `key` name after the bucket, it will know to create one for you.

Supported headers are the same as for bucket/key write requests, though `X-Riak-Vclock` will never be relevant for these POST requests.  Supported query parameters are also the same as for bucket/key PUT requests.

Normal status codes:

* `201 Created`

This command will store an object in the bucket `random_user_keys`, which bears the bucket type `users`.

```ruby
bucket = client.bucket('random_user_keys')
obj = Riak::RObject.new(bucket)
obj.content_type = 'application/json'
obj.raw_data = '{"user":"data"}'

obj.store(bucket_type: 'users')

# The client will assign a key like the following:
obj.key
"GB8fW6DDZtXogK19OLmaJf247DN"
```

```java
Location locationWithoutKey = new Location("random_user_keys")
        .setBucketType("users");
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

```python
bucket = client.bucket('random_user_keys', bucket_type='users')
obj = RiakObject(client, bucket)
obj.content_type = 'application/json'
obj.data = '{"user":"data"}'
obj.store()

obj.key

# The Python client will assign a random key along the following lines:
'ZPFF18PUqGW9efVou7EHhfE6h8a'
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

The normal HTTP response codes for `DELETE` operations are `204 No Content` and `404 Not Found`. 404 responses are *normal*, in the sense that `DELETE` operations are idempotent and not finding the resource has the same effect as deleting it.

Let's try to delete our `genius` key from the `oscar_wilde` bucket (which bears the type `quotes`) from above.

```ruby
bucket = client.bucket('oscar_wilde')
bucket.delete('genius', bucket_type: 'quotes')
```

```java
// Using the "myKey" location from above:
Location geniusQuote = new Location("oscar_wilde")
        .setBucketType("quotes")
        .setKey("genius");
DeleteValue delete = new DeleteValue.Builder(geniusQuote).build();
client.execute(delete);
```

```python
bucket = client.bucket('oscar_wilde', bucket_type='quotes')
bucket.delete('genius')
```

```erlang
riakc_pb_socket:delete(Pid, {<<"quotes">>, <<"oscar_wilde">>}, <<"genius">>)
```

```curl
curl -XDELETE http://localhost:8098/types/quotes/buckets/oscar_wilde/keys/genius
```

## Bucket Properties and Operations

Buckets are essentially a flat namespace in Riak. They allow the same key name to exist in multiple buckets and enable you to apply configurations across keys.

<div class="info">
<div class="title">How Many Buckets Can I Have?</div>
Buckets come with virtually no cost <em>except for when you modify the default bucket properties</em>. Modified bucket properties are gossiped around the 
cluster and therefore add to the amount of data sent around the network. In other words, buckets using the <tt>default</tt> bucket type are free. More on that in the next section.
</div>

### Bucket Types

A comprehensive tutorial on bucket types can be found in the [[Using Bucket Types]] document. Here, we'll discuss some basic parameters that can be managed using bucket types.

Here are some important bucket properties to be aware of:

Parameter | Description | Default
:---------|:------------|:-------
`n_val` | The number of replicas for objects in a bucket. The `n_val` should be an integer greater than 0 and less than or equal to the number of nodes in the cluster.<br /><br />**Note**: If you change the `n_val` after keys have been added to the bucket, it may result in failed reads, as the new value may not be replicated to all of the appropriate partitions.| `3`
`allow_mult` | With `allow_mult` set to `false`, clients will never be presented with siblings upon read. Though siblings will often be created in Riak during concurrent writes or network partitions even if `allow_mult` is set to `false`, only the most recent object as determined by timestamp will be presented to the client. If this parameter is set to `true`, Riak will present sibling objects to the client, which will then be responsible for resolving the confict. | `true`
`last_write_wins` | If `allow_mult` is set to `false`, setting `last_write_wins` to `true`, Riak will _always_ overwrite existing objects and will ignore the timestamps associated with those objects. | `false`

<div class="note">
<div class="title">Note</div>
Setting <tt>allow_mult</tt> to <tt>true</tt> and <tt>last_write_wins</tt> to false leads to unpredictable behavior and should not be used.
</div>

As an example, let's create a bucket type called `n_val_of_5` that sets the `n_val` to 5:

```bash
riak-admin bucket-type create n_val_of_5 '{"props":{"n_val":5}}'
```

We must activate the type for those parameters to take effect:

```bash
riak-admin bucket-type activate n_val_of_5
```

Once the type is activated, we can see which properties are associated with our bucket type (and, by extension, any bucket that bears that type):

```ruby
# You cannot currently fetch a bucket type's properties in the Ruby
# client. We suggest using curl instead. See the example in the code
# tab to the right.
```

```java
// Fetching the bucket properties of a bucket type/bucket combination
// must be done using a RiakCluster object rather than a RiakClient.

Location testType = new Location("any_bucket_name")
        .setBucketType("n_val_of_5");
FetchBucketPropsOperation fetchProps = new FetchBucketPropsOperation
        .Builder(testType)
        .build();
cluster.execute(fetchProps);
BucketProperties props = fetchProps.get().getBucketProperties();
```

```python
bt = BucketType(client, 'n_val_of_5')
bt.get_properties()
```

```erlang
riakc_pb_socket:get_bucket_type(Pid, <<"n_val_of_5">>).
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

We can also view this information in our browser at the URL specified above.

That's the basics of how the most essential Riak key/value operations work. In addition to this tutorial, we recommend an in-depth reading of the [[HTTP API]] page, as it will give you details on the headers, parameters, status, and other details that you should keep in mind even when using a client library.
