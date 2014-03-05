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

Riak organizes data into bucket types, buckets, keys, and values{{#2.0.0+}}, with [[bucket types|Using Bucket Types]] acting as an additional namespace in versions 2.0 and greater{{/2.0.0+}}. Values (or objects) are identifiable by a unique key, and each key/value pair is stored in a bucket.{{#2.0.0+}} The [[properties|Buckets]] of each bucket are determined by its bucket type.{{/2.0.0+}}

Buckets are essentially a flat namespace in Riak. You can name them whatever you'd like. They have no intrinsic significant beyond allowing you to store objects with the same key in different buckets. They do, however, enable you to provide common configurations to the keys and values within them, such as [[replication properties]] and [[commit hooks|Using Commit Hooks]].

Most of the interactions you'll have with Riak will be setting or retrieving the value of a key. Riak has [[supported client libraries|Client Libraries]] for Erlang, Java, PHP, Python, Ruby and C/C++. In addition, there are [[community-supported projects|Client Libraries#Community-Libraries]] for .NET, Node.js, Python, Perl, Clojure, Scala, Smalltalk, and many others.

### Read an Object

Here is the basic command form for retrieving a specific key from a bucket:

{{#2.0.0-}}

```
GET /buckets/BUCKET/keys/KEY
```
{{/2.0.0-}}
{{#2.0.0+}}

```
GET /types/TYPE/buckets/BUCKET/keys/KEY
```
{{/2.0.0+}}

Here is an example of a GET performed on the key `mykey` in the bucket `mybucket`{{#2.0.0+}}, which bears the type `mytype`{{/2.0.0+}}:

{{#2.0.0-}}

```curl
curl http://localhost:8098/buckets/mybucket/keys/mykey
```

```ruby
bucket = client.bucket('mybucket')
obj = bucket.get('mykey')
```

```python
bucket = client.bucket('mybucket')
obj = bucket.get('mykey')
```

```java
Bucket myBucket = client.fetchBucket("mybucket").execute();
String key = "mykey";
IRiakObject obj = myBucket.fetch(key).execute();
```
{{/2.0.0-}}
{{#2.0.0+}}

```curl
curl http://localhost:8098/types/mytype/buckets/mybucket/keys/mykey
```

```ruby
bucket = client.bucket('mybucket')
obj = bucket.get('mykey', bucket_type: 'mytype')
```
{{/2.0.0+}}

If there is no object stored under that particular key, Riak will return a message indicating that the object doesn't exist.

```curl
not found
```

```ruby
Riak::ProtobuffsFailedRequest: Expected success from Riak but received not_found. The requested object was not found.
```

```python
# In the Python, this operation will succeed and won't throw an error
# The object 'obj', however, will be empty
```

```java
/* 
  In the Java client, this operation will not throw an error, but the returned object will be null
*/
```

If you're using HTTP to interact with Riak, as opposed to using a client, Riak understands many HTTP-defined headers, such as `Accept` for content-type negotiation, which is relevant when dealing with siblings (see [[the sibling examples for the HTTP API|HTTP Fetch Object#Siblings-examples]]), and `If-None-Match`/`ETag` and `If-Modified-Since`/`Last-Modified` for conditional requests.

Riak also accepts many query parameters, including `r` for setting the R-value for GET requests (R values describe how many replicas need to agree when retrieving an existing object in order to return a successful response). If you omit the the `r` query parameter, Riak defaults to `r=2`.

Here is an example of attempting a read with `r` set to `3`:

{{#2.0.0-}}

```curl
curl http://localhost:8098/buckets/mybucket/keys/mykey?r=3
```

```ruby
bucket = client.bucket('mybucket')
obj = bucket.get('mykey', r: 3)
```

```python
bucket = client.bucket('mybucket')
obj = bucket.get('mykey', r=3)
```

```java
Bucket myBucket = client.fetchBucket("mybucket").execute();
String key = "mykey";
IRiakObject obj = myBucket.fetch(key).r(3).execute();
```
{{/2.0.0-}}
{{#2.0.0+}}

```curl
curl http://localhost:8098/types/mytype/buckets/mybucket/keys/mykey?r=3
```

```ruby
bucket = client.bucket('mybucket')
obj = bucket.get('mykey', r: 3, bucket_type: 'mytype')
```
{{/2.0.0+}}

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

With that in mind, try this command. This will attempt to read the key `doc2` from the bucket `test`{{#2.0.0+}}, which bears the type `mytype`{{/2.0.0+}}:

{{#2.0.0-}}
```curl
curl -v http://localhost:8098/buckets/test/keys/doc2

# Response
404 Not Found
```

```ruby
bucket = client.bucket('test')
bucket.get('doc2')

# Response
Riak::ProtobuffsFailedRequest: Expected success from Riak but received not_found. The requested object was not found.
```

```python
bucket = client.bucket('test')
bucket.get('doc2')

# Response
# This will return an empty RiakObject
<class 'riak.riak_object.RiakObject'>
```

```java
// import com.basho.riak.client.*

Bucket testBucket = client.fetchBucket("test");
String key = "doc2";
IRiakObject obj = testBucket.fetch(key).execute();
```
{{/2.0.0-}}
{{#2.0.0+}}

```curl
curl -v http://localhost:8098/types/mytype/buckets/test/keys/doc2

# Response
404 Not Found
```

```ruby
bucket = client.bucket('test')
bucket.get('doc2', bucket_type: 'mytype')

# Response
Riak::ProtobuffsFailedRequest: Expected success from Riak but received not_found. The requested object was not found.
```
{{/2.0.0+}}

This should return some form of `not found` response, as the key `doc2` does not exist (because you haven't created it yet).

### Store an Object

Your application will often have its own method of generating the keys for its data, e.g. on the basis of timestamps. If so, storing that data is easy. The basic request looks like this.

{{#2.0.0-}}

```
PUT /buckets/BUCKET/keys/KEY
```
{{/2.0.0-}}
{{#2.0.0+}}

```
PUT /types/TYPE/buckets/BUCKET/keys/KEY
```
{{/2.0.0+}}

<div class="info">
If you're using HTTP, <tt>POST</tt> is also a valid method, for compatibility's sake.
</div>

There is no need to intentionally create buckets in Riak. They pop into existence when keys are added to them, and disappear when all keys have been removed from them.

If you're using HTTP, some request headers are required for writes:

* `Content-Type` must be set for the stored object. Set what you expect to receive back when next requesting it.
* `X-Riak-Vclock` if the object already exists. The vector clock is attached to the object when it is read. If the object is new, this header may be omitted.

Here is an example of storing an object (just a snippet of text) under the key `mykey` in the bucket `mybucket`{{#2.0.0+}}, which bears the type `mytype`,{{/2.0.0+}} and passing that object's vector clock to Riak as part of the request:

{{#2.0.0-}}

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -H "X-Riak-Vclock: a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkymNl+GpUfYYvCwA=" \
  -d "some text" \
  http://localhost:8098/buckets/mybucket/keys/mykey
```

```ruby
obj = Riak::RObject.new('mybucket', 'mykey')
obj.content_type = 'text/plain'
obj.vclock = 'a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkymNl+GpUfYYvCwA='
obj.raw_data = 'some text'
obj.store
```

```python
obj = RiakObject('mybucket', 'mykey')
obj.content_type = 'text/plain'
obj.vclock = 'a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkymNl+GpUfYYvCwA='
obj.data = 'some text'
obj.store()
```

```java
Bucket testBucket = client.fetchBucket("mybucket").execute();
String key = "mykey";
String data = "some text";
byte[] vClock = javax.xml.bind.DatatypeConverter.parseBase64Binary("a85hYGBgzGDKBVIcypz/fgb9miicwZTImMfKINjfeYYvCwA=");
IRiakObject obj = RiakObjectBuilder
        .newBuilder(testBucket.getName(), key)
        .withValue(data)
        .withVClock(vClock)
        .build();
testBucket.store(key, obj).execute();

/* While you *can* pass in a raw vector clock in the Java client, the
   code above shows that that can get messy. It's recommendable that
   you use the @RiakVClock annotation to store the object's vector clock
   upon read, as demonstrated in this example:

   https://github.com/basho/riak-java-client/wiki/Storing-data-in-riak
*/
```
{{/2.0.0-}}
{{#2.0.0+}}

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -H "X-Riak-Vclock: a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkymNl+GpUfYYvCwA=" \
  -d "some text" \
  http://localhost:8098/types/mytype/buckets/mybucket/keys/mykey
```

```ruby
obj = Riak::RObject.new('test_bucket', 'test_key')
obj.content_type = 'text/plain'
obj.vclock = 'a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkymNl+GpUfYYvCwA='
obj.raw_data = 'some text'
obj.store(bucket_type: 'mytype')
```
{{/2.0.0+}}

Other HTTP request headers are optional for writes (and not necessary if using a client library):

* `X-Riak-Meta-YOUR_HEADER` for any additional metadata headers that should be stored with the object (eg. `X-Riak-Meta-FirstName`).
* `Link` user and system-defined links to other resources. Read more about [[Links]]. {{2.0.0-}}

Similar to how read requests support the `r` query parameter, write requests also support the following parameters:

Parameter | Default | Description
:---------|:--------|:-----------
`r` | `2` | How many replicas need to agree when retrieving an existing object before the write
`w` | `2` | How many replicas to write to before returning a successful response
`dw` | `0` | How many replicas to commit to durable storage before returning a successful response
`returnbody` | `false` | Whether to return the contents of the stored object

Here is an example of storing an object (another brief text snippet) under the key `test_key` in the bucket `test_bucket`{{#2.0.0+}}, which bears the type `test_type`{{/2.0.0+}}, with `w` set to 3 and `returnbody` set to `true`:

{{#2.0.0-}}

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "some text" \
  http://localhost:8098/buckets/test_bucket/keys/test_key?w=3&returnbody=true
```

```ruby
bucket = client.bucket('test_bucket')
obj = Riak::RObject.new(bucket, 'test_key')
obj.content_type = 'text/plain'
obj.raw_data = 'some text'
obj.store(w: 3, returnbody: true)
```

```python
bucket = client.bucket('test_bucket')
obj = RiakObject(bucket, 'test_key')
obj.content_type = 'text/plain'
obj.data = 'some text'
obj.store(w=3, return_body=True)
```

```java
Bucket testBucket = client.fetchBucket("test_bucket").execute();
String key = "test_key";
String data = "some text";
testBucket.store(key, data).w(3).returnBody(true).execute();
```
{{/2.0.0-}}
{{#2.0.0+}}

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "some text" \
  http://localhost:8098/types/test_type/buckets/test_bucket/keys/test_key?w=3&returnbody=true
```

```ruby
bucket = client.bucket('test_bucket')
obj = Riak::RObject.new(bucket, 'test_key')
obj.content_type = 'text/plain'
obj.raw_data = 'some text'
obj.store(bucket_type: 'test_type', w: 3, returnbody: true)
```
{{/2.0.0+}}

Normal HTTP status codes (responses will vary for Riak client libraries):

* `200 OK`
* `204 No Content`
* `300 Multiple Choices`

If `returnbody` is set to `true`, any of the response headers expected from a read request may be present. Like a `GET` request, `300 Multiple Choices` may be returned if siblings existed or were created as part of the operation, and the response can be dealt with similarly.

Let's give it a shot. Try running this in a terminal.

```curl
curl -v -XPUT http://localhost:8098/buckets/test/keys/doc?returnbody=true \
  -H "X-Riak-Vclock: a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==" \
  -H "Content-Type: application/json" \
  -d '{"bar":"baz"}'
```

```ruby
bucket = client.bucket('test')
obj = Riak::RObject.new(bucket, 'doc')
obj.content_type = 'application/json'
obj.raw_data = '{"bar":"baz"}'
obj.vclock = 'a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA=='
obj.store
```

```python
bucket = client.bucket('test')
obj = RiakObject(bucket, 'doc')
obj.content_type = 'application/json'
obj.data = '{"bar":"baz"}'
obj.vclock = 'a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA=='
obj.store()
```

```java
Bucket testBucket = client.fetchBucket("test").execute();
String key = "doc";
String rawJsonData = "{\"bar\":\"baz\"}";
byte[] vClock = javax.xml.bind.DatatypeConverter.parseBase64Binary("a85hYGBgzGDKBVIcypz/fgb9miicwZTImMfKINjfeYYvCwA=");
IRiakObject obj = RiakObjectBuilder
        .newBuilder(testBucket.getName(), key)
        .withValue(rawJsonData)
        .withVClock(vClock)
        .build();
testBucket.store(key, rawJsonData).execute();

/* While you *can* pass in a raw vector clock in the Java client, the
   code above shows that that can get messy. It's recommendable that
   you use the @RiakVClock annotation to store the object's vector clock
   upon read, as demonstrated in this example:

   https://github.com/basho/riak-java-client/wiki/Storing-data-in-riak
*/
```

### Store a New Object and Assign Random Key

If your application would rather leave key-generation up to Riak, issue a `POST` request to the bucket URL instead of a PUT to a bucket/key pair:

{{#2.0.0-}}

```
POST /buckets/BUCKET/keys
```
{{/2.0.0-}}
{{#2.0.0+}}

```
POST /types/TYPE/buckets/BUCKET/keys
```
{{/2.0.0+}}

If you don't pass Riak a `key` name after the bucket, it will know to create one for you.

Supported headers are the same as for bucket/key write requests, though `X-Riak-Vclock` will never be relevant for these POST requests.  Supported query parameters are also the same as for bucket/key PUT requests.

Normal status codes:

* `201 Created`

This command will store an object in the bucket `test` and assign it a key:

{{#2.0.0-}}

```curl
curl -v -XPOST \
  -H "Content-Type: text/plain" \
  -d "this is a test" \
  http://localhost:8098/buckets/test/keys

# In the output, you should see a Location header that will give you the
# location of the object in Riak, with the key at the end:

Location: /buckets/test/keys/G7FYUXtTsEdru4NP32eijMIRK3o
```

```ruby
bucket = client.bucket('test')
obj = Riak::RObject.new(bucket)
obj.content_type = 'text/plain'
obj.raw_data = 'this is a test'
obj.store
obj.key
#=> "YQGpaKH9n9rdAeeCgB0FJAY7Zi7" or something along those lines
```

```python
bucket = client.bucket('test')
obj = RiakObject(client, bucket)
obj.content_type = 'text/plain'
obj.data = 'this is a test'
obj.store()
obj.key
# 'MCGbrINPY0Z0V4oTm7xeTlDnUr4' or something along those lines
```

```java
Bucket testBucket = client.fetchBucket("test").execute();
IRiakObject objWithoutKey = RiakObjectBuilder
        .newBuilder(testBucket.getName(), null)
        .withContentType("text/plain")
        .withValue("this is a test")
        .build();
IRiakObject objWithRandomKey = testBucket.store(null, data)
        .withoutFetch()
        .returnBody(true)
        .execute();
System.out.println(objWithRandomKey.getKey());
```
{{/2.0.0-}}

{{#2.0.0+}}

```curl
curl -v -XPOST \
  -H "Content-Type: text/plain" \
  -d "this is a test" \
  http://localhost:8098/types/type/buckets/test/keys

# In the output, you should see a Location header that will give you the
# location of the object in Riak, with the key at the end:

Location: /buckets/test/keys/G7FYUXtTsEdru4NP32eijMIRK3o
```

```ruby
bucket = client.bucket('test')
obj = Riak::RObject.new(bucket)
obj.content_type = 'text/plain'
obj.raw_data = 'this is a test'
obj.store(bucket_type: 'type')
obj.key
#=> "YQGpaKH9n9rdAeeCgB0FJAY7Zi7" or something along those lines
```
{{/2.0.0+}}

### Delete an Object

The delete command follows a predictable pattern and looks like this:

{{#2.0.0-}}

```
DELETE /buckets/BUCKET/keys/KEY
```
{{/2.0.0-}}
{{#2.0.0+}}

```
DELETE /types/TYPE/buckets/BUCKET/keys/KEY
```
{{/2.0.0+}}

The normal response codes for a `DELETE` operations are `204 No Content` and `404 Not Found`

404 responses are *normal*, in the sense that `DELETE` operations are idempotent and not finding the resource has the same effect as deleting it.

Try this:

{{#2.0.0-}}

```curl
curl -v -XDELETE http://localhost:8098/buckets/test/keys/test2
```

```ruby
bucket = client.bucket('test')
bucket.delete('test2')
```

```python
bucket = client.bucket('test')
bucket.delete('test2')
```

```java
Bucket testBucket = client.fetchBucket("test").execute();
String key = "test2";
testBucket.delete(key);
```
{{/2.0.0-}}
{{#2.0.0+}}

```curl
curl -v -XDELETE http://localhost:8098/types/test/buckets/test/keys/test2
```

```ruby
bucket = client.bucket('test')
bucket.delete('test2', bucket_type: 'test')
```
{{/2.0.0+}}

## Bucket Properties and Operations

Buckets are essentially a flat namespace in Riak. They allow the same key name to exist in multiple buckets and provide some per-bucket configurability.

<div class="info">
<div class="title">How Many Buckets Can I Have?</div>
Buckets come with virtually no cost <em>except for when you modify the default bucket properties</em>. Modified bucket properties are gossiped around the cluster and therefore add to the amount of data sent around the network. In other words, buckets using the default bucket properties are free.
</div>

### Setting a Bucket's Properties

In addition to providing a namespace for keys, the properties of a bucket also define some of the behaviors that Riak will implement for the values stored in that bucket.

To set these properties, issue a `PUT` to the bucket's URL:

```
PUT /buckets/BUCKET/props
```

The body of the request should be a JSON object with a single entry "props".  Unmodified bucket properties may be omitted.

Important headers:

* `Content-Type: application/json`

The most important properties to consider for your bucket are:

* `n_val` (defaults to `3`) the number of replicas for objects in this bucket;
  `n_val` should be an integer greater than 0 and less than the number of partitions in the ring.
  <div class="note">If you change <code>n_val</code> after keys have been added to the bucket it may result in failed reads. The new value may not be replicated to all of the appropriate partitions.</div>

* `allow_mult` (boolean, defaults to `false`) With `allow_mult` set to `false`, clients will only get the most-recent-by-timestamp object. Otherwise, Riak maintains any sibling objects caused by concurrent writes (or network partitions).

{{#2.0.0-}}

Let's go ahead and alter the properties of a bucket. The following `PUT` will create a new bucket called `test` with a modified `n_val` of `5`.

```curl
curl -v -XPUT http://localhost:8098/buckets/test/props \
  -H "Content-Type: application/json" \
  -d '{"props":{"n_val":5}}'
```

```ruby
bucket = client.bucket 'test'
bucket.n_val = 5
```

```python
bucket = client.bucket('test')
bucket.n_val = 5
```

```java
Bucket testBucket = client.createBucket("test").nVal(5);
```
{{/2.0.0-}}
{{#2.0.0+}}

# TODO

{{/2.0.0+}}

### Fetching Bucket Properties

Here is how you use the [[HTTP API]] to retrieve (or `GET`) the bucket properties and/or keys:

```
GET /buckets/BUCKET/props
```

The optional query parameters are:

* `props`: `true`|`false` - whether to return the bucket properties (defaults to `true`)
* `keys`: `true`|`false`|`stream` - whether to return the keys stored in the bucket (defaults to `false`); see the [[HTTP API's list keys|HTTP List Keys]] for details about dealing with a `keys=stream` response

With that in mind, go ahead and run this command. This will `GET` the bucket information that we just set with the sample command above:

```curl
curl -v http://localhost:8098/buckets/test/props
```

```ruby
bucket = client.bucket 'test'
bucket.properties
```

```python
bucket = client.bucket('test')
bucket.get_properties()
```

```java
Bucket testBucket = client.fetchBucket("test").execute();

/*
The Java client does not allow you to fetch all bucket properties at once.
Instead, you can fetch them one at a time, as below:
*/
testBucket.getNVal();
testBucket.getPR();
testBucket.getLastWriteWins();

// and so on
```

You can also view a bucket information's through any browser by going to `http://localhost:8098/buckets/<bucket>/props`. {{#2.0.0+}}You can see which bucket properties are associated with a specific bucket type in the browser as well, by going to `http://localhost:8098/types/<type>/props`.{{/2.0.0+}}

So, that's the basics of how the most essential Riak key/value operations work, along with some sample code from a few of our official Riak [[client libraries]]. In addition to this tutorial, an in-depth reading of the HTTP API page is highly recommended, as it will give you details on the headers, parameters, status, and other details that you should keep in mind, even when using a client library.
