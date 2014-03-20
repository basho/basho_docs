---
title: The Basics
project: riak
version: 2.0.0+
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

Buckets are essentially a flat namespace in Riak. You can name them whatever you'd like. They have no intrinsic significance beyond allowing you to store objects with the same key in different buckets. They do, however, enable you to provide common configurations to the keys and values within them, such as [[replication properties]] and [[commit hooks|Using Commit Hooks]]. Such [[properties|Buckets]] are managed using bucket types.

Most of the interactions you'll have with Riak will involve setting or retrieving the value of a key. Riak has [[supported client libraries|Client Libraries]] for Erlang, Java, PHP, Python, Ruby and C/C++. In addition, there are [[community-supported projects|Client Libraries#Community-Libraries]] for .NET, Node.js, Python, Perl, Clojure, Scala, Smalltalk, and many others.

### Read an Object

Here is the basic command form for retrieving a specific key from a bucket:

```
GET /types/TYPE/buckets/BUCKET/keys/KEY
```

Here is an example of a GET performed on the key `mykey` in the bucket `mybucket`, which bears the type `mytype`:

```python
bucket = client.bucket('mybucket', bucket_type='mytype')
obj = bucket.get('mykey')
```

```erlang
{ok, Obj} = riakc_pb_socket(Pid, {<<"mytype">>, <<"test_bucket">>}, <<"test_key">>).
```

```curl
curl http://localhost:8098/types/mytype/buckets/mybucket/keys/mykey
```

If there is no object stored under that particular key, Riak will return a message indicating that the object doesn't exist.

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

Riak also accepts many query parameters, including `r` for setting the R-value for GET requests (R values describe how many replicas need to agree when retrieving an existing object in order to return a successful response). If you omit the the `r` query parameter, Riak defaults to `r=2`.

Here is an example of attempting a read with `r` set to `3`:

```python
bucket = client.bucket('mybucket', bucket_type='mytype')
obj = bucket.get('mykey', r=3)
obj.data
```

```erlang
{ok, Obj} = riakc_pb_socket:get(Pid, {<<"mytype">>, <<"mybucket">>}, <<"mykey">>, [{r, 3}]).
```

```curl
curl http://localhost:8098/types/mytype/buckets/mybucket/keys/mykey?r=3
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

With that in mind, try the following operation, which will attempt to read the key `doc2` from the bucket `test`, which bears the type `mytype`:

```python
riak.RiakError: 'no_type'
```

```erlang
{error,notfound}
```

```curl
curl http://localhost:8098/types/mytype/buckets/mybucket/keys/mykey

# Response
404 Not Found
```

This operation should return some form of `not found` response because the key `doc2` does not yet exist (as you haven't created it yet).

### Store an Object

Your application will often have its own method of generating the keys for its data, e.g. on the basis of timestamps. If so, storing that data is easy. The basic request looks like this.

```
PUT /types/TYPE/buckets/BUCKET/keys/KEY
```

<div class="info">
If you're using HTTP, <tt>POST</tt> is also a valid method, for compatibility's sake.
</div>

There is no need to intentionally create buckets in Riak. They pop into existence when keys are added to them, and disappear when all keys have been removed from them. If you don't specify a bucket's type, the type `[[default|Using Bucket Types]]` will be applied.

If you're using HTTP, some request headers are required for writes:

* `Content-Type` must be set for the stored object. Set what you expect to receive back when next requesting it.
* `X-Riak-Vclock` if the object already exists. The [[vector clock|Vector Clocks]] is attached to the object when it is read. If the object is new, this header may be omitted.

Here is an example of storing an object (just a snippet of text) under the key `mykey` in the bucket `mybucket`, which bears the type `mytype`, and passing that object's vector clock to Riak as part of the request:

```python
bucket = client.bucket('mybucket', bucket_type='mytype')
obj = RiakObject(client, bucket, 'mykey')
obj.content_type = 'text/plain'
obj.data = 'some text'
obj.store()
```

```erlang
Object = riakc_obj:new({<<"mytype">>, <<"mybucket">>}, <<"mykey">>, <<"some text">>, <<"text/plain">>).
riakc_pb_socket:put(Pid, Object).
```

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -H "X-Riak-Vclock: a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkymNl+GpUfYYvCwA=" \
  -d "some text" \
  http://localhost:8098/types/mytype/buckets/mybucket/keys/mykey
```

Other HTTP request headers are optional for writes (and not necessary if using a client library):

* `X-Riak-Meta-YOUR_HEADER` for any additional metadata headers that should be stored with the object (eg. `X-Riak-Meta-FirstName`).

Similar to how read requests support the `r` query parameter, write requests also support the following parameters:

Parameter | Default | Description
:---------|:--------|:-----------
`r` | `2` | How many replicas need to agree when retrieving an existing object before the write
`w` | `2` | How many replicas to write to before returning a successful response
`dw` | `0` | How many replicas to commit to durable storage before returning a successful response
`returnbody` | `false` | Whether to return the contents of the stored object

Here is an example of storing an object (another brief text snippet) under the key `test_key` in the bucket `test_bucket`, which bears the type `test_type`, with `w` set to `3` and `returnbody` set to `true`:

```python
bucket = client.bucket('test_bucket', bucket_type='test_type')
obj = RiakObject(client, bucket, 'test_key')
obj.content_type = 'text/plain'
obj.data = 'some text'
obj.store(w=3, return_body=True)
```

```erlang
Object = riakc_obj:new({<<"test_type">>, <<"test_bucket">>}, <<"test_key">>, <<"some text">>, <<"text/plain">>).
riakc_pb_socket:put(Pid, Object).
```

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "some text" \
  http://localhost:8098/types/test_type/buckets/test_bucket/keys/test_key?w=3&returnbody=true
```

Normal HTTP status codes (responses will vary for client libraries):

* `200 OK`
* `204 No Content`
* `300 Multiple Choices`

If `returnbody` is set to `true`, any of the response headers expected from a read request may be present. Like a `GET` request, `300 Multiple Choices` may be returned if siblings existed or were created as part of the operation, and the response can be dealt with similarly.

Let's give it a shot:

```python
bucket = client.bucket('test', bucket_type='test_type')
obj = RiakObject(client, bucket, 'doc')
obj.content_type = 'application/json'
obj.data = '{"bar":"baz"}'
obj.vclock = 'a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA=='
obj.store(return_body=True)
```

```erlang
Object = riakc_obj:new({<<"test_type">>, <<"test">>}, <<"doc">>, <<"{'bar':'baz'}">>, <<"application/json">>).
ObjectWithVclock = riakc_obj:set_vclock(Object, <<"a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==">>).
riakc_pb_socket:put(Pid, ObjectWithVclock).
```

```curl
curl -v -XPUT \
  -H "X-Riak-Vclock: a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==" \
  -H "Content-Type: application/json" \
  -d '{"bar":"baz"}' \
  http://localhost:8098/types/test_type/buckets/test/keys/doc?returnbody=true
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

This command will store an object in the bucket `test` and assign it a random key:

```python
bucket = client.bucket('test', bucket_type='type')
obj = RiakObject(client, bucket)
obj.content_type = 'text/plain'
obj.data = 'this is a test'
obj.store()

obj.key

# The Python client will assign a random key along the following lines:
'ZPFF18PUqGW9efVou7EHhfE6h8a'
```

```erlang
Object = riakc_obj:new({<<"type">>, <<"test">>}, undefined, <<"this is a test">>, <<"text/plain">>).
riakc_pb_socket:put(Pid, Object).

%% The key can be retrieved from the output of the above call.
%% It will look something like this:

{ok,{riakc_obj,{<<"type">>,<<"test">>},
               <<"EZ7pp4bpdfpZw0fPUdTUafveQjO">>,undefined,[],undefined,
               undefined}}
```

```curl
curl -i -XPOST \
  -H "Content-Type: text/plain" \
  -d "this is a test" \
  http://localhost:8098/types/type/buckets/test/keys

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

Try this:

```python
bucket = client.bucket('mybucket', bucket_type='mytype')
bucket.delete('mykey')
```

```erlang
riakc_pb_socket:delete(Pid, {<<"mytype">>, <<"mybucket">>}, <<"mykey">>)
```

```curl
curl -XDELETE http://localhost:8098/types/mytype/buckets/mybucket/keys/mykey
```

## Bucket Properties and Operations

Buckets are essentially a flat namespace in Riak. They allow the same key name to exist in multiple buckets and enable you to apply configurations across keys.

<div class="info">
<div class="title">How Many Buckets Can I Have?</div>
Buckets come with virtually no cost <em>except for when you modify the default bucket properties</em>. Modified bucket properties are gossiped around the cluster and therefore add to the amount of data sent around the network. In other words, buckets using the <tt>default</tt> bucket type are free. More on that in the next section.
</div>

### Bucket Types

A comprehensive tutorial on bucket types can be found in the [[Using Bucket Types]] document. Here, we'll discuss some basic parameters that can be managed using bucket types.

The most important properties to consider for buckets are the following:

Parameter | Description | Default
:---------|:------------|:-------
`n_val` | The number of replicas for objects in a bucket. The `n_val` should be an integer greater than 0 and less than the number of partitions in the ring.<br /><br />**Note**: If you change the `n_val` after keys have been added to the bucket, it may result in failed reads, as the new value may not be replicated to all of the appropriate partitions.| `3`
`allow_mult` | With `allow_mult` set to `false`, clients will only get the most recent object as determined by timestamp. Otherwise, Riak maintains any sibling objects caused by concurrent writes (or network partitions). | `false`

Let's go ahead and create a bucket type called `test_type` that sets the `n_val` to 5:

```bash
riak-admin bucket-type create test_type '{"props":{"n_val":5}}'
```

We must activate the type for those parameters to take effect:

```bash
riak-admin bucket-type activate test_type
```

Once the type is activated, we can see which properties are associated with our bucket type (and, by extension, any bucket that bears that type):

```python
bt = BucketType(client, 'test_type')
bt.get_properties()
```

```erlang
riakc_pb_socket:get_bucket_type(Pid, <<"test_type">>).
```

```curl
curl http://localhost:8098/types/test_type/props
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

So, that's the basics of how the most essential Riak key/value operations work. In addition to this tutorial, an in-depth reading of the [[HTTP API]] page is highly recommended, as it will give you details on the headers, parameters, status, and other details that you should keep in mind even when using a client library.
