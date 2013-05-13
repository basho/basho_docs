---
title: Basic Riak API Operations
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, fast-track]
prev: "[[Building a Dev Environment|Building a Development Environment]]"
up:   "[[The Riak Fast Track]]"
next: "[[MapReduce Queries|Loading Data and Running MapReduce Queries]]"
versions: false
interest: [
"[[The HTTP API In Depth|HTTP API]]",
"[[Protocol Buffers API|PBC API]]",
"[[Replication in Depth|Replication]]",
"<a href='http://blog.basho.com/2010/01/29/why-vector-clocks-are-easy/'>Why Vector Clocks are Easy</a>",
"<a href='http://blog.basho.com/2010/04/05/why-vector-clocks-are-hard/'>Why Vector Clocks are Hard</a>"
]
---

For this module, we are going to work with the Riak HTTP API.

## Object/Key Operations

Riak organizes data into Buckets, Keys, and Values. Values (or objects) are identifiable by a unique key, and each key/value pair is stored in a bucket. Buckets are essentially a flat namespace in Riak and have little significance beyond their ability to allow the same key name to exist in multiple buckets and to provide some per-bucket configurability for things like replication factor and pre/post-commit hooks.

Most of the interactions you'll have with Riak will be setting or retrieving the value of a key. This section describes how to do that using the Riak HTTP API. We also offer [[supported client libraries|Client Libraries]] for Erlang, Java, PHP, Python, Ruby and C/C++. In addition, there are [[community-supported projects|Community Developed Libraries and Projects]] for .NET, Node.js, Python (and Twisted), Griffon, Small Talk, Perl, Scala, Clojure, and many others.

### Required Knowledge

* All updates to existing objects should include the *X-Riak-Vclock* header that was retrieved from Riak along with the object (see [[Vector Clocks]]).
* Buckets, keys, and link specifications may not contain unescaped slashes. Use a URL-escaping library or replace slashes with `%2F`.

### Read an Object

Here is the basic command formation for retrieving a specific key from a bucket.

```bash
GET /riak/BUCKET/KEY
```

The body of the response will contain the contents of the object (if it exists).

Riak understands many HTTP-defined headers, like *Accept* for content-type negotiation (relevant when dealing with siblings, see [[the sibling examples for the HTTP API|HTTP Fetch Object#Siblings examples]], and *If-None-Match*/*ETag* and *If-Modified-Since*/*Last-Modified* for conditional requests.

Riak also accepts many query parameters, including *r* for setting the R-value for this GET request (R Values describe how many replicas need to agree when retrieving an existing object in order to return a successful response. R values will be explained more in the final section of the Fast Track Tutorial). If you omit the the *r* query parameter, Riak defaults to *r=2*.

Normal response codes:

* *200 OK*
* *300 Multiple Choices*
* *304 Not Modified*

Typical error codes:

* *404 Not Found*

So, with that in mind, try this command. This will request (GET) the key "doc2" from the bucket "test."

```bash
$ curl -v http://127.0.0.1:8091/riak/test/doc2
```

This should return a *404 Not Found* as the key "doc2" does not exist (you haven't created it yet!).

### Store an object with existing or user-defined key

Your application will often have its own method of generating the keys for its data.  If so, storing that data is easy.  The basic request looks like this.

*Note that this is not the only URL format available. Alternate forms can be found in the [[HTTP API]].*

```bash
PUT /riak/BUCKET/KEY
```

<div class="info"><code>POST</code> is also a valid verb, for compatibility's sake.</div>

Remember, buckets are automatically created when you add keys to them. There is no need to explicitly "create" a bucket (more on buckets and their properties further down the page.)

Some request headers are required for PUTs:

* *Content-Type* must be set for the stored object. Set what you expect to receive back when next requesting it.
* *X-Riak-Vclock* if the object already exists, the vector clock attached to the object when read; if the object is new, this header may be omitted

Other request headers are optional for PUTs:

* *X-Riak-Meta-_YourHeader_* any additional metadata headers that should be stored with the object.
* *Link* user and system-defined links to other resources. Read more about [[Links]].

Similar to how GET requests support the "r" query parameter, PUT requests also support these parameters:

* *r* how many replicas need to agree when retrieving an existing object before the write (integer value, default is 2)
* *w* how many replicas to write to before returning a successful response (integer value, default is 2).
* *dw* how many replicas to commit to durable storage before returning a successful response (integer value, default is 0)
* *returnbody* whether to return the contents of the stored object (boolean string, default is "false")

Normal status codes:

* *200 OK*
* *204 No Content*
* *300 Multiple Choices*

If *returnbody=true*, any of the response headers expected from a GET request may be present. Like a GET request, *300 Multiple Choices* may be returned if siblings existed or were created as part of the operation, and the response can be dealt with similarly.

Let's give it a shot. Try running this in a terminal.


```
$ curl -v -XPUT http://127.0.0.1:8091/riak/test/doc?returnbody=true \
  -H "X-Riak-Vclock: a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==" \
  -H "Content-Type: application/json" \
  -d '{"bar":"baz"}'
```

### Store a new object and assign random key

If your application would rather leave key-generation up to Riak, issue a POST request to the bucket URL instead of a PUT to a bucket/key pair:

```bash
POST /riak/BUCKET
```

If you don't pass Riak a "key" name after the bucket, it will know to create one for you.

Supported headers are the same as for bucket/key PUT requests, though *X-Riak-Vclock* will never be relevant for these POST requests.  Supported query parameters are also the same as for bucket/key PUT requests.

Normal status codes:

* *201 Created*

This command will store an object, in the bucket "test" and assign it a key:

```
$ curl -v -XPOST http://127.0.0.1:8091/riak/test \
  -H 'Content-Type: text/plain' \
  -d 'this is a test'
```

In the output, the *Location* header will give the you key for that object. To view the newly created object, go to `http://127.0.0.1:8091/*_Location_*` in your browser.

If you've done it correctly, you should see the value (which is "this is a test").

### Delete an object

Lastly, you'll need to know how to delete keys.

The command, as you can probably guess, follows a predictable pattern and looks like this:

```bash
DELETE /riak/BUCKET/KEY
```

The normal response codes for a DELETE operations are *204 No Content* and *404 Not Found*

404 responses are "normal" in the sense that DELETE operations are idempotent and not finding the resource has the same effect as deleting it.

Try this:

```bash
$ curl -v -XDELETE http://127.0.0.1:8091/riak/test/test2
```

## Bucket Properties and Operations

Buckets are essentially a flat namespace in Riak. They allow the same key name to exist in multiple buckets and provide some per-bucket configurability.

<div class="info"><div class="title">How Many Buckets Can I Have?</div>
Currently, buckets come with virtually no cost except for when you modify the default bucket properties. Modified Bucket properties are gossiped around the cluster and therefore add to the amount of data sent around the network. In other words, buckets using the default bucket properties are free.
</div>

### Setting a bucket's properties

There is no need to "create" buckets in Riak.  They pop into existence when keys are added to them, and disappear when all keys have been removed from them.

However, in addition to providing a namespace for keys, the properties of a bucket also define some of the behaviors that Riak will implement for the values stored in the bucket.

To set these properties, issue a PUT to the bucket's URL:

```bash
PUT /riak/BUCKET
```

The body of the request should be a JSON object with a single entry "props."  Unmodified bucket properties may be omitted.

Important headers:

* Content-Type: *application/json*

The most important properties to consider for your bucket are:

* *n_val* - the number of replicas for objects in this bucket (defaults to 3); *n_val* should be an integer greater than 0 and less than the number of partitions in the ring.

<div class="note">Changing <code>n_val</code> after keys have been added to the bucket is not advisable as it may result in failed reads because the new value may not be replicated to all the appropriate partitions.</div>

* *allow_mult* - _true_ or _false_ (defaults to _false_ ); Riak maintains any sibling objects caused by things like concurrent writes or network partitions. With *allow_mult* set to false, clients will only get the most-recent-by-timestamp object.

Let's go ahead and alter the properties of a Bucket. The following PUT will create a new bucket called "test" with a modified n_val of 5.

```
$ curl -v -XPUT http://127.0.0.1:8091/riak/test \
  -H "Content-Type: application/json" \
  -d '{"props":{"n_val":5}}'
```

### GET Buckets

Here is how you use the HTTP API to retrieve (or "GET") the bucket properties and/or keys:

```bash
GET /riak/BUCKET
```

Again, quite simple. (Are you starting to see a pattern?)

The optional query parameters are:

* `props`=`true`|`false` - whether to return the bucket properties (defaults to `true`)
* `keys`=`true`|`false`|`stream` - whether to return the keys stored in the bucket (defaults to `false`); see the [[HTTP API's list keys|HTTP List Keys]] for details about dealing with a `keys=stream` response


With that in mind, go ahead and run this command. This will GET the bucket information that we just set with the sample command above:


```bash
$ curl -v http://127.0.0.1:8091/riak/test
```

You can also view this Bucket information through any browser by going to `http://127.0.0.1:8091/riak/test`

So, that's the basics of how the HTTP API works. An in depth reading of the HTTP API page (linked below) is highly recommended. This will give you details on the headers, parameters, and status that you should keep in mind when using the HTTP Interface.
