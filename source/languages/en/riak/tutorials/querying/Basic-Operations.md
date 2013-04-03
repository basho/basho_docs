---
title: Basic Requests
project: riak
version: 0.10.0+
document: tutorials
toc: true
audience: beginner
keywords: [querying, api, http]
prev: "[[Querying Riak]]"
up:   "[[Querying Riak]]"
next: "[[MapReduce]]"
---

Most of the interactions you'll have with Riak will be setting or retrieving the value of a key. This section describes how to do that using the Riak [[HTTP API]], but the concepts apply equally to Riak's [[Protocol Buffers Client API|PBC API]] interface.

Riak has [[supported client libraries|Client Libraries]] for Erlang, Java, PHP, Python, Ruby and C/C++. In addition, there are [[community-supported projects|Community Developed Libraries and Projects]] for .NET, Node.js, Python (and Twisted), Griffon, Small Talk, Perl, Scala, Clojure, and many others.

## Best Practices for HTTP

When sending requests to Riak via HTTP, it's useful to keep these points in mind.

1. All updates to existing objects should include the *X-Riak-Vclock* header that was retrieved from Riak along with the object (see [[Vector Clocks]]).
2. Buckets, keys, and link specifications may not contain unescaped slashes. Use a URL-escaping library or replace slashes with `%2F`.

## Read an Object

Here is the basic command formation for retrieving a specific key from a bucket.

```
GET /riak/bucket/key
```

The body of the response will contain the contents of the object (if it exists).

Riak understands many HTTP-defined headers, like `Accept` for content-type negotiation (relevant when dealing with siblings, see [[the sibling examples for the HTTP API|HTTP Fetch Object#Siblings-examples]], and `If-None-Match`/`ETag` and `If-Modified-Since`/`Last-Modified` for conditional requests.

Riak also accepts many query parameters, including `r` for setting the R-value for this GET request (R Values describe how many replicas need to agree when retrieving an existing object in order to return a successful response. R values will be explained more in the final section of the Fast Track Tutorial). If you omit the the `r` query parameter, Riak defaults to `r=2`.

Normal response codes:

* `200 OK`
* `300 Multiple Choices`
* `304 Not Modified`

Typical error codes:

* `404 Not Found`

### Example Error Code

This command will request (GET) the key `doc2` from the bucket `test`.

```bash
$ curl -v http://127.0.0.1:8091/riak/test/doc2
```

This should return a *404 Not Found* as the key `doc2` does not exist (you haven't created it yet).

## Store an object with existing or user-defined key

Your application will often have its own method of generating the keys for its data.  If so, storing that data is easy.  The basic request looks like this.

```
PUT /riak/bucket/key
```

Remember, buckets are automatically created when you add keys to them. There is no need to explicitly "create" a bucket (more on buckets and their properties further down the page.)

Some request headers are required for PUTs:

* `Content-Type` must be set for the stored object. Set what you expect to receive back when next requesting it.
* `X-Riak-Vclock` if the object already exists, the vector clock attached to the object when read; if the object is new, this header may be omitted

Other request headers are optional for PUTs:

* `X-Riak-Meta-YOUR_HEADER` any additional metadata headers that should be stored with the object.
* `Link` user and system-defined links to other resources. Read more about [[Links]].

Similar to how GET requests support the `r` query parameter, PUT requests also support these parameters:

* `r` how many replicas need to agree when retrieving an existing object before the write *(integer, default is 2)*
* `w` how many replicas to write to before returning a successful response *(integer, default is 2)*
* `dw` how many replicas to commit to durable storage before returning a successful response *(integer, default is 0)*
* `returnbody` whether to return the contents of the stored object *(boolean, default is false)*

Normal status codes:

* `200 OK`
* `204 No Content`
* `300 Multiple Choices`

### Example

If `returnbody=true`, any of the response headers expected from a GET request may be present. Like a GET request, `300 Multiple Choices` may be returned if siblings existed or were created as part of the operation, and the response can be dealt with similarly.

Try running this in a terminal.

```bash
$ curl -v -XPUT -d '{"bar":"baz"}' -H "Content-Type: application/json" \
  -H "X-Riak-Vclock: a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==" \
  http://127.0.0.1:8091/riak/test/doc?returnbody=true
```

## Store a new object and assign random key

If your application would rather leave key-generation up to Riak, issue a POST request to the bucket URL instead of a PUT to a bucket/key pair:

```
POST /riak/bucket
```

If you don't pass Riak a "key" name after the bucket, it will know to create one for you.

Supported headers are the same as for bucket/key PUT requests, though *X-Riak-Vclock* will never be relevant for these POST requests.  Supported query parameters are also the same as for bucket/key PUT requests.

Normal status codes:

* `201 Created`

This command will store an object, in the bucket "test" and assign it a key:

```bash
$ curl -v -d 'this is a test' -H "Content-Type: text/plain" \
  http://127.0.0.1:8091/riak/test
```

In the output, the *Location* header will give the you key for that object. To view the newly created object, go to `http://127.0.0.1:8091/*_Location_*` in your browser.

If you've done it correctly, you should see the value (which is "this is a test").

## Delete an object

Lastly, you'll need to know how to delete keys.

The command, as you can probably guess, follows a predictable pattern and looks like this:

```
DELETE /riak/bucket/key
```

Normal status codes:

* `204 No Content`
* `404 Not Found`

404 responses are _normal_ in the sense that DELETE operations are idempotent and not finding the resource has the same effect as deleting it.

```bash
$ curl -v -X DELETE http://127.0.0.1:8091/riak/test/test2
```
