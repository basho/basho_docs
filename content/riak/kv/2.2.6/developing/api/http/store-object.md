---
title: "HTTP Store Object"
description: ""
project: "riak_kv"
project_version: "2.2.6"
menu:
  riak_kv-2.2.6:
    name: "Store Object"
    identifier: "http_store_object"
    weight: 106
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.2.6/dev/references/http/store-object
  - /riak/kv/2.2.6/dev/references/http/store-object
---

Stores an object under the specified bucket / key. Storing an object comes in
two forms, depending on whether you want to use a key of your choosing, or let
Riak assign a key to a new object.

## Request

```bash
POST /types/type/buckets/bucket/keys       # Riak-defined key
PUT /types/type/buckets/bucket/keys/key    # User-defined key
POST /buckets/bucket/keys                  # Riak-defined key
PUT /buckets/bucket/keys/key               # User-defined key
```

For the sake of compatibility with older clients, `POST` is also acceptable in
the form where the key is specified.

Important headers:

* `Content-Type` must be set for the stored object. Set what you expect to
receive back when next requesting it.
* `X-Riak-Vclock` if the object already exists, the vector clock attached to the
object when read.
* `X-Riak-Meta-*` - any additional metadata headers that should be stored with
the object.
* `X-Riak-Index-*` - index entries under which this object should be indexed.
[Read more about Secondary Indexing]({{<baseurl>}}riak/kv/2.2.6/developing/api/http/secondary-indexes)
* `Link` - user and system-defined links to other resources. [Read more about Links.]({{<baseurl>}}riak/kv/2.2.6/developing/api/http/link-walking)

Optional headers (only valid on `PUT`):

* `If-None-Match`, `If-Match`, `If-Modified-Since`, and `If-Unmodified-Since`
invoke conditional request semantics, matching on the `ETag` and `Last-Modified`
of the existing object.  These can be used to prevent overwriting a modified
object.  If the test fails, you will receive a `412 Precondition Failed`
response. This does not prevent concurrent writes; it is possible for the
condition to evaluate to true for multiple requests if the requests occur at the
same time.

Optional query parameters:

* `w` (write quorum) how many replicas to write to before returning a successful
response (default is defined by the bucket level)
* `dw` (durable write quorum) how many replicas to commit to durable storage
before returning a successful response (default is defined at the bucket level)
* `pw` how many primary replicas must be online to attempt a write (default is
defined at the bucket level)
* `returnbody=[true|false]` whether to return the contents of the stored object.

*<ins>This request must include a body (entity).</ins>*

## Response

Normal status codes:

* `201 Created` (when submitting without a key)
* `200 OK`
* `204 No Content`
* `300 Multiple Choices`

Typical error codes:

* `400 Bad Request` - e.g. when r, w, or dw parameters are invalid (> N)
* `412 Precondition Failed` if one of the conditional request headers failed to
match (see above)

Important headers:

* `Location` a relative URL to the newly-created object (when submitting without
a key)

If `returnbody=true`, any of the response headers expected from [HTTP Fetch Object]({{<baseurl>}}riak/kv/2.2.6/developing/api/http/fetch-object) may be present. Like when fetching the object, `300 Multiple Choices`
may be returned if siblings existed or were created as part of the operation,
and the response can be dealt with similarly.

## Example: Storing Without Key

```curl
$ curl -v http://127.0.0.1:8098/buckets/test/keys \
       -H "Content-Type: text/plain" -d 'this is a test'
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> POST /buckets/test/keys HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
> Content-Type: text/plain
> Content-Length: 14
>
< HTTP/1.1 201 Created
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Location: /buckets/test/keys/bzPygTesROPtGGVUKfyvp2RR49
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/json
< Content-Length: 0
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
```

## Example: Storing With Key

```curl
$ curl -v -XPUT -d '{"bar":"baz"}' -H "Content-Type: application/json" -H "X-Riak-Vclock: a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==" http://127.0.0.1:8098/buckets/test/keys/doc?returnbody=true
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> PUT /buckets/test/keys/doc?returnbody=true HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
> Content-Type: application/json
> X-Riak-Vclock: a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==
> Content-Length: 13
>
< HTTP/1.1 200 OK
< X-Riak-Vclock: a85hYGBgymDKBVIszMk55zKYEhnzWBlKIniO8kGF2TyvHYIKfwcJZwEA
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Link: </buckets/test>; rel="up"
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/json
< Content-Length: 13
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
{"bar":"baz"}
```
