---
title: "HTTP Fetch Object"
description: ""
project: "riak_kv"
project_version: "2.0.1"
menu:
  riak_kv-2.0.1:
    name: "Fetch Object"
    identifier: "http_fetch_object"
    weight: 105
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.0.1/dev/references/http/fetch-object
  - /riak/kv/2.0.1/dev/references/http/fetch-object
---

Reads an object from the specified bucket/key.

## Request

```bash
GET /buckets/bucket/keys/key
```

Important headers:

* `Accept` - When `multipart/mixed` is the preferred content-type, objects with
siblings will return all siblings in single request. See [Siblings examples](#siblings-examples). See
also RFC 2616 - [Accept header definition](http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1).

Optional headers:

* `If-None-Match` and `If-Modified-Since` invoke conditional request semantics,
matching on the `ETag` and `Last-Modified` of the object, respectively.  If the
object fails one of the tests (that is, if the ETag is equal or the object is
unmodified since the supplied timestamp), Riak will return a `304 Not Modified`
response. See also RFC 2616 - [304 Not Modified](http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.5).

Optional query parameters:

* `r` - (read quorum) how many replicas need to agree when retrieving the
object ([default is defined by the bucket]({{<baseurl>}}riak/kv/2.0.1/developing/api/http/set-bucket-props))
* `pr` - how many primary replicas need to be online when doing the read
([default is defined by the bucket]({{<baseurl>}}riak/kv/2.0.1/developing/api/http/set-bucket-props))
* `basic_quorum` - whether to return early in some failure cases (eg. when r=1
and you get 2 errors and a success `basic_quorum=true` would return an error)
([default is defined by the bucket]({{<baseurl>}}riak/kv/2.0.1/developing/api/http/set-bucket-props))
* `notfound_ok` - whether to treat notfounds as successful reads for the
purposes of R ([default is defined by the bucket]({{<baseurl>}}riak/kv/2.0.1/developing/api/http/set-bucket-props))
* `vtag` - when accessing an object with siblings, which sibling to retrieve.
Scroll down to the [Manually requesting siblings](#manually-requesting-siblings) example for more information.

## Response

Normal response codes:

* `200 OK`
* `300 Multiple Choices`
* `304 Not Modified` (when using conditional request semantics)

Typical error codes:

* `400 Bad Request` - e.g. when r parameter is invalid (> N)
* `404 Not Found` - the object could not be found on enough partitions
* `503 Service Unavailable` - the request timed out internally

Important headers:

* `Content-Type` - the media type/format
* `X-Riak-Vclock` - the opaque vector clock for the object
* `X-Riak-Meta-*` - any user-defined metadata defined when storing the object
* `ETag` - the entity tag for the object, useful for conditional GET operations
and validation-based caching
* `Last-Modified` - a timestamp for when the object was last written, in HTTP
datetime format
* `Link` - user- and system-defined links to other resources. [Read more about Links.]({{<baseurl>}}riak/kv/2.0.1/learn/glossary/#links)

The body of the response will be the contents of the object except when siblings
are present.

{{% note title="Siblings" %}}
When `allow_mult` is set to true in the bucket properties, concurrent updates
are allowed to create "sibling" objects, meaning that the object has any
number of different values that are related to one another by the vector
clock.  This allows your application to use its own conflict resolution
technique.

An object with multiple sibling values will result in a `300 Multiple Choices`
response.  If the `Accept` header prefers `multipart/mixed`, all siblings will
be returned in a single request as sections of the `multipart/mixed` response
body.  Otherwise, a list of "vtags" will be given in a simple text format. You
can request individual siblings by adding the `vtag` query parameter. Scroll
down to the 'manually requesting siblings' example below for more information.

To resolve the conflict, store the resolved version with the `X-Riak-Vclock`
given in the response.
{{% /note %}}

## Simple Example

```curl
$ curl -v http://127.0.0.1:8098/buckets/test/keys/doc2
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /buckets/test/keys/doc2 HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
>
< HTTP/1.1 200 OK
< X-Riak-Vclock: a85hYGBgzGDKBVIsbLvm1WYwJTLmsTLcjeE5ypcFAA==
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Link: </buckets/test>; rel="up"
< Last-Modified: Wed, 10 Mar 2010 18:11:41 GMT
< ETag: 6dQBm9oYA1mxRSH0e96l5W
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/json
< Content-Length: 13
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
{"foo":"bar"}
```

## Siblings examples

### Manually requesting siblings

Simple call to fetch an object that has siblings:

```curl
$ curl -v http://127.0.0.1:8098/buckets/test/keys/doc
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /buckets/test/keys/doc HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
>
< HTTP/1.1 300 Multiple Choices
< X-Riak-Vclock: a85hYGDgyGDKBVIszMk55zKYEhnzWBlKIniO8kGF2TyvHYIKf0cIszUnMTBzHYVKbIhEUl+VK4spDFTPxhHzFyqhEoVQz7wkSAGLMGuz6FSocFIUijE3pt5HlsgCAA==
< Vary: Accept, Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: text/plain
< Content-Length: 102
<
Siblings:
16vic4eU9ny46o4KPiDz1f
4v5xOg4bVwUYZdMkqf0d6I
6nr5tDTmhxnwuAFJDd2s6G
6zRSZFUJlHXZ15o9CG0BYl
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
```

Now request one of the siblings directly:

```curl
$ curl -v http://127.0.0.1:8098/buckets/test/keys/doc?vtag=16vic4eU9ny46o4KPiDz1f
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /buckets/test/keys/doc?vtag=16vic4eU9ny46o4KPiDz1f HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
>
< HTTP/1.1 200 OK
< X-Riak-Vclock: a85hYGDgyGDKBVIszMk55zKYEhnzWBlKIniO8kGF2TyvHYIKf0cIszUnMTBzHYVKbIhEUl+VK4spDFTPxhHzFyqhEoVQz7wkSAGLMGuz6FSocFIUijE3pt5HlsgCAA==
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Link: </buckets/test>; rel="up"
< Last-Modified: Wed, 10 Mar 2010 18:01:06 GMT
< ETag: 16vic4eU9ny46o4KPiDz1f
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/x-www-form-urlencoded
< Content-Length: 13
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
{"bar":"baz"}
```

### Get all siblings in one request

```curl
$ curl -v http://127.0.0.1:8098/buckets/test/keys/doc -H "Accept: multipart/mixed"
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /buckets/test/keys/doc HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: multipart/mixed
>
< HTTP/1.1 300 Multiple Choices
< X-Riak-Vclock: a85hYGDgyGDKBVIszMk55zKYEhnzWBlKIniO8kGF2TyvHYIKf0cIszUnMTBzHYVKbIhEUl+VK4spDFTPxhHzFyqhEoVQz7wkSAGLMGuz6FSocFIUijE3pt5HlsgCAA==
< Vary: Accept, Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: multipart/mixed; boundary=YinLMzyUR9feB17okMytgKsylvh
< Content-Length: 766
<

--YinLMzyUR9feB17okMytgKsylvh
Content-Type: application/x-www-form-urlencoded
Link: </buckets/test>; rel="up"
Etag: 16vic4eU9ny46o4KPiDz1f
Last-Modified: Wed, 10 Mar 2010 18:01:06 GMT

{"bar":"baz"}
--YinLMzyUR9feB17okMytgKsylvh
Content-Type: application/json
Link: </buckets/test>; rel="up"
Etag: 4v5xOg4bVwUYZdMkqf0d6I
Last-Modified: Wed, 10 Mar 2010 18:00:04 GMT

{"bar":"baz"}
--YinLMzyUR9feB17okMytgKsylvh
Content-Type: application/json
Link: </buckets/test>; rel="up"
Etag: 6nr5tDTmhxnwuAFJDd2s6G
Last-Modified: Wed, 10 Mar 2010 17:58:08 GMT

{"bar":"baz"}
--YinLMzyUR9feB17okMytgKsylvh
Content-Type: application/json
Link: </buckets/test>; rel="up"
Etag: 6zRSZFUJlHXZ15o9CG0BYl
Last-Modified: Wed, 10 Mar 2010 17:55:03 GMT

{"foo":"bar"}
--YinLMzyUR9feB17okMytgKsylvh--
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
```
