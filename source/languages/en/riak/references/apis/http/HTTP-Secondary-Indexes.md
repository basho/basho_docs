---
title: HTTP Secondary Indexes
project: riak
version: 1.0.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Query Operations"
---

[[Secondary Indexes]] allows an application to tag a Riak object with one or more field/value pairs. The object is indexed under these field/value pairs, and the application can later query the index to retrieve a list of matching keys.

## Request

Exact Match:

```bash
GET /buckets/mybucket/index/myindex_bin/value
```

Or Range Query:
```
GET /buckets/mybucket/index/myindex_bin/start/end
```

## Response

Normal status codes:

+ `200 OK`

Typical error codes:

+ `400 Bad Request` - if the index name or index value is invalid.
+ `500 Internal Server Error` - if there was an error in processing a map or reduce function, or if indexing is not supported by the system.
+ `503 Service Unavailable` - if the job timed out before it could complete

## Example

```bash
$ curl -v http://localhost:8098/buckets/mybucket/index/field1_bin/val1
* About to connect() to localhost port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to localhost (127.0.0.1) port 8098 (#0)
> GET /buckets/mybucket/index/field1_bin/val1 HTTP/1.1
> User-Agent: curl/7.19.7 (universal-apple-darwin10.0) libcurl/7.19.7 OpenSSL/0.9.8r zlib/1.2.3
> Host: localhost:8098
> Accept: */*
>
< HTTP/1.1 200 OK
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/json
< Content-Length: 19
<
* Connection #0 to host localhost left intact
* Closing connection #0
{"keys":["mykey1"]}%
```
