---
title: "HTTP Secondary Indexes"
description: ""
project: "riak_kv"
project_version: 2.9.7
menu:
  riak_kv-2.9.7:
    name: "Secondary Indexes"
    identifier: "http_2i"
    weight: 109
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.9.7/dev/references/http/secondary-indexes
  - /riak/kv/2.9.7/dev/references/http/secondary-indexes
---

[Secondary Indexes]({{<baseurl>}}riak/kv/2.9.7/developing/usage/secondary-indexes) allows an application to tag a Riak object with one or more field/value pairs. The object is indexed under these field/value pairs, and the application can later query the index to retrieve a list of matching keys.

## Request

### Exact Match

```bash
GET /buckets/mybucket/index/myindex_bin/value
```

### Range Query

```
GET /buckets/mybucket/index/myindex_bin/start/end
```

#### Range query with terms

To see the index values matched by the range, use `return_terms=true`.

```
GET /buckets/mybucket/index/myindex_bin/start/end?return_terms=true
```

### Pagination

Add the parameter `max_results` for pagination. This will limit the results and provide for the next request a `continuation` value.

```
GET /buckets/mybucket/index/myindex_bin/start/end?return_terms=true&max_results=500
GET /buckets/mybucket/index/myindex_bin/start/end?return_terms=true&max_results=500&continuation=g2gCbQAAAAdyaXBqYWtlbQAAABIzNDkyMjA2ODcwNTcxMjk0NzM=
```

### Streaming

```
GET /buckets/mybucket/index/myindex_bin/start/end?stream=true
```

## Response

Normal status codes:

+ `200 OK`

Typical error codes:

+ `400 Bad Request` - if the index name or index value is invalid.
+ `500 Internal Server Error` - if there was an error in processing a map or reduce function, or if indexing is not supported by the system.
+ `503 Service Unavailable` - if the job timed out before it could complete

## Example

```curl
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




