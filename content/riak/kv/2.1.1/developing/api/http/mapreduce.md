---
title: "HTTP MapReduce"
description: ""
project: "riak_kv"
project_version: "2.1.1"
menu:
  riak_kv-2.1.1:
    name: "MapReduce"
    identifier: "http_mapreduce"
    weight: 108
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.1.1/dev/references/http/mapreduce
  - /riak/kv/2.1.1/dev/references/http/mapreduce
---

[MapReduce]({{<baseurl>}}riak/kv/2.1.1/developing/usage/mapreduce) is a generic way to query Riak by specifying inputs and constructing a set of map, reduce, and link phases through which data will flow.

## Request

```bash
POST /mapred
```

Important headers:
* `Content-Type` - must always be `application/json`.  The format of the request body is described in detail on the [MapReduce]({{<baseurl>}}riak/kv/2.1.1/developing/usage/mapreduce) page.

Optional query parameters:
* `chunked` - when set to `true`, results will be returned as they are received in `multipart/mixed` format using chunked-encoding.

_+This request must include an entity (body), which is the JSON form of the MapReduce query.+_

## Response

Normal status codes:
* `200 OK`

Typical error codes:
* `400 Bad Request` - if an invalid job is submitted.
* `500 Internal Server Error` - if there was an error in processing a map or reduce function
* `503 Service Unavailable` - if the job timed out before it could complete

Important headers:
* `Content-Type` - `application/json` when `chunked` is not true, otherwise `multipart/mixed` with `application/json` sections.

## Example

```curl
$ curl -v -d '{"inputs":"test", "query":[{"link":{"bucket":"test"}},{"map":{"language":"javascript","name":"Riak.mapValuesJson"}}]}' -H "Content-Type: application/json" http://127.0.0.1:8098/mapred
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> POST /mapred HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
> Content-Type: application/json
> Content-Length: 117
>
< HTTP/1.1 200 OK
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/json
< Content-Length: 30
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
[{"foo":"bar"},{"riak":"CAP"}]
```
