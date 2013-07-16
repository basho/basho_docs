---
title: HTTP Delete Object
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Object/Key Operations"
moved: {
  '1.4.0-': '/references/apis/http/HTTP-Delete-Object'
}
---

Deletes an object from the specified bucket / key.

## Request

```
DELETE /riak/bucket/key           # Old format
DELETE /buckets/bucket/keys/key   # New format
```

Optional query parameters:

* `rw` - quorum for both operations (get and put) involved in deleting an
object (default is set at the bucket level)
* `r` - (read quorum) how many replicas need to agree when retrieving the object
* `pr` - (primary read quorum) works like `r` but requires that the nodes
read from are not fallback nodes
* `w` - (write quorum) how many replicas must confirm receiving writes before returning a successful response
* `dw` - (durable write quorum) how many replicas to commit to durable storage
before returning a successful response
* `pw` - (primary write quorum) how many replicas to commit to primary nodes
before returning a successful response

## Response

<div class="note"><div class="title">Client ID</div>
<p>All requests to Riak &lt;1.0 or Riak 1.0 without `vnode_vclocks` enabled
should include the `X-Riak-ClientId` header, which can be any string that
uniquely identifies the client, for purposes of tracing object modifications in
the [[vector clock|Vector Clocks]].</p>
</div>

Normal response codes:

* `204 No Content`
* `404 Not Found`

Typical error codes:

* `400 Bad Request` - e.g. when rw parameter is invalid (> N)

`404` responses are "normal" in the sense that DELETE operations are idempotent
and not finding the resource has the same effect as deleting it.

## Example

```bash
$ curl -v -X DELETE http://127.0.0.1:8098/riak/test/test2
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> DELETE /riak/test/test2 HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
>
< HTTP/1.1 204 No Content
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/json
< Content-Length: 0
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
```
