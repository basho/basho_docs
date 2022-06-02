---
title: "HTTP Set Bucket Properties"
description: ""
project: "riak_kv"
project_version: "2.9.0p5"
menu:
  riak_kv-2.9.0p5:
    name: "Set Bucket Properties"
    identifier: "http_set_bucket_props"
    weight: 101
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.9.0p5/dev/references/http/set-bucket-props
  - /riak/kv/2.9.0p5/dev/references/http/set-bucket-props
  - /riak/2.9.0p5/developing/api/http/set-bucket-props/
  - /riak/2.9.0/developing/api/http/set-bucket-props/
  - /riak/kv/2.9.0/developing/api/http/set-bucket-props/
  - /riak/kv/2.9.0p1/developing/api/http/set-bucket-props/
  - /riak/kv/2.9.0p2/developing/api/http/set-bucket-props/
  - /riak/kv/2.9.0p3/developing/api/http/set-bucket-props/
  - /riak/kv/2.9.0p4/developing/api/http/set-bucket-props/
---


Sets bucket properties like "n_val" and "allow_mult".

## Request

```bash
PUT /buckets/bucket/props
```

Important headers:

* `Content-Type` - `application/json`

The body of the request should be a JSON object with a single entry "props".
Unmodified bucket properties may be omitted.

Available properties:

* `n_val` (integer > 0) - the number of replicas for objects in this bucket
* `allow_mult` (true or false) - whether to allow sibling objects to be created
(concurrent updates)
* `last_write_wins` (true or false) - whether to ignore object history (vector
clock) when writing
* `precommit` - [precommit hooks]({{<baseurl>}}riak/kv/2.9.0p5/developing/usage/commit-hooks)
* `postcommit` - [postcommit hooks]({{<baseurl>}}riak/kv/2.9.0p5/developing/usage/commit-hooks)
* `r, w, dw, rw` - default quorum values for operations on keys in the bucket.
Valid values are:
  * `"all"` - all nodes must respond
  * `"quorum"` - (n_val/2) + 1 nodes must respond. *This is the default.*
  * `"one"` - equivalent to 1
  * *Any integer* - must be less than or equal to n_val
* `backend` - when using `riak_kv_multi_backend`, which named backend to use for
the bucket
* `node_confirms` - declares the number of diverse physical node acks required for a write
to be successful

Other properties do exist but are not commonly modified.

{{% note title="Property types" %}}
Make sure you use the proper types for attributes like **n_val** and
**allow_mult**. If you use strings instead of integers and booleans
respectively, you may see some odd errors in your logs, saying something like
`"{badarith,[{riak_kv_util,normalize_rw_value,2},]}"`.
{{% /note %}}

## Response

Normal status codes:

* `204 No Content`

Typical error codes:

* `400 Bad Request` - if the submitted JSON is invalid
* `415 Unsupported Media Type` - if the Content-Type was not set to
application/json in the request

If successful, no content will be returned in the response body.

## Example

```curl
$ curl -v -XPUT http://127.0.0.1:8098/buckets/test/props /
       -H "Content-Type: application/json" -d '{"props":{"n_val":5}}'
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> PUT /buckets/test/props HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4
OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
> Content-Type: application/json
> Content-Length: 21
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
