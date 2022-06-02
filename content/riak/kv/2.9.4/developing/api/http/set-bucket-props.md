---
title: "HTTP Set Bucket Properties"
description: ""
project: "riak_kv"
project_version: 2.9.4
menu:
  riak_kv-2.9.4:
    name: "Set Bucket Properties"
    identifier: "http_set_bucket_props"
    weight: 101
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.9.4/dev/references/http/set-bucket-props
  - /riak/kv/2.9.4/dev/references/http/set-bucket-props
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
* `precommit` - [precommit hooks]({{<baseurl>}}riak/kv/2.9.4/developing/usage/commit-hooks)
* `postcommit` - [postcommit hooks]({{<baseurl>}}riak/kv/2.9.4/developing/usage/commit-hooks)
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

{{% note title="Node Confirms" %}}
`node_confirms` is a tunable for durability. When operating in a failure state, Riak will store replicas in fallback vnodes, and in some case multiple fallbacks may be on the same physical node. `node_confirms` is an option that specifies how many distinct physical nodes must acknowledge a write for it to be considered successful.

When riak receives a 'put', it starts up a riak_kv_put_fsm (finite state machine). This prepares and then validates the options, then calls any precommit hooks, before executing a put to the local vnode in the preflist, which becomes the co-ordinating node. This then waits for the local vnode response before executing the put request remotely on the two remaining nodes in the preflist.

The fsm then waits for the remote vnode responses, and as it receives responses, it adds these results and checks whether enough results have been collected to satisfy the bucket properties such as 'dw' and 'pw'.
When analysing the responses, Riak will count the number of different nodes from which results have been returned. The finite state machine can now be required to wait for a minimum number of confirmations from different nodes, whilst also ensuring all other configured options are satisfied.

Once all options are satisfied, the response is returned, post commit hooks are called and the fsm finishes.
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
$ curl -v -XPUT http://127.0.0.1:8098/buckets/test/props \
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

