---
title: "HTTP List Buckets"
description: ""
project: "riak_kv"
project_version: 2.9.4
menu:
  riak_kv-2.9.4:
    name: "List Buckets"
    identifier: "http_list_buckets"
    weight: 103
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.9.4/dev/references/http/list-buckets
  - /riak/kv/2.9.4/dev/references/http/list-buckets
---

Lists all known buckets (ones that have keys stored in them).

{{% note title="Not for production use" %}}
Similar to the list keys operation, this requires traversing all keys stored
in the cluster and should not be used in production.
{{% /note %}}

## Request

```bash
# Using the default bucket type
GET /buckets?buckets=true

# Using a non-default bucket type
GET /types/<type>/buckets?buckets=true
```

Required query parameter:

* **buckets=true** - required to invoke the list-buckets functionality

## Response

Normal status codes:

* `200 OK`

Important headers:

* `Content-Type - application/json`

The JSON object in the response will contain a single entry, "buckets", which
will be an array of bucket names.

## Example

```curl
$ curl -i http://localhost:8098/buckets?buckets=true
HTTP/1.1 200 OK
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
Date: Fri, 30 Sep 2011 15:24:35 GMT
Content-Type: application/json
Content-Length: 21

{"buckets":["files"]}
```

