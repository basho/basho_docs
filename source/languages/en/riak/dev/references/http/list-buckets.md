---
title: HTTP List Buckets
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Bucket Operations"
moved: {
  '1.4.0-': '/references/apis/http/HTTP-List-Buckets'
}
---

Lists all known buckets, i.e. all buckets that have keys stored within
them.

<div class="note">
<div class="title">Not for production use</div>
Similar to the list keys operation, this requires traversing all keys
stored in the cluster and should not be used in production.
</div>

## Request

```
GET /buckets?buckets=true
```

Query parameters:

* `buckets=true` --- Sends the list of buckets as a JSON list
* `buckets=stream` --- Sends the list of buckets to the clients as a
  stream

## Response

Normal status codes:

* `200 OK`

Important headers:

* `Content-Type: application/json`

The JSON object in the response will contain a single entry,
`buckets`, which will be an array of bucket names.

## Example

```curl
curl -i http://localhost:8098/buckets?buckets=true

HTTP/1.1 200 OK
Vary: Accept-Encoding
Server: <server info>
Date: <current date>
Content-Type: application/json
Content-Length: 21

{"buckets":["files"]}
```
