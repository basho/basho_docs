---
title: "HTTP Counters"
description: ""
project: "riak_kv"
project_version: 2.9.9
menu:
  riak_kv-2.9.9:
    name: "Counters"
    identifier: "http_counters"
    weight: 118
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.9.9/dev/references/http/counters
  - /riak/kv/2.9.9/dev/references/http/counters
---

Riak counters are a CRDT (convergent replicated data type) that (eventually)
converge to the correct total. You merely increment the counter with some
integer, and any potential conflicts will be automatically resolved by Riak.

## Setup

Riak counters can only be used if the bucket has the `allow_mult` property
set to `true`.

```
curl -XPUT localhost:8098/buckets/BUCKET/props \
  -H "Content-Type: application/json" \
  -d "{\"props\" : {\"allow_mult\": true}}"
```

If you attempt to use counters without setting the above, you'll get this
message:

```
Counters require bucket property 'allow_mult=true'
```

## Request

To insert just POST an integer value using the `/counters` resource. This will
increment that keyed value by the given amount.

```
POST /buckets/BUCKET/counters/KEY
```

To receive the current value is a GET using `/counters`

```
GET /buckets/BUCKET/counters/KEY
```

## Response

The regular POST/PUT ([HTTP Store Object]({{<baseurl>}}riak/kv/2.9.9/developing/api/http/store-object)) and GET ([HTTP Fetch Object]({{<baseurl>}}riak/kv/2.9.9/developing/api/http/fetch-object)) responses apply here.

Caveats: Counters have no support for Secondary Indexes (2i), Links or Custom HTTP Metadata.

## Example

The body must be an integer (positive or negative).

```
curl -XPOST http://localhost:8098/buckets/my_bucket/counters/my_key -d "1"

curl http://localhost:8098/buckets/my_bucket/counters/my_key
1

curl -XPOST http://localhost:8098/buckets/my_bucket/counters/my_key -d "100"

curl http://localhost:8098/buckets/my_bucket/counters/my_key
101

curl -XPOST http://localhost:8098/buckets/my_bucket/counters/my_key -d "-1"
100
```




