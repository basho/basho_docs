---
title: HTTP Counters
project: riak
version: 1.4.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Datatypes"
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

To recieve the current value is a GET using `/counters`

```
GET /buckets/BUCKET/counters/KEY
```

## Response

The regular POST/PUT ([[HTTP Store Object]]) and GET ([[HTTP Fetch Object]]) responses apply here.

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
