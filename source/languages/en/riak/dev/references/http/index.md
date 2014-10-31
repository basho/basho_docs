---
title: HTTP API
project: riak
version: 1.0.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
index: true
moved: {
  '1.4.0-': '/references/apis/http'
}
---

Riak has a rich, full-featured HTTP 1.1 API.  This is an overview of the
operations you can perform via HTTP and can be used as a guide for developing a
compliant client.  All URLs assume the default configuration values where
applicable. All examples use `curl` to interact with Riak.

<div class="note"><div class="title">Client ID</div>
<p>All requests to Riak &lt;1.0 or Riak 1.0 without `vnode_vclocks` enabled
should include the `X-Riak-ClientId` header, which can be any string that
uniquely identifies the client, for purposes of tracing object modifications in
the [[vector clock|Vector Clocks]].</p>
</div>

<div class="note"><div class="title">URL Escaping</div>
<p>Buckets, keys, and link specifications may not contain unescaped slashes. Use
a URL-escaping library or replace slashes with `%2F`.</p>
</div>

## API Method Quick Reference

* Bucket related methods
..* GET /types/{{ type_name }}/buckets/{{ bucket_name }}/props
... Fetches bucket properties. [[HTTP Get Bucket Properties]]
..* PUT /types/{{ type_name }}/buckets/{{ bucket_name }}/props
... Sets bucket properties. [[HTTP Set Bucket Properties]]
..* DELETE /types/{{ type_name }}/buckets/{{ bucket_name }}/props
... Resets the buckets back to default properties.
..* GET /types/{{ type_name }}/buckets?buckets=true
... Lists all of the buckets within the node [[HTTP List Buckets]]
..* GET /types/{{ type_name }}/buckets/{{ bucket_name }}/keys?keys
... Lists all of the keys within the bucket [[HTTP List Keys]]

* Object related methods
..* GET /types/{{ type_name }}/buckets/{{ bucket_name }}/keys/{{ key_name }}
... Fetch an object by bucket and key. [[HTTP Fetch Object]]
..* POST /types/{{ type_name }}/buckets/{{ bucket_name }}/keys
... Store a new object within Riak, letting Riak generate a key. [[HTTP Store Object]]
..* PUT /types/{{ type_name }}/buckets/{{ bucket_name }}/keys/{{ key_name }}
... Store a new object within Riak, with user providing a key. [[HTTP Store Object]]
..* DELETE /types/{{ type_name }}/buckets/{{ bucket_name }}/keys/{{ key_name }}
... Delete an object at the specified location [[HTTP Delete Object]]

* Datatype related methods
..* GET /types/{{ type_name }}/buckets/{{ bucket_name }}/counters/{{ key_name }}
... Fetch a counter by bucket and key. [[HTTP Counters]]
..* POST /types/{{ type_name }}/buckets/{{ bucket_name }}/counters/{{ key_name }}
... Insert to a counter by bucket and key. [[HTTP Counters]]

* Query related methods
..* GET /types/{{ type_name }}/buckets/{{ bucket_name }}/keys/{{ key_name }}/{{ bucket_name|tag|keep }}
... Traverse links of an object. [[HTTP Link Walking]]
..* POST /mapred
... Perform a map reduce query. [[HTTP MapReduce]]
..* GET /types/{{ type_name }}/buckets/{{ bucket_name }}/index/{{ index_bin }}/{{ value }}
... Use secondary indexes to find objects with exact match. [[HTTP Secondary Indexes]]
..* GET /types/{{ type_name }}/buckets/{{ bucket_name }}/index/{{ index_bin }}/{{ start }}/{{ end }}
... Use secondary indexes to find objects matching the range. [[HTTP Secondary Indexes]]

* Server related methods
..* GET /ping
... Checks to verify the server is alive. [[HTTP Ping]]
..* GET /stats
... Get the servers status. [[HTTP Status]]
..* GET /
... List the servers resources. [[HTTP List Resources]]

## Bucket Operations

Buckets in Riak are a virtual concept. They exist primarily as a namespace
mechanism and as a mechanism to isolate specific behavior changes that deviate
from the default bucket settings. For example, you may augment the [[number of
replicas|Replication#Selecting-an-N-value-(n_val)]], the specific storage
backend and [[commit hooks|Using Commit Hooks]] at the bucket level.

<div class="info"><div class="title">How Many Buckets Can I Have?</div>
<p>Currently, buckets come with virtually no cost except for when you modify the
default bucket properties. Modified Bucket properties are gossiped around the
cluster and therefore add to the amount of data sent around the network. In
other words, buckets using the default bucket properties are free.</p>
</div>

<div class="note"><div class="title">Delete Buckets</div>
<p>There is no straightforward way to delete an entire Bucket. To delete all
the keys in a bucket, you'll need to delete them all individually.</P>
</div>

* [[HTTP List Buckets]]
* [[HTTP List Keys]]
* [[HTTP Get Bucket Properties]]
* [[HTTP Set Bucket Properties]]
* [[HTTP Reset Bucket Properties]] {{1.3.0+}}

## Object/Key Operations

The combination of bucket, key, value and metadata are referred to as a "Riak
Object". The operations below affect individual objects in Riak.

* [[HTTP Fetch Object]]
* [[HTTP Store Object]]
* [[HTTP Delete Object]]

{{#1.4.0+}}
## Datatypes

* [[HTTP Counters]]

{{/1.4.0+}}

## Query Operations

* [[HTTP Link Walking]]
* [[HTTP MapReduce]]
* [[HTTP Secondary Indexes]]

## Server Operations

* [[HTTP Ping]]
* [[HTTP Status]]
* [[HTTP List Resources]]