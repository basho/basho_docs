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

Riak has a rich, full-featured HTTP 1.1 API. This is an overview of the
operations you can perform via HTTP and can be used as a guide for
developing a compliant client. All URLs assume the default configuration
values where applicable. All examples use `curl` to interact with Riak.

<div class="note">
<div class="title">URL Escaping</div>
Buckets, keys, and link specifications may not contain unescaped
slashes. Use a URL-escaping library or replace slashes with `%2F`.
</div>

## API Method Quick Reference

### Bucket-related Operations

Method | URL | Doc
:------|:----|:---
`GET` | `/types/<type>/buckets/<bucket>/props` | [[HTTP Get Bucket Properties]]
`PUT` | `/types/<type>/buckets/<bucket>/props` | [[HTTP Set Bucket Properties]]
`DELETE` | `/types/<type>/buckets/<bucket>/props` | [[HTTP Reset Bucket Properties]]
`GET` | `/types/<type>/buckets?buckets=true` | [[HTTP List Buckets]]
`GET` | `/types/<type>/buckets/<bucket>/keys?keys=true` | [[HTTP List Keys]]

### Object-related Operations

Method | URL | Doc
:------|:----|:---
`GET` | `/types/<type>/buckets/<bucket>/keys/<key>` | [[HTTP Fetch Object]]
`POST` | `/types/<type>/buckets/<bucket>/keys/<key>` | [[HTTP Store Object]]
`PUT` | `/types/<type>/buckets/<bucket>/keys/<key>` | [[HTTP Store Object]]
`DELETE` | `/types/<type>/buckets/<bucket>/keys/<key>` | [[HTTP Delete Object]]

### Riak-Data-Type-related Operations

For documentation on the HTTP API for [[Riak Data Types|Data Types]],
see the `curl` examples in [[Using Data Types]].

### Query-related Operations

Method | URL | Doc
:------|:----|:---
`POST` | `/mapred` | [[HTTP MapReduce]]
`GET` | `/types/<type>/buckets/<bucket>/index/<index>/<value>` | [[HTTP Secondary Indexes]]
`GET` | `/types/<type>/buckets/<bucket>/index/<index>/<start>/<end>` | [[HTTP Secondary Indexes]]

### Server-related Operations

Method | URL | Doc
:------|:----|:---
`GET` | `/ping` | [[HTTP Ping]]
`GET` | `/stats` | [[HTTP Status]]
`GET` | `/` | [[HTTP List Resources]]

## Bucket Operations

Buckets in Riak are a virtual concept. They exist primarily as a
namespace mechanism and as a mechanism to isolate specific behavior
changes that deviate from the default bucket settings. For example, you
may augment the [[number of
replicas|Replication#Selecting-an-N-value-(n_val)]], the specific
storage backend and [[commit hooks|Using Commit Hooks]] at the bucket
level.

<div class="note">
<div class="title">How Many Buckets Can I Have?</div>
Currently, buckets come with virtually no cost except for when you
modify the default bucket properties. Modified Bucket properties are
gossiped around the cluster and therefore add to the amount of data sent
around the network. In other words, buckets using the default bucket
properties are free.
</div>
