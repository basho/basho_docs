---
title: "HTTP API"
description: ""
project: "riak_kv"
project_version: 3.0.13
menu:
  riak_kv-3.0.13:
    name: "HTTP API"
    identifier: "apis_http"
    weight: 102
    parent: "developing_apis"
toc: true
aliases:
  - /riak/3.0.13/dev/references/http
  - /riak/kv/3.0.13/dev/references/http
---

Riak has a rich, full-featured HTTP 1.1 API. This is an overview of the
operations you can perform via HTTP and can be used as a guide for
developing a compliant client. All URLs assume the default configuration
values where applicable. All examples use `curl` to interact with Riak.

> **URL Escaping**
>
> Buckets, keys, and link specifications may not contain unescaped
slashes. Use a URL-escaping library or replace slashes with `%2F`.

## Bucket-related Operations

Method | URL | Doc
:------|:----|:---
`GET` | `/types/<type>/buckets/<bucket>/props` | [HTTP Get Bucket Properties]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/get-bucket-props)
`PUT` | `/types/<type>/buckets/<bucket>/props` | [HTTP Set Bucket Properties]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/set-bucket-props)
`DELETE` | `/types/<type>/buckets/<bucket>/props` | [HTTP Reset Bucket Properties]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/reset-bucket-props)
`GET` | `/types/<type>/buckets?buckets=true` | [HTTP List Buckets]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/list-buckets)
`GET` | `/types/<type>/buckets/<bucket>/keys?keys=true` | [HTTP List Keys]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/list-keys)

## Object-related Operations

Method | URL | Doc
:------|:----|:---
`GET` | `/types/<type>/buckets/<bucket>/keys/<key>` | [HTTP Fetch Object]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/fetch-object)
`POST` | `/types/<type>/buckets/<bucket>/keys` | [HTTP Store Object]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/store-object)
`PUT` | `/types/<type>/buckets/<bucket>/keys/<key>` | [HTTP Store Object]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/store-object)
`POST` | `/types/<type>/buckets/<bucket>/keys/<key>` | [HTTP Store Object]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/store-object)
`DELETE` | `/types/<type>/buckets/<bucket>/keys/<key>` | [HTTP Delete Object]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/delete-object)

## Riak-Data-Type-related Operations

Method | URL
:------|:----
`GET` | `/types/<type>/buckets/<bucket>/datatypes/<key>`
`POST` | `/types/<type>/buckets/<bucket>/datatypes`
`POST` | `/types/<type>/buckets/<bucket>/datatypes/<key>`

For documentation on the HTTP API for [Riak Data Types]({{<baseurl>}}riak/kv/3.0.13/learn/concepts/crdts),
see the `curl` examples in [Using Data Types]({{<baseurl>}}riak/kv/3.0.13/developing/data-types/#usage-examples)
and subpages e.g. [sets]({{<baseurl>}}riak/kv/3.0.13/developing/data-types/sets).

Advanced users may consult the technical documentation inside the Riak
KV internal module `riak_kv_wm_crdt`.

## Query-related Operations

Method | URL | Doc
:------|:----|:---
`POST` | `/mapred` | [HTTP MapReduce]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/mapreduce)
`GET` | `/types/<type>/buckets/<bucket>/index/<index>/<value>` | [HTTP Secondary Indexes]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/secondary-indexes)
`GET` | `/types/<type>/buckets/<bucket>/index/<index>/<start>/<end>` | [HTTP Secondary Indexes]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/secondary-indexes)

## Server-related Operations

Method | URL | Doc
:------|:----|:---
`GET` | `/ping` | [HTTP Ping]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/ping)
`GET` | `/stats` | [HTTP Status]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/status)
`GET` | `/` | [HTTP List Resources]({{<baseurl>}}riak/kv/3.0.13/developing/api/http/list-resources)

