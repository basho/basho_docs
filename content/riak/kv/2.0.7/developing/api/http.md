---
title: "HTTP API"
description: ""
project: "riak_kv"
project_version: "2.0.7"
menu:
  riak_kv-2.0.7:
    name: "HTTP API"
    identifier: "apis_http"
    weight: 102
    parent: "developing_apis"
toc: true
aliases:
  - /riak/2.0.7/dev/references/http
  - /riak/kv/2.0.7/dev/references/http
canonical_link: "https://docs.basho.com/riak/kv/latest/developing/api/http"
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
`GET` | `/types/<type>/buckets/<bucket>/props` | [HTTP Get Bucket Properties](/riak/kv/2.0.7/developing/api/http/get-bucket-props)
`PUT` | `/types/<type>/buckets/<bucket>/props` | [HTTP Set Bucket Properties](/riak/kv/2.0.7/developing/api/http/set-bucket-props)
`DELETE` | `/types/<type>/buckets/<bucket>/props` | [HTTP Reset Bucket Properties](/riak/kv/2.0.7/developing/api/http/reset-bucket-props)
`GET` | `/types/<type>/buckets?buckets=true` | [HTTP List Buckets](/riak/kv/2.0.7/developing/api/http/list-buckets)
`GET` | `/types/<type>/buckets/<bucket>/keys?keys=true` | [HTTP List Keys](/riak/kv/2.0.7/developing/api/http/list-keys)

## Object-related Operations

Method | URL | Doc
:------|:----|:---
`GET` | `/types/<type>/buckets/<bucket>/keys/<key>` | [HTTP Fetch Object](/riak/kv/2.0.7/developing/api/http/fetch-object)
`POST` | `/types/<type>/buckets/<bucket>/keys/<key>` | [HTTP Store Object](/riak/kv/2.0.7/developing/api/http/store-object)
`PUT` | `/types/<type>/buckets/<bucket>/keys/<key>` | [HTTP Store Object](/riak/kv/2.0.7/developing/api/http/store-object)
`DELETE` | `/types/<type>/buckets/<bucket>/keys/<key>` | [HTTP Delete Object](/riak/kv/2.0.7/developing/api/http/delete-object)

## Riak-Data-Type-related Operations

For documentation on the HTTP API for [Riak Data Types](/riak/kv/2.0.7/learn/concepts/crdts),
see the `curl` examples in [Using Data Types](/riak/kv/2.0.7/developing/data-types).

## Query-related Operations

Method | URL | Doc
:------|:----|:---
`POST` | `/mapred` | [HTTP MapReduce](/riak/kv/2.0.7/developing/api/http/mapreduce)
`GET` | `/types/<type>/buckets/<bucket>/index/<index>/<value>` | [HTTP Secondary Indexes](/riak/kv/2.0.7/developing/api/http/secondary-indexes)
`GET` | `/types/<type>/buckets/<bucket>/index/<index>/<start>/<end>` | [HTTP Secondary Indexes](/riak/kv/2.0.7/developing/api/http/secondary-indexes)

## Server-related Operations

Method | URL | Doc
:------|:----|:---
`GET` | `/ping` | [HTTP Ping](/riak/kv/2.0.7/developing/api/http/ping)
`GET` | `/stats` | [HTTP Status](/riak/kv/2.0.7/developing/api/http/status)
`GET` | `/` | [HTTP List Resources](/riak/kv/2.0.7/developing/api/http/list-resources)

## Search-related Operations

Method | URL | Doc
:------|:----|:---
`GET` | `/search/query/<index_name>` | [HTTP Search Query](/riak/kv/2.0.7/developing/api/http/search-query)
`GET` | `/search/index` | [HTTP Search Index Info](/riak/kv/2.0.7/developing/api/http/search-index-info)
`GET` | `/search/index/<index_name>` | [HTTP Fetch Search Index](/riak/kv/2.0.7/developing/api/http/fetch-search-index)
`PUT` | `/search/index/<index_name>` | [HTTP Store Search Index](/riak/kv/2.0.7/developing/api/http/store-search-index)
`DELETE` | `/search/index/<index_name>` | [HTTP Delete Search Index](/riak/kv/2.0.7/developing/api/http/delete-search-index)
`GET` | `/search/schema/<schema_name>` | [HTTP Fetch Search Schema](/riak/kv/2.0.7/developing/api/http/fetch-search-schema)
`PUT` | `/search/schema/<schema_name>` | [HTTP Store Search Schema](/riak/kv/2.0.7/developing/api/http/store-search-schema)
