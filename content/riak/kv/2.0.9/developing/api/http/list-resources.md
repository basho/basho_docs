---
title: "HTTP List Resources"
description: ""
project: "riak_kv"
project_version: "2.0.9"
menu:
  riak_kv-2.0.9:
    name: "List Resources"
    identifier: "http_list_resources"
    weight: 112
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.0.9/dev/references/http/list-resources
  - /riak/kv/2.0.9/dev/references/http/list-resources
---

List available HTTP resources for the Riak node. This can be used by clients to
automatically recognize the location of the resources for specific operations.

The standard resources are:

* `riak_kv_wm_buckets` - [Bucket Operations]({{<baseurl>}}riak/kv/2.0.9/developing/api/http/#bucket-operations)
* `riak_kv_wm_index` - [HTTP Secondary Indexes]({{<baseurl>}}riak/kv/2.0.9/developing/api/http/secondary-indexes)
* `riak_kv_wm_link_walker` - [HTTP Link Walking]({{<baseurl>}}riak/kv/2.0.9/developing/api/http/link-walking)
* `riak_kv_wm_mapred` - [HTTP MapReduce]({{<baseurl>}}riak/kv/2.0.9/developing/api/http/mapreduce)
* `riak_kv_wm_object`- [Object/Key Operations]({{<baseurl>}}riak/kv/2.0.9/developing/api/http/#object-key-operations)
* `riak_kv_wm_ping` - [HTTP Ping]({{<baseurl>}}riak/kv/2.0.9/developing/api/http/ping)
* `riak_kv_wm_props` - [HTTP Set Bucket Properties]({{<baseurl>}}riak/kv/2.0.9/developing/api/http/set-bucket-props)
* `riak_kv_wm_stats` - [HTTP Status]({{<baseurl>}}riak/kv/2.0.9/developing/api/http/status)

## Request

```bash
GET /
```

Headers:

* `Accept` - `application/json` or `text/html`

## Response

Normal status codes:

* `200 OK`

Important headers:

* `Link` - all resources that are described in the response body, but in Link
form

## Example

Request JSON response

```curl
$ curl -i http://localhost:8098 -H "Accept: application/json"
HTTP/1.1 200 OK
Vary: Accept
Server: MochiWeb/1.1 WebMachine/1.10.0 (never breaks eye contact)
Link: </buckets>; rel="riak_kv_wm_buckets",</riak>; rel="riak_kv_wm_buckets",</buckets>; rel="riak_kv_wm_counter",</buckets>; rel="riak_kv_wm_index",</buckets>; rel="riak_kv_wm_keylist",</buckets>; rel="riak_kv_wm_link_walker",</riak>; rel="riak_kv_wm_link_walker",</mapred>; rel="riak_kv_wm_mapred",</buckets>; rel="riak_kv_wm_object",</riak>; rel="riak_kv_wm_object",</ping>; rel="riak_kv_wm_ping",</buckets>; rel="riak_kv_wm_props",</stats>; rel="riak_kv_wm_stats"
Date: Wed, 27 Nov 2013 20:18:31 GMT
Content-Type: application/json
Content-Length: 398

{"riak_kv_wm_buckets":"/buckets","riak_kv_wm_buckets":"/riak","riak_kv_wm_counter":"/buckets","riak_kv_wm_index":"/buckets","riak_kv_wm_keylist":"/buckets","riak_kv_wm_link_walker":"/buckets","riak_kv_wm_link_walker":"/riak","riak_kv_wm_mapred":"/mapred","riak_kv_wm_object":"/buckets","riak_kv_wm_object":"/riak","riak_kv_wm_ping":"/ping","riak_kv_wm_props":"/buckets","riak_kv_wm_stats":"/stats"}

# Request HTML response
curl -i http://localhost:8098 -H "Accept: text/html"
HTTP/1.1 200 OK
Vary: Accept
Server: MochiWeb/1.1 WebMachine/1.10.0 (never breaks eye contact)
Link: </buckets>; rel="riak_kv_wm_buckets",</riak>; rel="riak_kv_wm_buckets",</buckets>; rel="riak_kv_wm_counter",</buckets>; rel="riak_kv_wm_index",</buckets>; rel="riak_kv_wm_keylist",</buckets>; rel="riak_kv_wm_link_walker",</riak>; rel="riak_kv_wm_link_walker",</mapred>; rel="riak_kv_wm_mapred",</buckets>; rel="riak_kv_wm_object",</riak>; rel="riak_kv_wm_object",</ping>; rel="riak_kv_wm_ping",</buckets>; rel="riak_kv_wm_props",</stats>; rel="riak_kv_wm_stats"
Date: Wed, 27 Nov 2013 20:20:05 GMT
Content-Type: text/html
Content-Length: 666

<html><body><ul><li><a href="buckets">riak_kv_wm_buckets</a></li><li><a href="/riak">riak_kv_wm_buckets</a></li><li><a href="buckets">riak_kv_wm_counter</a></li><li><a href="buckets">riak_kv_wm_index</a></li><li><a href="buckets">riak_kv_wm_keylist</a></li><li><a href="buckets">riak_kv_wm_link_walker</a></li><li><a href="/riak">riak_kv_wm_link_walker</a></li><li><a href="mapred">riak_kv_wm_mapred</a></li><li><a href="buckets">riak_kv_wm_object</a></li><li><a href="/riak">riak_kv_wm_object</a></li><li><a href="ping">riak_kv_wm_ping</a></li><li><a href="buckets">riak_kv_wm_props</a></li><li><a href="stats">riak_kv_wm_stats</a></li></ul></body></html>
```
