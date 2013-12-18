---
title: HTTP List Resources
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Server Operations"
moved: {
  '1.4.0-': '/references/apis/http/HTTP-List-Resources'
}
---

List available HTTP resources for the Riak node. This can be used by clients to
automatically recognize the location of the resources for specific operations.

The standard resources are:

* `riak_kv_wm_buckets` - [[Bucket Operations|HTTP API#Bucket-Operations]]
* `riak_kv_wm_index` - [[HTTP Secondary Indexes]]
* `riak_kv_wm_link_walker` - [[HTTP Link Walking]]
* `riak_kv_wm_mapred` - [[HTTP MapReduce]]
* `riak_kv_wm_object`- [[Object/Key Operations|HTTP API#Object-Key-Operations]]
* `riak_kv_wm_ping` - [[HTTP Ping]]
* `riak_kv_wm_props` - [[HTTP Set Bucket Properties]]
* `riak_kv_wm_stats` - [[HTTP Status]]

If Search is enabled, the following resources will also be included:

* `riak_solr_searcher_wm` - [[Solr search|Using Search#Querying]]
* `riak_solr_indexer_wm` - [[Solr indexing|Advanced Search#Indexing-using-the-Solr-Interface]]

{{#1.0.0-}}

If Luwak is enabled, the following resource will also be included:

* `luwak_wm_file` - [[Luwak Operations|HTTP API#Luwak Operations (Large Objects)]]

{{/1.0.0-}}

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

```bash
# Request JSON response
curl -i http://localhost:8098 -H "Accept: application/json"
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

<html><body><ul><li><a href="/buckets">riak_kv_wm_buckets</a></li><li><a href="/riak">riak_kv_wm_buckets</a></li><li><a href="/buckets">riak_kv_wm_counter</a></li><li><a href="/buckets">riak_kv_wm_index</a></li><li><a href="/buckets">riak_kv_wm_keylist</a></li><li><a href="/buckets">riak_kv_wm_link_walker</a></li><li><a href="/riak">riak_kv_wm_link_walker</a></li><li><a href="/mapred">riak_kv_wm_mapred</a></li><li><a href="/buckets">riak_kv_wm_object</a></li><li><a href="/riak">riak_kv_wm_object</a></li><li><a href="/ping">riak_kv_wm_ping</a></li><li><a href="/buckets">riak_kv_wm_props</a></li><li><a href="/stats">riak_kv_wm_stats</a></li></ul></body></html>
```
