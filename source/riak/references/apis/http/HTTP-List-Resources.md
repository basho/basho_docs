---
title: HTTP リソースのリスト
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Server Operations"
---

Riakノードのために、利用出来るHTTPリソースをリストする。
これは特定のオペレーションに使うリソースの位置を自動的に認識するために、クライアントが使用することができる。

標準リソース:

* `riak_kv_wm_buckets` - [[バケットのオペレーション|HTTP API#Bucket Operations]]
* `riak_kv_wm_index` - [[HTTPセカンダリインデクス|HTTP Secondary Indexes]]
* `riak_kv_wm_link_walker` - [[HTTPリンクウォーキング|HTTP Link Walking]]
* `riak_kv_wm_mapred` - [[HTTP MapReduce]]
* `riak_kv_wm_object`- [[オブジェクト / キー オペレーション|HTTP API#Object/Key Operations]]
* `riak_kv_wm_ping` - [[HTTP Ping]]
* `riak_kv_wm_props` - [[HTTP バケット プロパティのセット|HTTP Set Bucket Properties]]
* `riak_kv_wm_stats` - [[HTTPステータス|HTTP Status]]

サーチが有効であれば、以下のリソースも含まれます。

* `riak_solr_searcher_wm` - [[Solr サーチh|Riak Search - Querying#Querying via the Solr Interface]]
* `riak_solr_indexer_wm` - [[Solr インデクス|Riak Search - Indexing#Indexing using the Solr Interface]]

{{#1.0.0-}}

Luwak が有効であれば、以下のリソースも含まれます。

* `luwak_wm_file` - [[Luwak オペレーション|HTTP API#Luwak Operations (Large Objects)]]

{{/1.0.0-}}

## リクエスト

```bash
GET /
```

ヘッダ:

* `Accept` - `application/json` または `text/html`

## レスポンス

正常ステータスコード:

* `200 OK`

重要なヘッダ:

* `Link` - 全てのリソースはリンクではなく、レスポンスボディに記録されている

## サンプル

```bash
# Request JSON response
curl -i http://localhost:8098 -H "Accept: application/json"
HTTP/1.1 200 OK
Vary: Accept
Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
Link: </riak>; rel="riak_kv_wm_link_walker",</mapred>; rel="riak_kv_wm_mapred",</ping>; rel="riak_kv_wm_ping",</riak>; rel="riak_kv_wm_raw",</stats>; rel="riak_kv_wm_stats"
Date: Fri, 30 Sep 2011 15:24:35 GMT
Content-Type: application/json
Content-Length: 143

{"riak_kv_wm_link_walker":"/riak","riak_kv_wm_mapred":"/mapred","riak_kv_wm_ping":"/ping","riak_kv_wm_raw":"/riak","riak_kv_wm_stats":"/stats"}

# Request HTML response
curl -i http://localhost:8098 -H "Accept: text/html"
HTTP/1.1 200 OK
Vary: Accept
Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
Link: </riak>; rel="riak_kv_wm_link_walker",</mapred>; rel="riak_kv_wm_mapred",</ping>; rel="riak_kv_wm_ping",</riak>; rel="riak_kv_wm_raw",</stats>; rel="riak_kv_wm_stats"
Date: Fri, 30 Sep 2011 15:24:35 GMT
Content-Type: text/html
Content-Length: 267

<html><body><ul><li><a href="/riak">riak_kv_wm_link_walker</a></li><li><a href="/mapred">riak_kv_wm_mapred</a></li><li><a href="/ping">riak_kv_wm_ping</a></li><li><a href="/riak">riak_kv_wm_raw</a></li><li><a href="/stats">riak_kv_wm_stats</a></li></ul></body></html>
```