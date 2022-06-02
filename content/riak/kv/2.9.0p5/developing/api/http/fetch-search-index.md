---
title: "HTTP Fetch Search Index"
description: ""
project: "riak_kv"
project_version: "2.9.0p5"
menu:
  riak_kv-2.9.0p5:
    name: "Fetch Search Index"
    identifier: "http_fetch_search_index"
    weight: 115
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.9.0p5/dev/references/http/fetch-search-index
  - /riak/kv/2.9.0p5/dev/references/http/fetch-search-index
  - /riak/2.9.0p5/developing/api/http/fetch-search-index/
  - /riak/2.9.0/developing/api/http/fetch-search-index/
  - /riak/kv/2.9.0/developing/api/http/fetch-search-index/
  - /riak/kv/2.9.0p1/developing/api/http/fetch-search-index/
  - /riak/kv/2.9.0p2/developing/api/http/fetch-search-index/
  - /riak/kv/2.9.0p3/developing/api/http/fetch-search-index/
  - /riak/kv/2.9.0p4/developing/api/http/fetch-search-index/
---

Retrieves information about a Riak Search [index]({{<baseurl>}}riak/kv/2.9.0p5/developing/usage/search/#simple-setup).

## Request

```
GET /search/index/<index_name>
```

## Normal Response Codes

* `200 OK`

## Typical Error Codes

* `404 Object Not Found` --- No Search index with that name is currently
    available
* `503 Service Unavailable` --- The request timed out internally

## Response

If the index is found, Riak will output a JSON object describing the
index, including its name, the [`n_val`]({{<baseurl>}}riak/kv/2.9.0p5/developing/app-guide/replication-properties/#a-primer-on-n-r-and-w) associated with it, and the [search schema]({{<baseurl>}}riak/kv/2.9.0p5/developing/usage/search-schemas) used by the index. Here is an example:

```json
{
  "name": "my_index",
  "n_val": 3,
  "schema": "_yz_default"
}
```
