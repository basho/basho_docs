---
title: "HTTP Store Search Index"
description: ""
project: "riak_kv"
project_version: "2.1.1"
menu:
  riak_kv-2.1.1:
    name: "Store Search Index"
    identifier: "http_store_search_index"
    weight: 115
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.1.3/dev/references/http/store-search-index
canonical_link: "docs.basho.com/riak/kv/latest/developing/api/http/store-search-index.md"
---

Creates a new Riak Search [index](/riak/kv/2.1.1/developing/usage/search/#Simple-Setup).

## Request

```
PUT /search/index/<index_name>
```

## Optional Request Body

If you run a `PUT` request to this endpoint without a request body, Riak
will create a new Search index that uses the [default Search schema](/riak/kv/2.1.1/developing/usage/search-schemas/#The-Default-Schema), i.e. `_yz_default`.

To specify a different schema, however, you must pass Riak a JSON object
as the request body in which the `schema` field specifies the name of
the schema to use. If you've [stored a schema](/riak/kv/2.1.1/developing/usage/search-schemas/#Custom-Schemas) called `my_custom_schema`, the following `PUT`
request would create an index called `my_index` that used that schema:

```curl
curl -XPUT http://localhost:8098/search/index/my_index \
  -H "Content-Type: application/json" \
  -d '{"schema": "my_custom_schema"}'
```

More information can be found in [Using Search](/riak/kv/2.1.1/developing/usage/search).

## Normal Response Codes

* `204 No Content` --- The index has been successfully created

## Typical Error Codes

* `409 Conflict` --- The index cannot be created because there is
    already an index with that name
* `503 Service Unavailable` --- The request timed out internally
