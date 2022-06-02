---
title: "HTTP Store Search Index"
description: ""
project: "riak_kv"
project_version: 2.9.2
menu:
  riak_kv-2.9.2:
    name: "Store Search Index"
    identifier: "http_store_search_index"
    weight: 115
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.9.2/dev/references/http/store-search-index
  - /riak/kv/2.9.2/dev/references/http/store-search-index
---

Creates a new Riak Search [index]({{<baseurl>}}riak/kv/2.9.2/developing/usage/search/#simple-setup).

## Request

```
PUT /search/index/<index_name>
```

## Optional Request Body

If you run a `PUT` request to this endpoint without a request body, Riak
will create a new Search index that uses the [default Search schema]({{<baseurl>}}riak/kv/2.9.2/developing/usage/search-schemas/#the-default-schema), i.e. `_yz_default`.

To specify a different schema, however, you must pass Riak a JSON object
as the request body in which the `schema` field specifies the name of
the schema to use. If you've [stored a schema]({{<baseurl>}}riak/kv/2.9.2/developing/usage/search-schemas/#custom-schemas) called `my_custom_schema`, the following `PUT`
request would create an index called `my_index` that used that schema:

```curl
curl -XPUT http://localhost:8098/search/index/my_index \
  -H "Content-Type: application/json" \
  -d '{"schema": "my_custom_schema"}'
```

More information can be found in [Using Search]({{<baseurl>}}riak/kv/2.9.2/developing/usage/search).

## Normal Response Codes

* `204 No Content` --- The index has been successfully created

## Typical Error Codes

* `409 Conflict` --- The index cannot be created because there is
    already an index with that name
* `503 Service Unavailable` --- The request timed out internally
