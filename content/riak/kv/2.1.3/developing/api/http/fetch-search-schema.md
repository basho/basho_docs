---
title: "HTTP Fetch Search Schema"
description: ""
project: "riak_kv"
project_version: "2.1.3"
menu:
  riak_kv-2.1.3:
    name: "Fetch Search Schema"
    identifier: "http_fetch_search_schema"
    weight: 116
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.1.3/dev/references/http/fetch-search-schema
  - /riak/kv/2.1.3/dev/references/http/fetch-search-schema
---

Retrieves a Riak KV [search schema]({{<baseurl>}}riak/kv/2.1.3/developing/usage/search-schemas).

## Request

```
GET /search/schema/<schema_name>
```

## Normal Response Codes

* `200 OK`

## Typical Error Codes

* `404 Object Not Found`
* `503 Service Unavailable` --- The request timed out internally

## Response

If the schema is found, Riak will return the contents of the schema as
XML (all Riak Search schemas are XML).
