---
title: "HTTP Fetch Search Schema"
description: ""
project: "riak_kv"
project_version: "2.0.4"
lastmod: 2015-01-10T00:00:00-00:00
sitemap:
  priority: 0.1
menu:
  riak_kv-2.0.4:
    name: "Fetch Search Schema"
    identifier: "http_fetch_search_schema"
    weight: 116
    parent: "apis_http"
toc: true
version_history:
  in: "2.0.0-2.9999.9999"
aliases:
  - /riak/2.0.4/dev/references/http/fetch-search-schema
  - /riak/kv/2.0.4/dev/references/http/fetch-search-schema
---

Retrieves a Riak KV [search schema]({{<baseurl>}}riak/kv/2.0.4/developing/usage/search-schemas).

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
