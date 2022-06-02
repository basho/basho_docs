---
title: "HTTP Delete Search Index"
description: ""
project: "riak_kv"
project_version: 2.9.2
menu:
  riak_kv-2.9.2:
    name: "Delete Search Index"
    identifier: "http_delete_search_index"
    weight: 116
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.9.2/dev/references/http/delete-search-index
  - /riak/kv/2.9.2/dev/references/http/delete-search-index
---

Deletes a Riak Search index.

## Request

```
DELETE /search/index/<index_name>
```

## Normal Response Codes

* `204 No Content` --- The index was successfully deleted (also returned
    if the index did not exist to begin with)

## Typical Error Codes

* `503 Service Unavailable` --- The request timed out internally
