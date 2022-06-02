---
title: "HTTP Search Index Info"
description: ""
project: "riak_kv"
project_version: "2.1.1"
menu:
  riak_kv-2.1.1:
    name: "Search Index Info"
    identifier: "http_search_index_info"
    weight: 114
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.1.1/dev/references/http/search-index-info
  - /riak/kv/2.1.1/dev/references/http/search-index-info
---

Retrieves information about all currently available [Search indexes]({{<baseurl>}}riak/kv/2.1.1/developing/usage/search) in JSON format.

## Request

```
GET /search/index
```

## Response

If there are no currently available Search indexes, a `200 OK` will be
returned but with an empty list as the response value.

Below is the example output if there is one Search index, called
`test_index`, currently available:

```json
[
  {
    "n_val": 3,
    "name": "test_index",
    "schema": "_yz_default"
  }
]
```

#### Normal Response Codes

* `200 OK`

#### Typical Error Codes

* `404 Object Not Found` --- Typically returned if Riak Search is not
    currently enabled on the node
* `503 Service Unavailable` --- The request timed out internally
