---
title: HTTP Delete Search Index
project: riak
version: 2.0.0+
document: api
audience: advanced
group_by: "Search-related Operations"
keywords: [http, api, search, index, yokozuna]
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
