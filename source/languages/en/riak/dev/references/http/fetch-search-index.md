---
title: HTTP Fetch Search Index
project: riak
version: 2.0.0+
document: api
audience: advanced
group_by: "Search-related Operations"
keywords: [http, api, search, schema, yokozuna]
---

Retrieves information about a Riak Search [[index|Using
Search#Simple-Setup]].

## Request

```
GET /search/index/<index_name>
```

## Response

If the index is found, Riak will output a JSON object describing the
index, including its name, the `[[n_val|Replication
Properties#A-Primer-on-N-R-and-W]]` associated with it, and the [[search
schema]] used by the index. Here is an example:

```json
{
  "name": "my_index",
  "n_val": 3,
  "schema": "_yz_default"
}
```

## Normal Response Codes

* `200 OK`

## Typical Error Codes

* `404 Object Not Found`
