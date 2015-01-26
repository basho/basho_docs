---
title: HTTP Fetch Search Schema
project: riak
version: 2.0.0+
document: api
audience: advanced
group_by: "Search-related Operations"
keywords: [http, api, search, schema, yokozuna]
---

Retrieves a Riak [[search schema]].

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
