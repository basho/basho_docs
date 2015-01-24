---
title: HTTP Store Search Index
project: riak
version: 2.0.0+
document: api
audience: advanced
keywords: [http, api, search, index, yokozuna]
group_by: "Search-related Operations"
---

Creates a new Riak Search [[index|Using Search#Simple-Setup]].

## Request

```
PUT /search/index/<index_name>
```

## Optional Request Body

If you run a `PUT` request to this endpoint without a request body, Riak
will create a new Search index that uses the [[default Search
schema|Search Schema#The-Default-Schema]], i.e. `_yz_default`.

To specify a different schema, however, you must pass Riak a JSON object
as the request body in which the `schema` field specifies the name of
the schema to use. If you've [[stored a schema|Search
Schema#Custom-Schemas]] called `my_custom_schema`, the following `PUT`
request would create an index called `my_index` that used that schema:

```curl
curl -XPUT http://localhost:8098/search/index/my_index \
  -H "Content-Type: application/json" \
  -d '{"schema": "my_custom_schema"}'
```

More information can be found in [[Using Search]].

## Normal Response Codes

* `204 No Content` --- The index has been successfully created

## Typical Error Codes

* `409 Conflict` --- The index cannot be created because there is
    already an index with that name
