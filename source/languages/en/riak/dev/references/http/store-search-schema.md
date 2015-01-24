---
title: HTTP Store Search Schema
project: riak
version: 2.0.0+
document: api
audience: advanced
group_by: "Search-related Operations"
keywords: [http, api, search, index, yokozuna]
---

Creates a new Riak [[Search schema]].

## Request

```
PUT /search/index/<schema_name>
```

## Required Form Data

In order to create a new Search schema, you must pass Riak a properly
formed XML schema. More information can be found in the [[Search
Schema]] document. If you've created a schema and stored it in the filed
`my_schema.xml` and would like to create a new schema called
`my_custom_schema`, you would use the following HTTP request:

```curl
curl -XPUT http://localhost:8098/search/schema/my_custom_schema \
  -H "Content-Type: application/xml" \
  --data-binary @my_schema.xml
```

## Normal Response

* `204 No Content` --- The schema has been successfully created

## Typical Error Codes

* `409 Conflict` --- The schema cannot be created because there is
    already a schema with that name
