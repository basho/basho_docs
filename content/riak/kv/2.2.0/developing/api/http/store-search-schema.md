---
title: "HTTP Store Search Schema"
description: ""
project: "riak_kv"
project_version: "2.2.0"
menu:
  riak_kv-2.2.0:
    name: "Store Search Schema"
    identifier: "http_store_search_schema"
    weight: 117
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.2.0/dev/references/http/store-search-schema
  - /riak/kv/2.2.0/dev/references/http/store-search-schema
---

Creates a new Riak [Search schema]({{<baseurl>}}riak/kv/2.2.0/developing/usage/search-schemas).

## Request

```
PUT /search/schema/<schema_name>
```

## Required Form Data

In order to create a new Search schema, you must pass Riak a properly
formed XML schema. More information can be found in the [Search Schema]({{<baseurl>}}riak/kv/2.2.0/developing/usage/search-schemas) document. If you've created a schema and stored it in the filed
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

* `400 Bad Request` --- The schema cannot be created because there is
    something wrong with the schema itself, e.g. an XML formatting error
    that makes Riak Search unable to parse the schema
* `409 Conflict` --- The schema cannot be created because there is
    already a schema with that name
* `503 Service Unavailable` --- The request timed out internally
