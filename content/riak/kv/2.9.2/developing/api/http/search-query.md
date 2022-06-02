---
title: "HTTP Search Query"
description: ""
project: "riak_kv"
project_version: 2.9.2
menu:
  riak_kv-2.9.2:
    name: "Search Query"
    identifier: "http_search_query"
    weight: 113
    parent: "apis_http"
toc: true
aliases:
  - /riak/2.9.2/dev/references/http/search-query
  - /riak/kv/2.9.2/dev/references/http/search-query
---

Performs a [Riak KV Search]({{<baseurl>}}riak/kv/2.9.2/developing/usage/search) query.

## Request

```
GET /search/query/<index_name>
```

## Optional Query Parameters

* `wt` --- The [response
    writer](https://cwiki.apache.org/confluence/display/solr/Response+Writers)
    to be used when returning the Search payload. The currently
    available options are `json` and `xml`. The default is `xml`.
* `q` --- The actual Search query itself. Examples can be found in
    [Using Search]({{<baseurl>}}riak/kv/2.9.2/developing/usage/search). If a query is not specified, Riak will return
    information about the index itself, e.g. the number of documents
    indexed.

## Normal Response Codes

* `200 OK`

## Typical Error Codes

* `400 Bad Request` --- Returned when, for example, a malformed query is
    supplied
* `404 Object Not Found` --- Returned if the Search index you are
    attempting to query does not exist
* `503 Service Unavailable` --- The request timed out internally

## Response

If a `200 OK` is returned, then the Search query has been successful.
Below is an example JSON response from querying an index that currently
has no documents associated with it:

```json
{
  "response": {
    "docs": [],
    "maxScore": 0.0,
    "numFound": 0,
    "start": 0
  },
  "responseHeader": {
    "status": 0,
    "QTime": 10,
    "params": { /* internal info from the query */ }
  }
}
```
