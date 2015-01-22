---
title: HTTP Search Query
project: riak
version: 2.0.0+
document: api
audience: advanced
keywords: [api, http, search, yokozuna]
---

Performs a [[Riak Search|Using Search]] query.

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
    [[Using Search]]. If a query is not specified, Riak will return
    information about the index itself, e.g. the number of documents
    indexed.

## Normal Response Codes

* `200 OK`

Typical error codes:

* `400 Bad Request` --- Returned when, for example, a malformed query is
    supplied
* `404 Object Not Found` --- Returned if the Search index you are
    attempting to query does not exist
* `503 Service Unavailable` --- The request timed out internally

## Response

If a `200 OK` is returned, then the Search query has been successful,
and a payload like the following should be returned:

```json

```
