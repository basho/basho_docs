---
title: "HTTP Client API"
description: "HTTP Client API"
menu:
  riak_ts-1.3.0:
    name: "HTTP"
    identifier: "ts_http_api"
    weight: 401
    parent: "develop"
project: "riak_ts"
project_version: "1.3.0"
toc: true
aliases:
    - /riakts/1.3.0/developing/http/
canonical_link: "docs.basho.com/riak/ts/latest/developing/http"
---


This document will cover the API calls for accessing Riak TS data over HTTP.


## Overview

All Riak TS calls use the '/ts' endpoint. Each API call has a corresponding URL:

| Call   | Request URL         | Request type | Description  |
|------------|---------------------|--------------|--------------|
| get        | http://server/ts/tables/»Table«/keys/»Family«/»a-family«/»Series«/»a-series«/»Time«/»a-time«  | GET          | single-key get of a value | 
| put        | http://server/ts/tables/»Table«/keys  | PUT          | put a single or a batch of records    |
| delete     | http://server/ts/tables/»Table«/»Family«/»a-family/»Series«/»a-series«/»Time«/»a-time«  | DELETE       | single-key delete         |
| list_keys  | http://server/ts/tables/»Table«/list_keys  | GET          | streaming list keys     |
| query      | http://server/ts/query  | POST         | execute a query |


## Keys and values

Single-key `get` uses the full Family-Series-Time path to determine which record to get. The entire record will be returned.

`delete` also uses the full path to designate which record to delete.

The `put` call uses a full record or a list of full records in order to add entries to the table.

Streaming `list_keys` returns all the URLs as plain text separated by a new line for the all the keys in the table.

The `query` to be executed should be sent as a plain text string in the body of the request.


## Returning results

All request results will be JSON-encoded, except for the streaming list of keys, which returns plain text.

Error conditions will be returned by a JSON structure containing an internal error code and a human-readable message, in addition to reporting HTTP status code in the response in the 400 range.