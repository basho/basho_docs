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
| get        | http://»Server«/ts/v1/tables/»Table«/keys/»Family«/»a-family«/»Series«/»a-series«/»Time«/»a-time«  | GET          | single-key get of a value | 
| put        | http://»Server«/ts/v1/tables/»Table«/keys --data '[»Row(s)«]' | POST          | put a single or a batch of rows    |
| delete     | http://»Server«/ts/v1/tables/»Table«/keys/»Family«/»a-family«/»Series«/»a-series«/»Time«/»a-time«  | DELETE       | single-key delete         |
| list_keys  | http://»Server«/ts/v1/tables/»Table«/list_keys  | GET          | streaming list keys     |
| query      | http://»Server«/ts/v1/query --data "»Query«"  | POST         | execute a query |


## Keys and Values

Single-key `get` uses the full Family-Series-Time path to determine which row to get. The entire row will be returned.

`delete` also uses the full path to designate which row to delete.

The `put` call uses a full row or a list of full rows in order to add entries to the table.  Each row is specified in a separate tuple.

Streaming `list_keys` returns all the URLs as plain text separated by a new line for all the keys in the table.

The `query` to be executed should be sent as a plain text string in the body of the request.

The value of `»Server«` is the IP address of your system and the HTTP interface port, separated by a colon.  This value is the `listener.http.internal` setting in the `riak.conf` file.


## Returning Results

All request results will be JSON-encoded, except for the streaming list of keys which returns plain text.

Error conditions will be returned by a JSON structure containing an internal error code and a human-readable message, in addition to reporting HTTP status code in the response in the 400 range.

## Examples

In this section, the example Riak TS calls use the HTTP API and access the following table:

```
CREATE TABLE GeoCheckin
(
   myfamily    varchar   not null,
   myseries    varchar   not null,
   time        timestamp not null,
   weather     varchar   not null,
   temperature double,
   PRIMARY KEY (
     (myfamily, myseries, quantum(time, 15, 'm')),
      myfamily, myseries, time
   )
)
```

Let's say we want to write the following rows to the table:

```
"family1", "series1", 1234567, "hot", 23.5
"family2", "series99", 1234568, "windy", 19.8
```

The following example is a `put` call that can be used to write the two rows:

```
curl -XPOST http://127.0.0.1:8098/ts/v1/tables/GeoCheckin/keys --data '[{"myfamily":"family1","myseries":"series1","time":1234567,"weather":"hot","temperature":23.5},{"myfamily":"family2","myseries":"series99","time":1234568,"weather":"windy","temperature":19.8}]'
```

The following example is a `get` call that can be used to read one of the rows:

```
curl -XGET http://127.0.0.1:8098/ts/v1/tables/GeoCheckin/keys/myfamily/family1/myseries/series1/time/1234567
```

The following example is a `query` call that can be used to run a SELECT query to return all rows that satisfy the condition in the WHERE clause:

```
curl -XPOST http://127.0.0.1:8098/ts/v1/query --data "SELECT * FROM GeoCheckin WHERE myfamily = 'family2' AND myseries = 'series99' AND time >= 1200000 and time <= 1500000"
```

The following example is a `list_keys` call that can be used to stream a list of keys in the table.

```
curl -XGET http://127.0.0.1:8098/ts/v1/tables/GeoCheckin/list_keys
```

The following example is a `delete` call that can be used to delete one of the rows:

```
curl -XDELETE http://127.0.0.1:8098/ts/v1/tables/GeoCheckin/keys/myfamily/family2/myseries/series99/time/1234568
```