---
title: "HTTP Client API"
description: "HTTP Client API"
menu:
  riak_ts-1.4.0:
    name: "HTTP"
    identifier: "ts_http_api"
    weight: 401
    parent: "develop"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/developing/http/
---


This document will cover the API calls for accessing Riak TS data over HTTP.


## Overview

All Riak TS calls use the '/ts' endpoint. Using the following table schema, each API call has a corresponding URL:

```sql
CREATE TABLE GeoCheckin
(
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR   NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (region, state, QUANTUM(time, 15, 'm')),
     region, state, time
   )
)
```

| Call  | Request URL         | Type | Description  |
|:------|---------------------|:--------------:|--------------|
|put    | /ts/v1/tables/GeoCheckin/keys '[»Row(s)«]' | POST | put a single or a batch of rows |
|get    | /ts/v1/tables/GeoCheckin/keys/region/North%20West/state/WA/time/14201136 | GET | single-key get of a value |
|delete | /ts/v1/tables/GeoCheckin/keys/region/North%20West/state/WA/time/14201136 | DELETE | single-key delete         |
|query  | /ts/v1/query --data "»Query«"  | POST | execute a query |
|list_keys | /ts/v1/tables/GeoCheckin/list_keys  | GET | streaming list keys     |


## Percent-encoding

All keys need to be implemented in the query string using percent-encoding (or URL encoding), which can be seen in the examples above for the `get` and `delete` operations. Failing to do so will result in a `400 - Bad Request` response. Percent-encoding variants where a space character is replaced with a `+` will work as expected, but it is recommended that modern variants are used where spaces are encoded as `%20`.

## Keys and Values

| Call      | Method | Type  |
|-----------|--------|-------|
| put       | POST    | Write |
| get       | GET    | Read  |
| delete    | DELETE | Write |
| query     | POST   | Query |
| list_keys | GET    | Read  |

Single-key `get` and `delete` requires the complete primary key to be serialized in the path of the URL, using the order defined in the schema to determine which row to get. The entire row will be returned.

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
   state       VARCHAR   NOT NULL,
   city        VARCHAR   NOT NULL,
   time        TIMESTAMP NOT NULL,
   weather     VARCHAR   NOT NULL,
   temperature DOUBLE,
   PRIMARY KEY (
     (state, city, QUANTUM(time, 15, 'm')),
      state, city, time
   )
)
```

A `query` call can be used to run a `CREATE TABLE` statement to create the GeoCheckin table:

```
$ curl -XPOST http://127.0.0.1:8098/ts/v1/query --data "CREATE TABLE GeoCheckin (state VARCHAR NOT NULL, city VARCHAR NOT NULL, time TIMESTAMP NOT NULL, weather VARCHAR NOT NULL, temperature DOUBLE, PRIMARY KEY ((state, city, QUANTUM(time, 15, 'm')), state, city, time))"

{"success":true}
```

Let's write the following rows to the table:

```
"Florida", "Miami", 1234567, "hot", 23.5
"Illinois", "Chicago", 1234568, "windy", 19.8
```

A `put` call can be used to write the two rows:

```
$ curl -XPOST http://127.0.0.1:8098/ts/v1/tables/GeoCheckin/keys --data '[{"state":"Florida","city":"Miami","time":1234567,"weather":"hot","temperature":23.5},{"state":"Illinois","city":"Chicago","time":1234568,"weather":"windy","temperature":19.8}]'

{"success":true}
```

A `get` call can be used to read one of the rows:

```
$ curl -XGET http://127.0.0.1:8098/ts/v1/tables/GeoCheckin/keys/state/Florida/city/Miami/time/1234567

{"state":"Florida","city":"Miami","time":1234567,"weather":"hot","temperature":23.5}
```

A `query` call can be used to run a `SELECT` query to display all columns of the GeoCheckin table and return all rows that satisfy the WHERE clause:

```
$ curl -XPOST http://127.0.0.1:8098/ts/v1/query --data "SELECT * FROM GeoCheckin WHERE state = 'Illinois' AND city = 'Chicago' AND time >= 1200000 and time <= 1500000"

{"columns":["state","city","time","weather","temperature"],"rows":[["Illinois","Chicago",1234568,"windy",19.8]]}
```

A `list_keys` call can be used to stream a list of keys in the table:

```
$ curl -XGET http://127.0.0.1:8098/ts/v1/tables/GeoCheckin/list_keys

http://127.0.0.1:8098/ts/v1/tables/GeoCheckin/keys/state/Florida/city/Miami/time/1234567
http://127.0.0.1:8098/ts/v1/tables/GeoCheckin/keys/state/Illinois/city/Chicago/time/1234568
```

A `delete` call can be used to delete one of the rows:

```
$ curl -XDELETE http://127.0.0.1:8098/ts/v1/tables/GeoCheckin/keys/state/Illinois/city/Chicago/time/1234568

{"success":true}
```

The following are bad calls that result in various errors:

```
$ curl -XPOST http://127.0.0.1:8098/ts/v1/query --data "CREATE TABLE GeoCheckin (state varchar not null, city varchar not null, time timestamp not null, weather varchar not null, temperature double, PRIMARY KEY ((state, city, quantum(timecol, 15, 'm')), state, city, time))"

Query error: Local key does not match primary key
```
```
$ curl -XPOST http://127.0.0.1:8098/ts/v1/tables/GeoCheckin/keys --data '[{"state":"Florida","city":"Miami","time":1234567,"weather":50,"temperature":23.5}]'

Bad value for field "weather" of type varchar in table "GeoCheckin"
```
```
$ curl -XGET http://127.0.0.1:8098/ts/v1/tables/GeoCheckin2/keys/state/Florida/city/Miami/time/1234567

Table "GeoCheckin2" does not exist
```
```
$ curl -XPOST http://192.168.99.14:8098/ts/v1/query --data "SELECT ? FROM GeoCheckin WHERE state = 'Illinois' AND city = 'Chicago' AND time >= 1200000 and time <= 1500000"

Query error: Unexpected token '?'.
```
```
$ curl -XDELETE http://127.0.0.1:8098/ts/v1/tables/GeoCheckin/keys/state/Colorado/city/Denver/time/1234570

Key not found

```
