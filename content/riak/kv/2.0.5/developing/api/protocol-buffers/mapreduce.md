---
title: "PBC MapReduce"
description: ""
project: "riak_kv"
project_version: "2.0.5"
menu:
  riak_kv-2.0.5:
    name: "MapReduce"
    identifier: "pbc_mapreduce"
    weight: 107
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.0.5/dev/references/protocol-buffers/mapreduce
  - /riak/kv/2.0.5/dev/references/protocol-buffers/mapreduce
---

Execute a MapReduce job.

## Request


```protobuf
message RpbMapRedReq {
    required bytes request = 1;
    required bytes content_type = 2;
}
```


Required Parameters

* `request` --- MapReduce job
* `content_type` - Encoding for MapReduce job

Mapreduce jobs can be encoded in two different ways

* `application/json` --- JSON-encoded MapReduce job
* `application/x-erlang-binary` --- Erlang external term format

The JSON encoding is the same as [REST API]({{<baseurl>}}riak/kv/2.0.5/developing/usage/mapreduce/#rest) and
the external term format is the same as the [local Erlang API]({{<baseurl>}}riak/kv/2.0.5/developing/app-guide/advanced-mapreduce/#erlang)

## Response

The results of the MapReduce job is returned for each phase that
generates a result, encoded in the same format the job was submitted in.
Multiple response messages will be returned followed by a final message
at the end of the job.

```protobuf
message RpbMapRedResp {
    optional uint32 phase = 1;
    optional bytes response = 2;
    optional bool done = 3;
}
```


Values

* `phase` --- Phase number of the MapReduce job
* `response` - Response encoded with the content_type submitted
* `done` - Set `true` on the last response packet

## Example

Here is how submitting a JSON encoded job to sum up a bucket full of
JSON encoded values.

```
{"inputs": "bucket_501653",
 "query":
    [{"map": {"arg": null,
              "name": "Riak.mapValuesJson",
              "language": "javascript",
              "keep": false}},
     {"reduce": {"arg": null,
                   "name": "Riak.reduceSum",
                   "language": "javascript",
                   "keep": true}}]}"
```

Request

```bash
Hex      00 00 00 F8 17 0A E2 01 7B 22 69 6E 70 75 74 73
         22 3A 20 22 62 75 63 6B 65 74 5F 35 30 31 36 35
         33 22 2C 20 22 71 75 65 72 79 22 3A 20 5B 7B 22
         6D 61 70 22 3A 20 7B 22 61 72 67 22 3A 20 6E 75
         6C 6C 2C 20 22 6E 61 6D 65 22 3A 20 22 52 69 61
         6B 2E 6D 61 70 56 61 6C 75 65 73 4A 73 6F 6E 22
         2C 20 22 6C 61 6E 67 75 61 67 65 22 3A 20 22 6A
         61 76 61 73 63 72 69 70 74 22 2C 20 22 6B 65 65
         70 22 3A 20 66 61 6C 73 65 7D 7D 2C 20 7B 22 72
         65 64 75 63 65 22 3A 20 7B 22 61 72 67 22 3A 20
         6E 75 6C 6C 2C 20 22 6E 61 6D 65 22 3A 20 22 52
         69 61 6B 2E 72 65 64 75 63 65 53 75 6D 22 2C 20
         22 6C 61 6E 67 75 61 67 65 22 3A 20 22 6A 61 76
         61 73 63 72 69 70 74 22 2C 20 22 6B 65 65 70 22
         3A 20 74 72 75 65 7D 7D 5D 7D 12 10 61 70 70 6C
         69 63 61 74 69 6F 6E 2F 6A 73 6F 6E
Erlang <<0,0,0,248,23,10,226,1,123,34,105,110,112,117,116,115,34,58,32,34,98,
         117,99,107,101,116,95,53,48,49,54,53,51,34,44,32,34,113,117,101,114,
         121,34,58,32,91,123,34,109,97,112,34,58,32,123,34,97,114,103,34,58,32,
         110,117,108,108,44,32,34,110,97,109,101,34,58,32,34,82,105,97,107,46,
         109,97,112,86,97,108,117,101,115,74,115,111,110,34,44,32,34,108,97,
         110,103,117,97,103,101,34,58,32,34,106,97,118,97,115,99,114,105,112,
         116,34,44,32,34,107,101,101,112,34,58,32,102,97,108,115,101,125,125,
         44,32,123,34,114,101,100,117,99,101,34,58,32,123,34,97,114,103,34,58,
         32,110,117,108,108,44,32,34,110,97,109,101,34,58,32,34,82,105,97,107,
         46,114,101,100,117,99,101,83,117,109,34,44,32,34,108,97,110,103,117,
         97,103,101,34,58,32,34,106,97,118,97,115,99,114,105,112,116,34,44,32,
         34,107,101,101,112,34,58,32,116,114,117,101,125,125,93,125,18,16,97,
         112,112,108,105,99,97,116,105,111,110,47,106,115,111,110>>

RpbMapRedReq protoc decode:
request: "{"inputs": "bucket_501653", "query": [{"map": {"arg": null,
"name": "Riak.mapValuesJson", "language": "javascript", "keep": false}},
 {"reduce": {"arg": null, "name": "Riak.reduceSum", "language":
"javascript", "keep": true}}]}"
content_type: "application/json"

```


Response 1 - result from phase 1

```bash
Hex      00 00 00 08 18 08 01 12 03 5B 39 5D
Erlang <<0,0,0,8,24,8,1,18,3,91,57,93>>

RpbMapRedResp protoc decode:
phase: 1
response: "[[9]]"

```


Response 2 - end of MapReduce job

```bash
Hex      00 00 00 03 18 18 01
Erlang <<0,0,0,3,24,24,1>>

RpbMapRedResp protoc decode:
done: true

```
