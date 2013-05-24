---
title: PBC Search
project: riak
version: 1.2.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Query Operations"
---

Send a Search request to retrieve a list of documents, along with a few stats.

## Request


```bash
message RpbSearchQueryReq {
  required bytes  q      =  1;
  required bytes  index  =  2;
  optional uint32 rows   =  3;
  optional uint32 start  =  4;
  optional bytes  sort   =  5;
  optional bytes  filter =  6;
  optional bytes  df     =  7;
  optional bytes  op     =  8;
  repeated bytes  fl     =  9;
  optional bytes  presort = 10;
}
```

Required Parameters

* **q** - the bucket the index is for
* **index** - the name of the index to search

Optional Parameters

* **rows** - the maximum number of rows to return
* **start** - a start offset. the number of keys to skip before returning values
* **sort** - how the search results are to be sorted
* **filter** - filters search with additional query scoped to inline fields
* **df** - override the `default_field` setting in the schema file
* **op** - "and" or "or", to override the `default_op` operation setting in the schema file
* **fl** - return the fields limit
* **presort** - presort (key / score)


## Response

The results of a search query are returned as a repeating list of
0 or more RpbSearchDocs. RpbSearchDocs themselves are composed of
0 or more key/value pairs (RpbPair) that match the given request
parameters. It also returns the maximum search score and the number
of results.


```bash
// RbpPair is a generic key/value pair datatype used for other message types
message RpbPair {
  required bytes key = 1;
  optional bytes value = 2;
}
message RpbSearchDoc {
  repeated RpbPair fields = 1;
}
message RpbSearchQueryResp {
  repeated RpbSearchDoc docs      = 1;
  optional float        max_score = 2;
  optional uint32       num_found = 3;
}
```

Values

* **docs** - a list of docs that match the search request
* **max_score** - the top score returned
* **num_found** - returns the total number of values matched by this search


## Example

Request

Here we search for any animals that being with the string `pig`. We only
want the first 100, and sort the values by a `name` field.

```bash
RpbSearchQueryReq protoc decode:
q: "pig*"
index: "animals"
rows: 100
start: 0
sort: "name"

Hex     00 00 00 1A 1B 0A 04 70 69 67 2A 12 07 61 6E
        69 6D 61 6C 73 18 64 20 00 2A 04 6E 61 6D 65
Erlang  <<0,0,0,26,27,10,4,112,105,103,42,18,7,97,110,
          105,109,97,108,115,24,100,32,0,42,4,110,97,
          109,101>>
```

Response

```bash
Hex     00 00 00 36 1B 0A 1D 0A 0D 0A 06 61 6E 69 6D
        61 6C 12 03 70 69 67 0A 0C 0A 04 6E 61 6D 65
        12 04 66 72 65 64 0A 12 0A 10 0A 06 61 6E 69
        6D 61 6C 12 06 70 69 67 65 6F 6E 18 02
Erlang  <<0,0,0,54,27,10,29,10,13,10,6,97,110,105,109,
          97,108,18,3,112,105,103,10,12,10,4,110,97,
          109,101,18,4,102,114,101,100,10,18,10,16,10,
          6,97,110,105,109,97,108,18,6,112,105,103,
          101,111,110,24,2>>

RpbSearchQueryResp protoc decode:
docs {
  fields {
    key: "animal"
    value: "pig"
  }
  fields {
    key: "name"
    value: "fred"
  }
}
docs {
  fields {
    key: "animal"
    value: "pigeon"
  }
}
num_found: 2
```
