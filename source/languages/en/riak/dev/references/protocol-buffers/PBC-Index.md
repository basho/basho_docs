---
title: PBC Index
project: riak
version: 1.2.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Query Operations"
moved: {
  '1.4.0-': '/references/apis/protocol-buffers/PBC-Index/'
}
---

Request a set of keys that match a secondary index query.

## Request


```bash
message RpbIndexReq {
    enum IndexQueryType {
        eq = 0;
        range = 1;
    }
    required bytes bucket = 1;
    required bytes index = 2;
    required IndexQueryType qtype = 3;
    optional bytes key = 4;
    optional bytes range_min = 5;
    optional bytes range_max = 6;
}
```


Required Parameters

* **bucket** - the bucket the index is for
* **index** - specify the index to use
* **qtype** - an IndexQueryType of either 0 (eq) or 1 (range)

Index queries are one of two types

* **eq** - Exactly match the query for the given `key`
* **range** - Match over a min and max range (`range_min`, `range_max`)

Optional Parameters

* **key** - the exact value to match by. only used if qtype is eq
* **range_min** - the minimum value for a range query to match. only used if qtype is range
* **range_max** - the maximum value for a range query to match. only used if qtype is range


## Response

The results of a Secondary Index query are returned as a repeating list of
0 or more keys that match the given request parameters.


```bash
message RpbIndexResp {
    repeated bytes keys = 1;
}
```

Values

* **keys** - a list of keys that match the index request


## Example

Request

Here we look for any exact matches of "chicken" on an "animal_bin" index for a bucket named "farm".

```bash
RpbIndexReq protoc decode:
bucket: "farm"
index: "animal_bin"
qtype: 0
key: "chicken"

Hex     00 00 00 1E 19 0A 04 66 61 72 6D 12 0A 61 6E 69
        6D 61 6C 5F 62 69 6E 18 00 22 07 63 68 69 63 6B 65 6E
Erlang  <<0,0,0,30,25,10,10,4,102,97,114,109,18,10,97,110,105,
          109,97,108,95,98,105,110,24,0,34,7,99,104,105,99,107,
          101,110>>
```

Response

```bash
Hex     00 00 00 0F 1A 0A 03 68 65 6E 0A 07 72 6F 6F 73 74 65 72
Erlang  <<0,0,0,15,26,10,3,104,101,110,10,7,114,111,111,115,116,101,114>>

RpbIndexResp protoc decode:
keys: "hen"
keys: "rooster"
```
