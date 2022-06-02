---
title: "PBC Secondary Indexes"
description: ""
project: "riak_kv"
project_version: "2.0.7"
menu:
  riak_kv-2.0.7:
    name: "Secondary Indexes"
    identifier: "pbc_secondary_indexes"
    weight: 108
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.0.7/dev/references/protocol-buffers/secondary-indexes
  - /riak/kv/2.0.7/dev/references/protocol-buffers/secondary-indexes
---

Request a set of keys that match a secondary index query.

## Request

```protobuf
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
    optional bool return_terms = 7;
    optional bool stream = 8;
    optional uint32 max_results = 9;
    optional bytes continuation = 10;
    optional uint32 timeout = 11;
    optional bytes type = 12;
    optional bytes term_regex = 13;
    optional bool pagination_sort = 14;
}
```

#### Required Parameters

Parameter | Description
:---------|:-----------
`bucket` | The name of the bucket in which the Data Type is stored
`index` | The name of the index to be queried
`qtype` | The type of index query to be performed. This can take either of the two possible values of the `IndexQueryType` enum: `eq` for an exact index match for the given `key` or `range` for a range query

#### Optional Parameters

Parameter | Description
:---------|:-----------
`key` | The name of the index to be queried if `qtype` is set to `eq`
`range_min` and `range_max` | The minimum and maximum values for a range query if `qtype` is set to `range`
`return_terms` | If set to `true`, the response will include matched indexed values (for range queries only)
`stream` | If set to `true`, keys matching the index query will be streamed to the client instead of waiting for `max_results` or the full result to be tabulated
`max_results` | If pagination is turned on, the number of results to be returned to the client
`continuation` | If set to `true`, values are returned in a paginated response
`timeout` | The timeout duration, in milliseconds, after which Riak will return an error message
`type` | The bucket type of the bucket that is being queried. If not set, the bucket type `default` will be used. Learn more about [using bucket types]({{<baseurl>}}riak/kv/2.0.7/developing/usage/bucket-types).
`term_regex` | If set to a regular expression (as a binary), a term filter will be applied to the index query
`pagination_sort` | If set to `true`, paginated results will be sorted, first by index value, then by key

## Response

The results of a Secondary Index query are returned as a repeating list
of 0 or more keys that match the given request parameters.

```protobuf
message RpbIndexResp {
    repeated bytes keys = 1;
    repeated RpbPair results = 2;
    optional bytes continuation = 3;
    optional bool done = 4;
}
```

#### Values

Parameter | Description
:---------|:-----------
`keys` | A list of keys that match the index request
`results` | If `return_terms` is specified with range queries, used to return matched index values as key/value pairs in `RpbPair` messages. More on `RpbPair` messages can be found in [PBC Fetch Object]({{<baseurl>}}riak/kv/2.0.7/developing/api/protocol-buffers/fetch-object).
`continuation` | Used for paginated responses
`done` | Used for streaming. The value will be `true` when the current stream is done (either `max_results` has been reached or there are no more results).

## Example

#### Request

Here we look for any exact matches of `chicken` on an `animal_bin` index
for a bucket named `farm`.

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

#### Response

```bash
Hex     00 00 00 0F 1A 0A 03 68 65 6E 0A 07 72 6F 6F 73 74 65 72
Erlang  <<0,0,0,15,26,10,3,104,101,110,10,7,114,111,111,115,116,101,114>>

RpbIndexResp protoc decode:
keys: "hen"
keys: "rooster"
```
