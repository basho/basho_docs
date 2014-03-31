---
title: PBC Datatype Fetch
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, datatypes]
group_by: "Object/Key Operations"
---

The equivalent of `[[RpbGetReq|PBC Fetch Object]]` for [[Riak Data Types|Using Data Types]]. This request results in a `DtFetchResp` message (explained in the **Response** section below). Request-time options are limited to those that are relevant to structured Riak Data Types.

## Request

When fetching a Riak Data Types, you must specify the Data Type's location via `bucket`, `key`, and bucket type (`type`), as well as [[replication properties]] such as `r`, `n_val`, and `notfound_ok`, a `timeout` for the request, and whether or not the Data Type's opaque "context" should be included in the return message.

```protobuf
message DtFetchReq {
    // The identifier: bucket, key, and bucket type
    required bytes bucket = 1;
    required bytes key    = 2;
    required bytes type  = 3;

    // Request options
    optional uint32 r             =  4;
    optional uint32 pr            =  5;
    optional bool   basic_quorum  =  6;
    optional bool   notfound_ok   =  7;
    optional uint32 timeout       =  8;
    optional bool   sloppy_quorum =  9;
    optional uint32 n_val         = 10;

    // For read-only requests or context-free operations, you can set
    // this to false to reduce the size of the response payload.
    optional bool include_context = 11 [default=true];
}
```

## Response

The response to a fetch request (`[[DtFetchReq|PBC Data Type Fetch Request]]`) is a `DtFetchResp` message. If the `include_context` option is specified, an opaque "context" value will be returned along with the user-readable data. When sending an update request, the client should send this context as well, just as one would send a [[vclock|Vector Clocks]] for standard KV updates.

The `type` field indicates which value type to expect. When the `value` field is missing from the message, the client should interpret it as not found.

```protobuf
message DtFetchResp {
    enum DataType {
        COUNTER = 1;
        SET     = 2;
        MAP     = 3;
    }

    optional bytes    context = 1;
    required DataType type    = 2;
    optional DtValue  value   = 3;
}
```