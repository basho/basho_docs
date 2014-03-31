---
title: PBC Data Type Store
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, datatypes]
group_by: "Object/Key Operations"
---

A request to update the value of a [[Riak Data Type|Using Data Types]]. A `DtUpdateReq` message requires that you specify the location of the Data Type in Riak, which operations are to be performed, the request operations, and whether the Data Type's opaque context should be returned in the resulting `DtUpdateResp`.

## Request

```protobuf
message DtUpdateReq {
    // The identifier
    required bytes bucket = 1;

    // If this value is not specified, Riak will assign a key, as in normal
    // KV operations
    optional bytes key    = 2;

    // This value specifies the bucket type, NOT the Data Type, 
    // although the Data Type is constrained by the bucket type
    required bytes type   = 3;

    // Opaque update context
    optional bytes context = 4;

    // The operations to be performed
    required DtOp  op = 5;

    // Request options
    optional uint32 w               =  6;
    optional uint32 dw              =  7;
    optional uint32 pw              =  8;
    optional bool   return_body     =  9 [default=false];
    optional uint32 timeout         = 10;
    optional bool   sloppy_quorum   = 11;
    optional uint32 n_val           = 12;

    // When return_body is true, specifies whether the opaque context should
    // be returned in the DtUpdateResp message (discussed below)
    optional bool   include_context = 13 [default=true];
}
```

## Response

The response to a Data Type update request is analogous to `[[RpbPutResp|PBC Store Object]]` for KV operations. If the `return_body` is set in the update request message (as explained above), the message will include the opaque context of the Data Type (`context`) and the new value of the Data Type _after_ the update has completed (depending on whether the Data Type is a counter, set, or map). If no key was specified in the update request, it will include the Riak-assigned key (`key`).

```protobuf
message DtUpdateResp {
    optional bytes    key           = 1;
    optional bytes    context       = 2;
    optional sint64   counter_value = 3;
    repeated bytes    set_value     = 4;
    repeated MapEntry map_value     = 5;
}
```