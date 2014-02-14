---
title: PBC Datatype Update Request
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, datatypes]
group_by: "Object/Key Operations"
---

The equivalent of Riak KV's [[`RpbPutReq`|PBC Store Object]] message. Results in an empty response or `DtUpdateResp` if `return_body` is specified or the key is assigned by the server. The request-time options are limited to those that are relevant to structured Riak datatypes.

## Request

```bash
message DtUpdateReq {
    // The identifier
    required bytes bucket = 1;
    optional bytes key    = 2; // missing key results in server-assigned key, like KV
    required bytes type   = 3; // bucket type, not data-type (but the data-type is constrained per bucket-type)

    // Opaque update-context
    optional bytes context = 4;

    // The operations
    required DtOp  op = 5;

    // Request options
    optional uint32 w               =  6;
    optional uint32 dw              =  7;
    optional uint32 pw              =  8;
    optional bool   return_body     =  9 [default=false];
    optional uint32 timeout         = 10;
    optional bool   sloppy_quorum   = 11;  // Experimental, may change/disappear
    optional uint32 n_val           = 12;  // Experimental, may change/disappear
    optional bool   include_context = 13 [default=true]; // When return_body is true, should the context be returned too?
}
```