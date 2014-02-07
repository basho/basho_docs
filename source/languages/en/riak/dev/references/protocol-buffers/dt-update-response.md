---
title: PBC Datatype Update Response
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, datatypes]
group_by: "Object/Key Operations"
---

The equivalent of Riak KV's [[`RpbPutRequest`|PBC Store Object]]. Contains the assigned key it was assigned by the server, and the resulting value and context if `return_body` is set.

## Response

```bash
message DtUpdateResp {
    // The key, if assigned by the server
    optional bytes    key           = 1;

    // The opaque update context and value, if return_body was set.
    optional bytes    context       = 2;
    optional sint64   counter_value = 3;
    repeated bytes    set_value     = 4;
    repeated MapEntry map_value     = 5;
}
```