---
title: PBC Datatype Fetch Response
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, datatypes]
group_by: "Object/Key Operations"
---

The response to a fetch request ([[`DtFetchReq`|PBC Datatypes Fetch Request]]). If the `include_context` option is specified, an opaque "context" value will be returned along with the user-readable data. When sending an update request, the client should send this context as well, just as one would send a vclock for standard KV updates. The `type` field indicates which value type to expect. When `value` field is missing from the message, the client should interpret it as not found.

## Response

```bash
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