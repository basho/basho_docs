---
title: PBC Yokozuna Schema Get
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, yokozuna, search]
group_by: "Object/Key Operations"
---

Fetch a [[search schema]] from Riak Search.

## Request

In a request message, you only need to specify the name of the schema as a binary (under `name`);

```protobuf
message RpbYokozunaSchemaGetReq {
    required bytes name  =  1;  // Schema name
}
```

## Response

```protobuf
message RpbYokozunaSchemaGetResp {
  required RpbYokozunaSchema schema =  1;
}
```

The response message will include a `RpbYokozunaSchema` structure.

```protobuf
message RpbYokozunaSchema {
    required bytes name    =  1;
    optional bytes content =  2;
}
```

This message includes the schema `name` and its xml `content`.
