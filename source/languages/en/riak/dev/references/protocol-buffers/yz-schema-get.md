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

The response message will include an `RpbYokozunaSchema` message. More information about this message type can be found in [[PBC Yokozuna Schema Query]].