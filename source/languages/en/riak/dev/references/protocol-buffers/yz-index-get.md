---
title: PBC Yokozuna Index Get
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, yokozuna, search]
group_by: "Object/Key Operations"
---

Retrieve a search index from Riak Search

## Request

```protobuf
message RpbYokozunaIndexGetReq {
    optional bytes name  =  1;  // Index name
}
```

## Response

```protobuf
message RpbYokozunaIndexGetResp {
    repeated RpbYokozunaIndex index  =  1;
}
```