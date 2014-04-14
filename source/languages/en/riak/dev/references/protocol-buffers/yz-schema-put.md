---
title: PBC Yokozuna Schema Put Request
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, yokozuna, search]
group_by: "Object/Key Operations"
---

Create a new [[Riak Search schema|Advanced Search#Schemas]] or modify an existing schema.

## Request

```protobuf
message RpbYokozunaSchemaPutReq {
    required RpbYokozunaSchema schema =  1;
}
```

Each message must contain an `RpbYokozunaSchema` message under `schema`. More details on that message type can be found in [[PBC Yokozuna Schema Query]].