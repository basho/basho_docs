---
title: PBC Yokozuna Index Put
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, yokozuna, search]
group_by: "Object/Key Operations"
---

Create a new index or modify an existing index.

## Request

```protobuf
message RpbYokozunaIndexPutReq {
    required RpbYokozunaIndex index  =  1;
}
```

Each message must contain a `RpbYokozunaIndex` message providing information about the index being stored. More on that message type can be found in [[PBC Yokozuna Index Get]].