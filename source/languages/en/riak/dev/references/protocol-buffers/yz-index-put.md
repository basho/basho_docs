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

Create a new index or modify an existing index

## Request

```protobuf
message RpbYokozunaIndexPutReq {
    required RpbYokozunaIndex index  =  1;
}
```