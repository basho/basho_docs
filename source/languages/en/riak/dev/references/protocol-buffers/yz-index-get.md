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

Retrieve a search index from Riak Search.

## Request

The `name` parameter is the name of the index to fetch as a binary.

```protobuf
message RpbYokozunaIndexGetReq {
    optional bytes name  =  1;
}
```

## Response

In response to a `RpbYokozunaIndexGetReq`, Riak will return a message of the following form:

```protobuf
message RpbYokozunaIndexGetResp {
    repeated RpbYokozunaIndex index  =  1;
}
```

This message will contain any number of `RpbYokozunaIndex` messages, depending on how many indexes are returned. More on this message type can be found in [[PBC Yokozuna Index Query]].
