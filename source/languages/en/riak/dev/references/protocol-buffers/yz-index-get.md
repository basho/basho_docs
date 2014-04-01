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

This message will contain any number of `RpbYokozunaIndex` messages, depending on how many indexes are returned. Those messages are structured like this:

```protobuf
message RpbYokozunaIndex {
    required bytes name   =  1;
    optional bytes schema =  2;
    optional uint32 n_val =  3;
}
```

Each message will include the name of the index (`name`), the schema (`schema`), and the `n_val`, i.e. the number of replicas returned.
