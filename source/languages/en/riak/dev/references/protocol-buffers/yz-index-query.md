---
title: PBC Yokozuna Index Query
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, yokozuna, search]
group_by: "Object/Key Operations"
---

Query a Riak Search index.

## Index Message

```protobuf
message RpbYokozunaIndex {
    required bytes name   =  1;
    optional bytes schema =  2;
    optional uint32 n_val =  3;
}
```

Each message specifying an index must include the index's name as a binary (as `name`). Optionally, you can specify a `[[schema|Advanced Search#Schemas]]` and/or an `n_val`, i.e. the number of nodes on which the index is stored (for GET requests) or on which you wish the index to be stored (for PUT requests).