---
title: "PBC Yokozuna Index Get"
description: ""
project: "riak_kv"
project_version: "2.2.0"
menu:
  riak_kv-2.2.0:
    name: "Yokozuna Index Get"
    identifier: "pbc_yz_index_get"
    weight: 120
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.2.0/dev/references/protocol-buffers/yz-index-get
  - /riak/kv/2.2.0/dev/references/protocol-buffers/yz-index-get
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

If a `name` is passed through the `RpbYokozunaIndexGetReq` request, zero
or one `index` objects are returned. If `name` is empty, then a list of
all indexes will be returned.

Both requests will return a response of this form.

```protobuf
message RpbYokozunaIndexGetResp {
    repeated RpbYokozunaIndex index  =  1;
}
```

This message will contain any number of `RpbYokozunaIndex` messages,
depending on how many indexes are returned.

```protobuf
message RpbYokozunaIndex {
    required bytes name   =  1;
    optional bytes schema =  2;
    optional uint32 n_val =  3;
}
```

Each message specifying an index must include the index's name as a
binary (as `name`). Optionally, you can specify a [`schema`]({{<baseurl>}}riak/kv/2.2.0/developing/usage/search-schemas) name and/or an `n_val`, i.e. the number of nodes on which the
index is stored (for GET requests) or on which you wish the index to be
stored (for PUT requests). An index's `n_val` must match the associated
bucket's `n_val`.
