---
title: "PBC Yokozuna Index Put"
description: ""
project: "riak_kv"
project_version: "2.0.5"
lastmod: 2015-02-24T00:00:00-00:00
sitemap:
  priority: 0.1
menu:
  riak_kv-2.0.5:
    name: "Yokozuna Index Put"
    identifier: "pbc_yz_index_put"
    weight: 121
    parent: "apis_pbc"
toc: true
version_history:
  in: "2.0.0-2.9999.9999"
aliases:
  - /riak/2.0.5/dev/references/protocol-buffers/yz-index-put
  - /riak/kv/2.0.5/dev/references/protocol-buffers/yz-index-put
---

Create a new index or modify an existing index.

## Request

```protobuf
message RpbYokozunaIndexPutReq {
    required RpbYokozunaIndex index  =  1;
}
```

Each message must contain a `RpbYokozunaIndex` message providing
information about the index being stored.

```protobuf
message RpbYokozunaIndex {
    required bytes name   =  1;
    optional bytes schema =  2;
    optional uint32 n_val =  3;
}
```

Each message specifying an index must include the index's name as a
binary (as `name`). Optionally, you can specify a [`schema`]({{<baseurl>}}riak/kv/2.0.5/developing/usage/search-schemas) name and/or an `n_val`, i.e. the number of nodes on which the index is stored (for GET requests) or on which you wish the index to be stored (for PUT requests). An index's `n_val` must match the associated bucket's `n_val`.
