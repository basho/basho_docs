---
title: "PBC Yokozuna Index Delete"
description: ""
project: "riak_kv"
project_version: "2.0.6"
menu:
  riak_kv-2.0.6:
    name: "Yokozuna Index Delete"
    identifier: "pbc_yz_index_delete"
    weight: 122
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.1.3/dev/references/protocol-buffers/yz-index-delete
canonical_link: "docs.basho.com/riak/kv/latest/developing/api/protocol-buffers/yz-index-delete.md"
---

Delete a search index.

## Request

The `name` parameter is the name of the index to delete, as a binary.

```protobuf
message RpbYokozunaIndexDeleteReq {
    required bytes name  =  1;
}
```
