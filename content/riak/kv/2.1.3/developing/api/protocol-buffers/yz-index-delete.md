---
title: "PBC Yokozuna Index Delete"
description: ""
project: "riak_kv"
project_version: "2.1.3"
menu:
  riak_kv-2.1.3:
    name: "Yokozuna Index Delete"
    identifier: "pbc_yz_index_delete"
    weight: 122
    parent: "apis_pbc"
toc: true
version_history:
  in: "2.0.0-2.9.10"
aliases:
  - /riak/2.1.3/dev/references/protocol-buffers/yz-index-delete
  - /riak/kv/2.1.3/dev/references/protocol-buffers/yz-index-delete
---

Delete a search index.

## Request

The `name` parameter is the name of the index to delete, as a binary.

```protobuf
message RpbYokozunaIndexDeleteReq {
    required bytes name  =  1;
}
```
