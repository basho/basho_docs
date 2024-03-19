---
title: "PBC Yokozuna Index Delete"
description: ""
project: "riak_kv"
project_version: "2.0.5"
lastmod: 2015-02-24T00:00:00-00:00
sitemap:
  priority: 0.1
menu:
  riak_kv-2.0.5:
    name: "Yokozuna Index Delete"
    identifier: "pbc_yz_index_delete"
    weight: 122
    parent: "apis_pbc"
toc: true
version_history:
  in: "2.0.0-2.9999.9999"
aliases:
  - /riak/2.0.5/dev/references/protocol-buffers/yz-index-delete
  - /riak/kv/2.0.5/dev/references/protocol-buffers/yz-index-delete
---

Delete a search index.

## Request

The `name` parameter is the name of the index to delete, as a binary.

```protobuf
message RpbYokozunaIndexDeleteReq {
    required bytes name  =  1;
}
```
