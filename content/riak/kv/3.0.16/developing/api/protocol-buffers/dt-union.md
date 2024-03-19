---
title: "PBC Data Type Union"
description: ""
project: "riak_kv"
project_version: "3.0.16"
lastmod: 2023-06-23T00:00:00-00:00
sitemap:
  priority: 0.9
menu:
  riak_kv-3.0.16:
    name: "Data Type Union"
    identifier: "pbc_dt_union"
    weight: 115
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/3.0.16/dev/references/protocol-buffers/dt-union
  - /riak/kv/3.0.16/dev/references/protocol-buffers/dt-union
---

A "union" type for update operations.

## Request

```protobuf
message DtOp {
    optional CounterOp counter_op = 1;
    optional SetOp     set_op     = 2;
    optional MapOp     map_op     = 3;
}
```

The included operation depends on the Data Type that is being updated.
`DtOp` messages are sent only as part of a [`DtUpdateReq`]({{<baseurl>}}riak/kv/3.0.16/developing/api/protocol-buffers/dt-store) message.

