---
title: "PBC Data Type Union"
description: ""
project: "riak_kv"
project_version: "2.9.0p5"
menu:
  riak_kv-2.9.0p5:
    name: "Data Type Union"
    identifier: "pbc_dt_union"
    weight: 115
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.9.0p5/dev/references/protocol-buffers/dt-union
  - /riak/kv/2.9.0p5/dev/references/protocol-buffers/dt-union
  - /riak/2.9.0p5/developing/api/protocol-buffers/dt-union/
  - /riak/2.9.0/developing/api/protocol-buffers/dt-union/
  - /riak/kv/2.9.0/developing/api/protocol-buffers/dt-union/
  - /riak/kv/2.9.0p1/developing/api/protocol-buffers/dt-union/
  - /riak/kv/2.9.0p2/developing/api/protocol-buffers/dt-union/
  - /riak/kv/2.9.0p3/developing/api/protocol-buffers/dt-union/
  - /riak/kv/2.9.0p4/developing/api/protocol-buffers/dt-union/
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
`DtOp` messages are sent only as part of a [`DtUpdateReq`]({{<baseurl>}}riak/kv/2.9.0p5/developing/api/protocol-buffers/dt-store) message.
