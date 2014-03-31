---
title: PBC Data Type Union
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, datatypes]
group_by: "Object/Key Operations"
---

A "union" type for update operations. The included operation depends on the Data Type that is being updated. `DtOp` messages are sent only as part of a `[[DtUpdateReq|PBC Data Type Store}]]` message.

```protobuf
message DtOp {
    optional CounterOp counter_op = 1;
    optional SetOp     set_op     = 2;
    optional MapOp     map_op     = 3;
}
```