---
title: PBC Data Type Set Store
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, datatypes]
group_by: "Object/Key Operations"
---

An operation to update a set, either on its own (at the bucket/key level) or [[inside of a map|PBC Data Type Map Store]]. Set members are opaque binary values that can only be added (`adds`) or removed (`removes`) from a set. You can add or remove as many members of a set in a single message as you would like.

## Request

```protobuf
message SetOp {
    repeated bytes adds    = 1;
    repeated bytes removes = 2;
}
```