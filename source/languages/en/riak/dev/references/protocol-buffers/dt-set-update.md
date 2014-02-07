---
title: PBC Datatype Set Update
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, datatypes]
group_by: "Object/Key Operations"
---

An operation to update a set, either on its own (at the bucket level) or inside of a map. Set members are opaque binary values that can only be added or removed from a set.

## Request

```bash
message SetOp {
    repeated bytes adds    = 1;
    repeated bytes removes = 2;
}
```

## Response

TODO