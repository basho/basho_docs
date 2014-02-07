---
title: PBC Datatype Counter Update
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, datatypes]
group_by: "Object/Key Operations"
---

An operation to update a counter, either on its own (at the bucket level) or within a map. The `increment` field can be positive or negative. When absent, the counter will increment by 1.

## Request

```bash
message CounterOp {
    optional sint64 increment = 1;
}
```

## Response

TODO