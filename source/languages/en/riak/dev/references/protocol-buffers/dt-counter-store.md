---
title: PBC Data Type Counter Store
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, datatypes]
group_by: "Object/Key Operations"
---

An operation to update a [[counter|Using Data Types]].

## Request

```protobuf
message CounterOp {
    optional sint64 increment = 1;
}
```

The `increment` value specifies how much the counter will be incremented or decremented, depending on whether the `increment` value is positive or negative. This operation can be used to update counters that are stored on their own in a key or [[within a map|PBC Data Type Map Store]].