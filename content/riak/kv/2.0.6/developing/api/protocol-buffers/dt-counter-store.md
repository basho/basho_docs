---
title: "PBC Data Type Counter Store"
description: ""
project: "riak_kv"
project_version: "2.0.6"
menu:
  riak_kv-2.0.6:
    name: "Data Type Counter Store"
    identifier: "pbc_dt_counter_store"
    weight: 117
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.1.3/dev/references/protocol-buffers/dt-counter-store
canonical_link: "docs.basho.com/riak/kv/latest/developing/api/protocol-buffers/dt-counter-store"
---

An operation to update a [counter](/riak/kv/2.0.6/developing/data-types).

## Request

```protobuf
message CounterOp {
    optional sint64 increment = 1;
}
```

The `increment` value specifies how much the counter will be incremented
or decremented, depending on whether the `increment` value is positive
or negative. This operation can be used to update counters that are
stored on their own in a key or [within a map](/riak/kv/2.0.6/developing/api/protocol-buffers/dt-map-store).
