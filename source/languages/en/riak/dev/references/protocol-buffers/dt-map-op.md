---
title: PBC Datatype Map Field Operations
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, datatypes]
group_by: "Object/Key Operations"
---

An operation to update a map. All operations apply to individual fields nested in the map.

## Request

```bash
message MapOp {
    /*
     * ADD creates a new, "empty" value under a field. REMOVE removes
     * a field and value from the Map. UPDATE applies type-specific
     * operations to the values stored in the Map.
     */
    repeated MapField  adds    = 1;
    repeated MapField  removes = 2;
    repeated MapUpdate updates = 3;
}
```