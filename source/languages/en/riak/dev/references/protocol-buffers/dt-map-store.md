---
title: PBC Datatype Map Store
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, datatypes]
group_by: "Object/Key Operations"
---

An operation to be applied to a value stored in a map (the contents of an update operation). The operation field that is present depends on the type of the field to which it is applied.

## Request

```bash
message MapUpdate {
    /*
     * Flags only exist inside Maps and can only be enabled or
     * disabled, and there are no arguments to the operations.
     */
    enum FlagOp {
        ENABLE  = 1;
        DISABLE = 2;
    }

    required MapField  field       = 1;

    optional CounterOp counter_op  = 2;
    optional SetOp     set_op      = 3;

    /*
     * There is only one operation on a register, which is to set its
     * value, therefore the "operation" is the new value.
     */
    optional bytes     register_op = 4;
    optional FlagOp    flag_op     = 5;
    optional MapOp     map_op      = 6;
}
```

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
