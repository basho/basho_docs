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

An operation to be applied to a value stored in a map (the contents of an update operation). The operation field that is present depends on the type of the field to which it is applied. All operations apply to individual fields nested in the map, i.e. counter-specific operations apply to specified counters in the map, etc. Possible values for those operations are listed in comments in the message spec below.

## Request

```protobuf
message MapUpdate {
    // If a flag inside of a map is being updated, there are two options:
    enum FlagOp {
        ENABLE  = 1;
        DISABLE = 2;
    }

    // Specifies which map field is being updated
    required MapField  field       = 1;

    // If a counter and/or set is being updated, the type of update needs to
    // be specified
    optional CounterOp counter_op  = 2;
    optional SetOp     set_op      = 3;

    // There is only one operation on a register, which is to set its
    // value, therefore the "operation" is the new value.
    optional bytes     register_op = 4;

    // If an operation is performed on a flag, it must take one of the values
    // available in the FlagOp enum (above):
    optional FlagOp    flag_op     = 5;

    // Finally, you must specify a map operation, as explained below
    optional MapOp     map_op      = 6;
}
```

## Request

When updating map fields, you need to specify which operation is being performed on them (using a `MapOp` message).

```protobuf
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
