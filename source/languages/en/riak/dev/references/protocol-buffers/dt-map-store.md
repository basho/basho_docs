---
title: PBC Data Type Map Store
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, datatypes]
group_by: "Object/Key Operations"
---

An operation to be applied to a value stored in a map (the contents of an update operation). The operation field that is present depends on the type of the field to which it is applied. All operations apply to individual fields nested in the map, i.e. counter-specific operations apply to specified counters in the map, set-specific operations to sets, etc.

## Request

Operations on maps are requested using a `MapOp` message, which has the following structure:

```protobuf
message MapOp {
    repeated MapField  adds    = 1;
    repeated MapField  removes = 2;
    repeated MapUpdate updates = 3;
}
```

In a `MapOp` message, you can either add or remove fields (sets, counters, or maps) to or from the map or update a field or multiple fields. You can include as many field additions or removals and/or field updates as you wish.

Adding or removing a field involves including a `MapField` message in your `MapOp` operation:

```protobuf
message MapField {
    enum MapFieldType {
        COUNTER  = 1;
        SET      = 2;
        REGISTER = 3;
        FLAG     = 4;
        MAP      = 5;
    }
    required bytes        name = 1;
    required MapFieldType type = 2;
}
```

The `MapFieldType` specifies which type of field is being updated, and the `name` parameter specifies the name of that field.

If you wish to update a map field, you can do so using a `MapUpdate` message, which has the following structure:

```protobuf
message MapUpdate {
    enum FlagOp {
        ENABLE  = 1;
        DISABLE = 2;
    }
    required MapField  field       = 1;
    optional CounterOp counter_op  = 2;
    optional SetOp     set_op      = 3;
    optional bytes     register_op = 4;
    optional FlagOp    flag_op     = 5;
    optional MapOp     map_op      = 6;
}
```

The `MapField` parameter is explained above. The operations used to update fields depend on the Data Type in that field, i.e. `CounterOp` messages to update counters, `SetOp` messages to update sets, etc. Updating counters is covered in [[PBC Data Type Counter Store]] while updating sets is covered in [[PBC Data Type Set Store]].

If you are updating a flag, you do so by including a `FlagOp` message. As shown in the `MapUpdate` message above, this operation takes one of two values: `ENABLE` and `DISABLE` (`1` and `2`, respectively).

Updating a register does not involve sending a special message type. Instead, you must set the register to a desired value by specifying a binary for the `register_op` parameter.
