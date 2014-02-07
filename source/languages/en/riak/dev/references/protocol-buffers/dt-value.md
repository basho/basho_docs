---
title: PBC Datatype Value Request
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, datatypes]
group_by: "Object/Key Operations"
---

The value of the fetched datatype. If present in the response, empty values (such as empty sets or maps) should be treated as empty.

## Request

```bash
message DtValue {
    optional sint64   counter_value = 1;
    repeated bytes    set_value     = 2;
    repeated MapEntry map_value     = 3;
}
```

## Response

TODO