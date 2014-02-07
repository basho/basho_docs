---
title: PBC Yokozuna Index Query
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, yokozuna, search]
group_by: "Object/Key Operations"
---

```bash
message RpbYokozunaIndex {
    required bytes name   =  1;  // Index name
    optional bytes schema =  2;  // Schema name
}
```