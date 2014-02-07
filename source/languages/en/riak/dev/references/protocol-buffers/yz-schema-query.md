---
title: PBC Yokozuna Schema Query
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, yokozuna, search]
group_by: "Object/Key Operations"
---

```bash
message RpbYokozunaSchema {
    required bytes name    =  1;  // Index name
    optional bytes content =  2;  // Schema data
}
```