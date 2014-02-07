---
title: PBC Yokozuna Schema Get Request
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, yokozuna, search]
group_by: "Object/Key Operations"
---

```bash
message RpbYokozunaSchemaGetReq {
    required bytes name  =  1;  // Schema name
}
```