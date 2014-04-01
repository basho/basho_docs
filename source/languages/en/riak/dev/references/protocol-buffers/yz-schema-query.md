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

Query a Riak Search schema.

## Schema Message

```protobuf
message RpbYokozunaSchema {
    required bytes name    =  1;
    optional bytes content =  2;
}
```

If you are [[fetching a schema|PBC Yokozuna Schema Get Request]] by name, the return message (an `RpbYokozunaIndexGetResp` message) will include an `RpbYokozunaSchema` message.

If you are creating or [[updating a schema|PBC Yokozuna Schema Put Request]], you will need to specify the name of the schema as a binary (`name`) as well as the binary content of the schema (`content`).