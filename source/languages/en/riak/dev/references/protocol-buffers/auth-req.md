---
title: PBC Auth Request
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Object/Key Operations"
---

Sends a username and password to Riak as part of an authentication request

## Request

```bash
message RpbAuthReq {
    required bytes user = 1;
    required bytes password = 2;
}
```

## Response

