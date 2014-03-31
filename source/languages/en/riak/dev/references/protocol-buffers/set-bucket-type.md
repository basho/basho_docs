---
title: PBC Set Bucket Type
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, bucket-type]
group_by: "Object/Key Operations"
---

Assigns a set of [[bucket properties|PBC Set Bucket Properties]], or `props`. to a [[bucket type|Using Bucket Types]] name (`type`).

## Request

```protobuf
message RpbSetBucketTypeReq {
    required bytes type = 1;
    required RpbBucketProps props = 2;
}
```