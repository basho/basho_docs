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

Assigns a set of [[bucket properties|PBC Set Bucket Properties]] to a [[bucket type|Using Bucket Types]].

## Request

```protobuf
message RpbSetBucketTypeReq {
    required bytes type = 1;
    required RpbBucketProps props = 2;
}
```

The `type` field specifies the name of the bucket type as a binary. The `props` field contains an `[[RpbBucketProps|PBC Get Bucket Properties]]`.