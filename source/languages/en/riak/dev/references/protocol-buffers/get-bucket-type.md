---
title: PBC Get Bucket Type
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer, bucket-type]
group_by: "Object/Key Operations"
---

Gets the bucket properties associated with a [[bucket type|Using Bucket Types]].

## Request

```protobuf
message RpbGetBucketTypeReq {
    required bytes type = 1;
}
```

Only the name of the bucket type needs to be specified (under `name`).

## Response

A bucket type's properties will be sent to the client as part of an `[[RpbBucketProps|PBC Get Bucket Properties]]` message.