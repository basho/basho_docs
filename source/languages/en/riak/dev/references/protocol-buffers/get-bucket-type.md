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

Gets the bucket properties associated with a bucket type

## Request

```bash
message RpbGetBucketTypeReq {
    required bytes type = 1;
}
```