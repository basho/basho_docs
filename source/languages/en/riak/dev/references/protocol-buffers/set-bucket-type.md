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

Assigns a set of bucket properties to a bucket type name

## Request

```bash
message RpbSetBucketTypeReq {
    required bytes type = 1;
    required RpbBucketProps props = 2;
}
```

## Response

TODO