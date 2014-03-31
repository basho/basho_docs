---
title: PBC Reset Bucket Properties
project: riak
version: 2.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Object/Key Operations"
---

Request to reset the properties of a given bucket or bucket type.

## Request

```protobuf
message RpbSetBucketReq {
    required bytes bucket = 1;
    required RpbBucketProps props = 2;
    optional bytes type = 3;
}
```