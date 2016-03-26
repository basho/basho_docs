---
title: "PBC Set Bucket Type"
description: ""
project: "riak_kv"
project_version: "2.1.3"
menu:
  riak_kv-2.1.3:
    name: "Set Bucket Type"
    identifier: "pbc_set_bucket_type"
    weight: 113
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.1.3/dev/references/protocol-buffers/set-bucket-type
---

Assigns a set of [[bucket properties|PBC Set Bucket Properties]] to a
[[bucket type|Using Bucket Types]].

## Request

```protobuf
message RpbSetBucketTypeReq {
    required bytes type = 1;
    required RpbBucketProps props = 2;
}
```

The `type` field specifies the name of the bucket type as a binary. The
`props` field contains an `[[RpbBucketProps|PBC Get Bucket
Properties]]`.
