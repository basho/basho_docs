---
title: "PBC Set Bucket Type"
description: ""
project: "riak_kv"
project_version: "2.2.1"
lastmod: 2017-03-08T00:00:00-00:00
sitemap:
  priority: 0.1
menu:
  riak_kv-2.2.1:
    name: "Set Bucket Type"
    identifier: "pbc_set_bucket_type"
    weight: 113
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.2.1/dev/references/protocol-buffers/set-bucket-type
  - /riak/kv/2.2.1/dev/references/protocol-buffers/set-bucket-type
---

Assigns a set of [bucket properties]({{<baseurl>}}riak/kv/2.2.1/developing/api/protocol-buffers/set-bucket-props) to a
[bucket type]({{<baseurl>}}riak/kv/2.2.1/developing/usage/bucket-types).

## Request

```protobuf
message RpbSetBucketTypeReq {
    required bytes type = 1;
    required RpbBucketProps props = 2;
}
```

The `type` field specifies the name of the bucket type as a binary. The
`props` field contains an [`RpbBucketProps`]({{<baseurl>}}riak/kv/2.2.1/developing/api/protocol-buffers/get-bucket-props).
