---
title: "PBC Get Bucket Type"
description: ""
project: "riak_kv"
project_version: "3.0.16"
lastmod: 2023-06-23T00:00:00-00:00
sitemap:
  priority: 0.9
menu:
  riak_kv-3.0.16:
    name: "Get Bucket Type"
    identifier: "pbc_get_bucket_type"
    weight: 112
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/3.0.16/dev/references/protocol-buffers/get-bucket-type
  - /riak/kv/3.0.16/dev/references/protocol-buffers/get-bucket-type
---

Gets the bucket properties associated with a [bucket type]({{<baseurl>}}riak/kv/3.0.16/using/cluster-operations/bucket-types).

## Request

```protobuf
message RpbGetBucketTypeReq {
    required bytes type = 1;
}
```

Only the name of the bucket type needs to be specified (under `name`).

## Response

A bucket type's properties will be sent to the client as part of an
[`RpbBucketProps`]({{<baseurl>}}riak/kv/3.0.16/developing/api/protocol-buffers/get-bucket-props) message.

