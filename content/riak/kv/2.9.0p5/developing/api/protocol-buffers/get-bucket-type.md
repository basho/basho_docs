---
title: "PBC Get Bucket Type"
description: ""
project: "riak_kv"
project_version: "2.9.0p5"
menu:
  riak_kv-2.9.0p5:
    name: "Get Bucket Type"
    identifier: "pbc_get_bucket_type"
    weight: 112
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.9.0p5/dev/references/protocol-buffers/get-bucket-type
  - /riak/kv/2.9.0p5/dev/references/protocol-buffers/get-bucket-type
  - /riak/2.9.0p5/developing/api/protocol-buffers/get-bucket-type/
  - /riak/2.9.0/developing/api/protocol-buffers/get-bucket-type/
  - /riak/kv/2.9.0/developing/api/protocol-buffers/get-bucket-type/
  - /riak/kv/2.9.0p1/developing/api/protocol-buffers/get-bucket-type/
  - /riak/kv/2.9.0p2/developing/api/protocol-buffers/get-bucket-type/
  - /riak/kv/2.9.0p3/developing/api/protocol-buffers/get-bucket-type/
  - /riak/kv/2.9.0p4/developing/api/protocol-buffers/get-bucket-type/
---


Gets the bucket properties associated with a [bucket type]({{<baseurl>}}riak/kv/2.9.0p5/using/cluster-operations/bucket-types).

## Request

```protobuf
message RpbGetBucketTypeReq {
    required bytes type = 1;
}
```

Only the name of the bucket type needs to be specified (under `name`).

## Response

A bucket type's properties will be sent to the client as part of an
[`RpbBucketProps`]({{<baseurl>}}riak/kv/2.9.0p5/developing/api/protocol-buffers/get-bucket-props) message.
