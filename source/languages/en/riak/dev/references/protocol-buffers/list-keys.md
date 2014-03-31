---
title: PBC List Keys
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Bucket Operations"
moved: {
  '1.4.0-': '/references/apis/protocol-buffers/PBC-List-Keys'
}
---

List all of the keys in a bucket. This is a streaming call, with multiple response messages sent for each request.

<div class="note">
<div class="title">Not for production use</div>
<p>This operation requires traversing all keys stored in the cluster and should
not be used in production.</p>
</div>

## Request

```protobuf
message RpbListKeysReq {
    required bytes bucket = 1;
}
```


Optional Parameters

* **bucket** - bucket to get keys from

## Response


```protobuf
message RpbListKeysResp {
    repeated bytes keys = 1;
    optional bool done = 2;
}
```


Values

* **keys** - batch of keys in the bucket.
* **done** - set true on the last response packet
