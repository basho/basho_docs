---
title: "PBC Set Bucket Properties"
description: ""
project: "riak_kv"
project_version: "2.0.8"
menu:
  riak_kv-2.0.8:
    name: "Set Bucket Properties"
    identifier: "pbc_set_bucket_props"
    weight: 103
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.0.8/dev/references/protocol-buffers/set-bucket-props
  - /riak/kv/2.0.8/dev/references/protocol-buffers/set-bucket-props
---

Sets the properties for a bucket.

## Request

```protobuf
message RpbSetBucketReq {
    required bytes bucket = 1;
    required RpbBucketProps props = 2;
    optional bytes type = 3;
}
```

You must specify the name of the bucket (`bucket`) and include an
`RpbBucketProps` message. More on that message type can be found in the
[PBC Get Bucket Properties]({{<baseurl>}}riak/kv/2.0.8/developing/api/protocol-buffers/get-bucket-props) documentation.

You can also specify a [bucket type]({{<baseurl>}}riak/kv/2.0.8/developing/usage/bucket-types) using the
`type` value. If you do not specify a bucket type, the `default` bucket
type will be used by Riak.

## Response

Only the message code is returned.

## Example

Change `allow_mult` to true for the bucket `friends`:

#### Request

```bash
Hex      00 00 00 0E 15 0A 07 66 72 69 65 6E 64 73 12 02
         10 01
Erlang <<0,0,0,14,21,10,7,102,114,105,101,110,100,115,18,2,16,1>>

RpbSetBucketReq protoc decode:
bucket: "friends"
props {
  allow_mult: true
}

```

#### Response

```bash
Hex      00 00 00 01 16
Erlang <<0,0,0,1,22>>

RpbSetBucketResp - only message code defined
```
