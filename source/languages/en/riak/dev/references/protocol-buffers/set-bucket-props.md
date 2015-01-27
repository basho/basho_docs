---
title: PBC Set Bucket Properties
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Bucket Operations"
moved: {
  '1.4.0-': '/references/apis/protocol-buffers/PBC-Set-Bucket-Properties'
}
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
[[PBC Get Bucket Properties]] documentation.

You can also specify a [[bucket type|Using Bucket Types]] using the
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
