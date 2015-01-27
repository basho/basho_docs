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
message RpbResetBucketReq {
    required bytes bucket = 1;
    optional bytes type = 2;
}
```

You must specify the name of the bucket (`bucket`) and optionally a
[[bucket type|Using Bucket Types]] using the `type` value. If you do not
specify a bucket type, the `default` bucket type will be used by Riak.

## Response

Only the message code is returned.

## Example

Request to reset the properties for the bucket `friends`:

#### Request

```bash
Hex      00 00 00 0A 1D 0A 07 66 72 69 65 6E 64 73
Erlang <<0,0,0,10,29,10,7,102,114,105,101,110,100,115>>

RpbResetBucketReq protoc decode:
bucket: "friends"

```

#### Response

```bash
Hex      00 00 00 01 1E
Erlang <<0,0,0,1,30>>

RpbResetBucketResp - only message code defined
```
