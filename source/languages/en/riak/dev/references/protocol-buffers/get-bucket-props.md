---
title: PBC Get Bucket Properties
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Bucket Operations"
moved: {
  '1.4.0-': '/references/apis/protocol-buffers/PBC-Get-Bucket-Properties'
}
---

Get the properties for a bucket

## Request


```bash
message RpbGetBucketReq {
    required bytes bucket = 1;
}
```


Required Parameters

* **bucket** - bucket to retrieve properties for

## Response


```bash
message RpbGetBucketResp {
    required RpbBucketProps props = 1;
}
// Bucket properties
message RpbBucketProps {
    optional uint32 n_val = 1;
    optional bool allow_mult = 2;
}
```


Values

* **n_val** - current n_val for the bucket
* **allow_mult** - set allow_mult to true if conflicts should be returned to
clients

## Example

Request

```bash
Hex      00 00 00 0B 13 0A 08 6D 79 62 75 63 6B 65 74
Erlang <<0,0,0,11,19,10,8,109,121,98,117,99,107,101,116>>

RpbGetBucketReq protoc decode:
bucket: "mybucket"

```


Response

```bash
Hex      00 00 00 07 14 0A 04 08 05 10 01
Erlang <<0,0,0,7,20,10,4,8,5,16,1>>

RpbGetBucketResp protoc decode:
props {
  n_val: 5
  allow_mult: true
}

```
