---
title: PBC Set Buckets Properties
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Bucket Operations"
---

Set the properties for a bucket

<div class="note"><p>The PBC interface does not currently support all bucket
properties. It is currently limited to <code>allow_mult</code> and
<code>n_val</code>; other bucket properties would need to be set with the [[HTTP
API|HTTP Set Bucket Properties]].</p>
</div>


## Request


```bash
message RpbSetBucketReq {
    required bytes bucket = 1;
    required RpbBucketProps props = 2;
}
// Bucket properties
message RpbBucketProps {
    optional uint32 n_val = 1;
    optional bool allow_mult = 2;
}
```


Required Parameters

* **bucket** - bucket to set properties for
* **props** - updated properties - only set properties to change
* **n_val** - current n_val for the bucket
* **allow_mult** - allow_mult set true if conflicts are returned to clients

## Response

Only the message code is returned.

## Example

Change `allow_mult` to true for bucket "friends"

Request

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


Response

```bash
Hex      00 00 00 01 16
Erlang <<0,0,0,1,22>>

RpbSetBucketResp - only message code defined
```