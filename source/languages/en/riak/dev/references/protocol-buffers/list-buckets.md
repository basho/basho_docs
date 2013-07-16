---
title: PBC List Buckets
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Bucket Operations"
moved: {
  '1.4.0-': '/references/apis/protocol-buffers/PBC-List-Buckets'
}
---

List all of the bucket names available

<div class="note">
<div class="title">Caution</div>

This call can be expensive for the server - do not use in performance sensitive code

</div>


## Request

Only the message code is required.

## Response


```bash
message RpbListBucketsResp {
    repeated bytes buckets = 1;
}
```


Values

* **buckets** - buckets on the server

## Example

Request

```bash
Hex      00 00 00 01 0F
Erlang <<0,0,0,1,15>>

RpbListBucketsReq - only message code defined
```


Response

```bash
Hex      00 00 00 2A 10 0A 02 62 31 0A 02 62 35 0A 02 62
         34 0A 02 62 38 0A 02 62 33 0A 03 62 31 30 0A 02
         62 39 0A 02 62 32 0A 02 62 36 0A 02 62 37
Erlang <<0,0,0,42,16,10,2,98,49,10,2,98,53,10,2,98,52,10,2,98,56,10,2,98,51,10,
         3,98,49,48,10,2,98,57,10,2,98,50,10,2,98,54,10,2,98,55>>

RpbListBucketsResp protoc decode:
buckets: "b1"
buckets: "b5"
buckets: "b4"
buckets: "b8"
buckets: "b3"
buckets: "b10"
buckets: "b9"
buckets: "b2"
buckets: "b6"
buckets: "b7"

```
