---
title: PBC API
project: riak
version: 1.0.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
index: true
moved: {
  '1.4.0-': '/references/apis/protocol-buffers'
}
---

This is an overview of the operations you can perform over the Protocol Buffers
Client (PBC) interface to Riak, and can be used as a guide for developing a
compliant client.

## Protocol

Riak listens on a TCP port (8087 by default) for incoming connections. Once
connected the client can send a stream of requests on the same connection.

Each operation consists of a request message and one or more response messages.
Messages are all encoded the same way
* 32-bit length of message code + Protocol Buffers message in network order
* 8-bit message code to identify the Protocol Buffers message
* N-bytes of Protocol Buffers-encoded message

### Example


```bash
00 00 00 07 09 0A 01 62 12 01 6B
|----Len---|MC|----Message-----|

Len = 0x07
Message Code (MC) = 0x09 = RpbGetReq
RpbGetReq Message = 0x0A 0x01 0x62 0x12 0x01 0x6B

Decoded Message:
bucket: "b"
key: "k"
```


### Message Codes

Code | Message |
:----|:--------|
0 | `RpbErrorResp` |
1 | `RpbPingReq` |
2 | `RpbPingResp` |
3 | `RpbGetClientIdReq` |
4 | `RpbGetClientIdResp` |
5 | `RpbSetClientIdReq` |
6 | `RpbSetClientIdResp` |
7 | `RpbGetServerInfoReq` |
8 | `RpbGetServerInfoResp` |
9 | `RpbGetReq` |
10 | `RpbGetResp` |
11 | `RpbPutReq` |
12 | `RpbPutResp` |
13 | `RpbDelReq` |
14 | `RpbDelResp` |
15 | `RpbListBucketsReq` |
16 | `RpbListBucketsResp` |
17 | `RpbListKeysReq` |
18 | `RpbListKeysResp` |
19 | `RpbGetBucketReq` |
20 | `RpbGetBucketResp` |
21 | `RpbSetBucketReq` |
22 | `RpbSetBucketResp` |
23 | `RpbMapRedReq` |
24 | `RpbMapRedResp` |
25 | `RpbIndexReq` _(new in 1.2+)_ |
26 | `RpbIndexResp` _(new in 1.2+)_ |
27 | `RpbSearchQueryReq` _(new in 1.2+)_ |
28 | `RbpSearchQueryResp` _(new in 1.2+)_ |

<div class="info">
<div class="title">Message Definitions</div>
<p>All Protocol Buffers messages can be found defined in the [[riak.proto|https://github.com/basho/riak_pb/blob/master/src/riak.proto]] and other .proto files in the RiakPB project.</p>
</div>

### Error Response

If the server experiences an error processing a request it will return an
RpbErrorResp message instead of the response expected for the given request
(e.g. RbpGetResp is the expected response to RbpGetReq).  Error messages contain an error string and an error code.

```bash
message RpbErrorResp {
    required bytes errmsg = 1;
    required uint32 errcode = 2;
}
```

Values:

* **errmsg** - a string representation of what went wrong
* **errcode** - a numeric code. Currently only RIAKC_ERR_GENERAL=1 is defined.

## Bucket Operations

* [[PBC List Buckets]]
* [[PBC List Keys]]
* [[PBC Get Bucket Properties]]
* [[PBC Set Bucket Properties]]

## Object/Key Operations

* [[PBC Fetch Object]]
* [[PBC Store Object]]
* [[PBC Delete Object]]

## Query Operations

* [[PBC MapReduce]]
* [[PBC Secondary Indexes]]
* [[PBC Search]]

## Server Operations

* [[PBC Ping]]
* [[PBC Get Client ID]]
* [[PBC Set Client ID]]
* [[PBC Server Info]]