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

This is an overview of the operations you can perform using the [Protocol Buffers](https://code.google.com/p/protobuf/) Client (PBC) interface to Riak, and can be used as a guide for developing a PBC-compliant Riak client.

## Protocol

Riak listens on a TCP port (8087 by default) for incoming connections. Once
connected, the client can send a stream of requests on the same connection.

Each operation consists of a [request message](https://developers.google.com/protocol-buffers/docs/encoding) and one or more response messages. Messages are all encoded the same way, consisting of:

* 32-bit length of message code + Protocol Buffers message in network order
* 8-bit message code to identify the Protocol Buffers message
* N bytes of Protocol Buffers-encoded message

### Example

```
00 00 00 07 09 0A 01 62 12 01 6B
|----Len---|MC|----Message-----|

Len = 0x07
Message Code (MC) = 0x09 = RpbGetReq
RpbGetReq Message = 0x0A 0x01 0x62 0x12 0x01 0x6B

Decoded Message:
bucket: "b"
key: "k"
```

## Message Codes

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
25 | `RpbIndexReq` |
26 | `RpbIndexResp` |
27 | `RpbSearchQueryReq` |
28 | `RbpSearchQueryResp` |
29 | `RpbAuthReq` |
30 | `RpbGetBucketTypeReq` |
31 | `RpbSetBucketTypeReq` |
32 | `RpbResetBucketReq` |

<div class="info">
<div class="title">Message Definitions</div>
All Protocol Buffers messages are defined in the <tt>riak.proto</tt> and other <tt>.proto</tt> files in the <tt>/src</tt> directory of the <a href="https://github.com/basho/riak_pb">RiakPB</a> project.
</div>

### Error Response

If the request does not result in an error, Riak will return one of a variety
of response messages, e.g. `RpbGetResp` or `RpbPutResp`, depending on which 
request message is sent.

If the server experiences an error processing a request, however, it will 
return an `RpbErrorResp` message instead of the response expected for the 
given request (e.g. `RbpGetResp` is the expected response to `RbpGetReq`).
Error messages contain an error string and an error code, like this:

```protobuf
message RpbErrorResp {
    required bytes errmsg = 1;
    required uint32 errcode = 2;
}
```

#### Values

* `errmsg` --- a string representation of what went wrong
* `errcode` --- a numeric code. Currently, only `RIAKC_ERR_GENERAL=1` is defined.

## Bucket Operations

* [[PBC List Buckets]]
* [[PBC List Keys]]
* [[PBC Get Bucket Properties]]
* [[PBC Set Bucket Properties]]
* [[PBC Reset Bucket Properties]]

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
* [[PBC Server Info]]

## Bucket Type Operations

* [[PBC Get Bucket Type]]
* [[PBC Set Bucket Type]]

## Data Type Operations

* [[PBC Data Type Fetch]]
* [[PBC Data Type Union]]
* [[PBC Data Type Store]]
* [[PBC Data Type Counter Store]]
* [[PBC Data Type Set Store]]
* [[PBC Data Type Map Store]]

## Yokozuna Operations

* [[PBC Yokozuna Index Get]]
* [[PBC Yokozuna Index Put]]
* [[PBC Yokozuna Index Delete]]
* [[PBC Yokozuna Schema Get]]
* [[PBC Yokozuna Schema Put]]
