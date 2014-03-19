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

This is an overview of the operations you can perform over the [Protocol Buffers](https://code.google.com/p/protobuf/) Client (PBC) interface to Riak, and can be used as a guide for developing a PBC-compliant Riak client.

## Protocol

Riak listens on a TCP port (8087 by default) for incoming connections. Once
connected, the client can send a stream of requests on the same connection.

Each operation consists of a [request message](https://developers.google.com/protocol-buffers/docs/encoding) and one or more response messages. Messages are all encoded the same way:

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
25 | `RpbIndexReq` {{1.2.0+}} |
26 | `RpbIndexResp` {{1.2.0+}} |
27 | `RpbSearchQueryReq` {{1.2.0+}} |
28 | `RbpSearchQueryResp` {{1.2.0+}} |
29 | `RpbAuthReq` {{2.0.0+}} |
30 | `RpbGetBucketTypeReq` {{2.0.0+}} |
31 | `RpbSetBucketTypeReq` {{2.0.0+}} |
32 | `RpbResetBucketReq` {{2.0.0+}} |

<div class="info">
<div class="title">Message Definitions</div>
All Protocol Buffers messages are defined in the <tt>riak.proto</tt> and other <tt>.proto</tt> files in the <tt>/src</tt> directory of the <a href="https://github.com/basho/riak_pb">RiakPB</a> project.
</div>

### Error Response

If the server experiences an error processing a request, it will return an
`RpbErrorResp` message instead of the response expected for the given request
(e.g. `RbpGetResp` is the expected response to `RbpGetReq`).  Error messages contain an error string and an error code, like this:

```protobuf
message RpbErrorResp {
    required bytes errmsg = 1;
    required uint32 errcode = 2;
}
```

Values:

* `errmsg` --- a string representation of what went wrong
* `errcode` --- a numeric code. Currently only `RIAKC_ERR_GENERAL=1` is defined.

## Protocol Buffers Security



![Protocol Buffers security diagram](http://hijacked.us/~andrew/protobuffs_security4.png)

## Bucket Operations

* [[PBC List Buckets]]
* [[PBC List Keys]]
* [[PBC Get Bucket Properties]]
* [[PBC Set Bucket Properties]]
* [[PBC Reset Bucket Properties]] {{#2.0.0+}}

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

{{#2.0.0+}}
## Bucket Type Operations

* [[PBC Get Bucket Type]]
* [[PBC Set Bucket Type]]

## Data Type Operations

* [[PBC Datatype Fetch]]
* [[PBC Datatype Union]]
* [[PBC Datatype Store]]
* [[PBC Datatype Counter Store]]
* [[PBC Datatype Set Store]]
* [[PBC Datatype Map Store]]

## Yokozuna Operations

* [[PBC Yokozuna Index Query]]
* [[PBC Yokozuna Index Get]]
* [[PBC Yokozuna Index Put]]
* [[PBC Yokozuna Index Delete]]
* [[PBC Yokozuna Schema Query]]
* [[PBC Yokozuna Schema Get]]
* [[PBC Yokozuna Schema Put]]
{{/2.0.0+}}
