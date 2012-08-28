---
title: PBC API
project: riak
version: 1.2+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
index: true
---

This is an overview of the operations you can perform over the Protocol Buffers
Client (PBC) interface to Riak, and can be used as a guide for developing a
compliant client.

## Protocol

Riak listens on a TCP port (8087 by default) for incoming connections. Once
connected the client can send a stream of requests on the same connection.

Each operation consists of a request message and one or more response messages.
Messages are all encoded the same way
* 32-bit length of message code + protocol buffer message in network order
* 8-bit message code to identify the protocol buffer message
* N-bytes of protocol buffers encoded message

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

<table>
<tr><td>0</td><td>RpbErrorResp</td></tr>
<tr><td>1</td><td>RpbPingReq</td></tr>
<tr><td>2</td><td>RpbPingResp</td></tr>
<tr><td>3</td><td>RpbGetClientIdReq</td></tr>
<tr><td>4</td><td>RpbGetClientIdResp</td></tr>
<tr><td>5</td><td>RpbSetClientIdReq</td></tr>
<tr><td>6</td><td>RpbSetClientIdResp</td></tr>
<tr><td>7</td><td>RpbGetServerInfoReq</td></tr>
<tr><td>8</td><td>RpbGetServerInfoResp</td></tr>
<tr><td>9</td><td>RpbGetReq</td></tr>
<tr><td>10</td><td>RpbGetResp</td></tr>
<tr><td>11</td><td>RpbPutReq</td></tr>
<tr><td>12</td><td>RpbPutResp</td></tr>
<tr><td>13</td><td>RpbDelReq</td></tr>
<tr><td>14</td><td>RpbDelResp</td></tr>
<tr><td>15</td><td>RpbListBucketsReq</td></tr>
<tr><td>16</td><td>RpbListBucketsResp</td></tr>
<tr><td>17</td><td>RpbListKeysReq</td></tr>
<tr><td>18</td><td>RpbListKeysResp</td></tr>
<tr><td>19</td><td>RpbGetBucketReq</td></tr>
<tr><td>20</td><td>RpbGetBucketResp</td></tr>
<tr><td>21</td><td>RpbSetBucketReq</td></tr>
<tr><td>22</td><td>RpbSetBucketResp</td></tr>
<tr><td>23</td><td>RpbMapRedReq</td></tr>
<tr><td>24</td><td>RpbMapRedResp</td></tr>
<tr><td>25</td><td>RpbIndexReq <i>(new in 1.2+)</i></td></tr>
<tr><td>26</td><td>RpbIndexResp <i>(new in 1.2+)</i></td></tr>
<tr><td>27</td><td>RpbSearchQueryReq <i>(new in 1.2+)</i></td></tr>
<tr><td>28</td><td>RbpSearchQueryResp <i>(new in 1.2+)</i></td></tr>
</table>


<div class="info"><div class="title">Message Definitions</div>
<p>All Protocol Buffers messages can be found defined in the
[[riak.proto|https://github.com/basho/riak_pb/blob/master/src/riak.proto]] and other .proto files in the RiakPB project.</p>
</div>


### Error Response

If the server experiences an error processing a request it will return an
RpbErrorResp message instead of the response expected for the given request
(e.g. RbpGetResp is the expected response to RbpGetReq).  Error messages contain
an error string and an error code.

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
* [[PBC Index]]
* [[PBC Search]]

## Server Operations

* [[PBC Ping]]
* [[PBC Get Client ID]]
* [[PBC Set Client ID]]
* [[PBC Server Info]]
