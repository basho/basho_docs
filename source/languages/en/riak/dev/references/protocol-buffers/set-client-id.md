---
title: PBC Set Client ID
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Server Operations"
moved: {
  '1.4.0-': '/references/apis/protocol-buffers/PBC-Set-Client-ID'
}
---

Set the client id for this connection. A library may want to set the client id
if it has a good way to uniquely identify actors across reconnects. This will
reduce vector clock bloat.

<div class="note"><div class="title">Client IDs in 1.0</div>
All requests to Riak &lt;1.0 or Riak 1.0 without <tt>vnode_vclocks</tt> enabled should set the Client ID, which can be any string that uniquely identifies the client, for purposes of tracing object modifications in the [[vector clock|Vector Clocks]].
</div>

## Request

```bash
message RpbSetClientIdReq {
    required bytes client_id = 1; // Client id to use for this connection
}
```


## Response

Just the RpbSetClientIdResp message code.

## Example

Request

```bash
Hex      00 00 00 07 05 0A 04 01 65 01 B6
Erlang <<0,0,0,7,5,10,4,1,101,1,182>>

RpbSetClientIdReq protoc decode:
client_id: "001e001266"

```

Response

```bash
Hex      00 00 00 01 06
Erlang <<0,0,0,1,6>>

RpbSetClientIdResp - only message code defined
```
