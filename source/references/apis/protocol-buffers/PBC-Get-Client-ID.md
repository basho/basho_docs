---
title: PBC Get Client I
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Server Operations"
---

Get the client id used for this connection.  Client ids are used for conflict
resolution and each unique actor in the system should be assigned one.  A client
id is assigned randomly when the socket is connected and can be changed using
[[Set Client ID|PBC Set Client ID]].

<div class="note"><div class="title">Client IDs in 1.0</div>
<p>All requests to Riak &lt;1.0 or Riak 1.0 without <code>vnode_vclocks</code>
enabled should set the Client ID, which can be any string that uniquely
identifies the client, for purposes of tracing object modifications in the
[[vector clock|Vector Clocks]].</p>
</div>

## Request

Just the RpbGetClientIdReq message code. No request message defined.

## Response


```bash
// Get ClientId Request - no message defined, just send RpbGetClientIdReq
message code
message RpbGetClientIdResp {
    required bytes client_id = 1; // Client id in use for this connection
}
```


## Example

Request

```bash
Hex     00 00 00 01 03
Erlang  <<0,0,0,1,3>>
```


Response

```bash
Hex     00 00 00 07 04 0A 04 01 65 01 B5
Erlang <<0,0,0,7,4,10,4,1,101,1,181>>

RpbGetClientIdResp protoc decode:
client_id: "001e001265"

```
