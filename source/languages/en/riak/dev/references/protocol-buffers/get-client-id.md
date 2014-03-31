---
title: PBC Get Client ID
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Server Operations"
moved: {
  '1.4.0-': '/references/apis/protocol-buffers/PBC-Get-Client-ID'
}
---

Get the client id used for this connection. Client ids are used for conflict resolution and each unique actor in the system should be assigned one. A client id is assigned randomly when the socket is connected and can be changed using [[Set Client ID|PBC Set Client ID]].

<div class="note"><div class="title">Client IDs in 1.0</div>
All requests to Riak &lt;1.0 or Riak 1.0 without <code>vnode_vclocks</code>
enabled should set the Client ID, which can be any string that uniquely
identifies the client, for purposes of tracing object modifications in the
[[vector clock|Vector Clocks]].
</div>

## Request

Just the `RpbGetClientIdReq` message code. No request message defined.

## Response

```protobuf
// Get ClientId Request - no message defined, just send RpbGetClientIdReq
message code
message RpbGetClientIdResp {
    required bytes client_id = 1; // Client id in use for this connection
}
```
