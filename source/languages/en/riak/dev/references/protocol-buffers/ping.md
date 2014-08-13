---
title: PBC Ping
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Server Operations"
moved: {
  '1.4.0-': '/references/apis/protocol-buffers/PBC-Ping'
}
---

Check if the server is alive

## Request

Just the RpbPingReq message code. No request message defined.

## Response

Just the RpbPingResp message code. No response message defined.

## Example

Request

```bash
Hex    00 00 00 01 01
Erlang <<0,0,0,1,1>>
```

Response

```bash
Hex    00 00 00 01 02
Erlang <<0,0,0,1,2>>
```
