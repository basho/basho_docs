---
title: PBC Server Info
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Server Operations"
moved: {
  '1.4.0-': '/references/apis/protocol-buffers/PBC-Server-Info/'
}
---

## Request

Just the RpbGetServerInfoReq message code. No request message defined.

## Response


```bash
message RpbGetServerInfoResp {
    optional bytes node = 1;
    optional bytes server_version = 2;
}
```

## Example

Request

```bash
Hex      00 00 00 01 07
Erlang <<0,0,0,1,7>>

RpbGetServerInfoReq - only message code defined
```


Response

```bash
Hex      00 00 00 17 08 0A 0E 72 69 61 6B 40 31 32 37 2E
         30 2E 30 2E 31 12 04 30 2E 31 30
Erlang <<0,0,0,23,8,10,14,114,105,97,107,64,49,50,55,46,48,46,48,46,49,18,4,48,
         46,49,48>>

RpbGetServerInfoResp protoc decode:
node: "riak@127.0.0.1"
server_version: "0.10"
```
