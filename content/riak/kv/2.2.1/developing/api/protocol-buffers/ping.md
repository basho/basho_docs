---
title: "PBC Ping"
description: ""
project: "riak_kv"
project_version: "2.2.1"
lastmod: 2017-03-08T00:00:00-00:00
sitemap:
  priority: 0.1
menu:
  riak_kv-2.2.1:
    name: "Ping"
    identifier: "pbc_ping"
    weight: 110
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.2.1/dev/references/protocol-buffers/ping
  - /riak/kv/2.2.1/dev/references/protocol-buffers/ping
---

Check if the server is alive

## Request

Just the `RpbPingReq` message code. No request message defined.

## Response

Just the `RpbPingResp` message code. No response message defined.

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
