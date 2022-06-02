---
title: "PBC Get Client ID"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Get Client ID"
    identifier: "pbc_get_client_id"
    weight: 127
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.1.4/dev/references/protocol-buffers/get-client-id
  - /riak/kv/2.1.4/dev/references/protocol-buffers/get-client-id
---

{{% note title="Deprecation notice" %}}
The use of client IDs in conflict resolution is now deprecated in Riak. If you
are building or maintaining a Riak client that is intended to be compatible
with Riak 1.4 or later, you can safely ignore client IDs.
{{% /note %}}

Get the client id used for this connection. Client ids are used for
conflict resolution and each unique actor in the system should be
assigned one.  A client id is assigned randomly when the socket is
connected and can be changed using [Set Client ID]({{<baseurl>}}riak/kv/2.1.4/developing/api/protocol-buffers/set-client-id).

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

## Example

Request

```
Hex     00 00 00 01 03
Erlang  <<0,0,0,1,3>>
```


Response

```
Hex     00 00 00 07 04 0A 04 01 65 01 B5
Erlang <<0,0,0,7,4,10,4,1,101,1,181>>

RpbGetClientIdResp protoc decode:
client_id: "001e001265"
```
