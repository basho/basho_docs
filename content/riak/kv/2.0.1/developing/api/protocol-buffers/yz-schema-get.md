---
title: "PBC Yokozuna Schema Get"
description: ""
project: "riak_kv"
project_version: "2.0.1"
menu:
  riak_kv-2.0.1:
    name: "Yokozuna Schema Get"
    identifier: "pbc_yz_schema_get"
    weight: 123
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.0.1/dev/references/protocol-buffers/yz-schema-get
  - /riak/kv/2.0.1/dev/references/protocol-buffers/yz-schema-get
---

Fetch a [search schema]({{<baseurl>}}riak/kv/2.0.1/developing/usage/search-schemas) from Riak Search.

## Request

In a request message, you only need to specify the name of the schema as
a binary (under `name`);

```protobuf
message RpbYokozunaSchemaGetReq {
    required bytes name  =  1;  // Schema name
}
```

## Response

```protobuf
message RpbYokozunaSchemaGetResp {
  required RpbYokozunaSchema schema =  1;
}
```

The response message will include a `RpbYokozunaSchema` structure.

```protobuf
message RpbYokozunaSchema {
    required bytes name    =  1;
    optional bytes content =  2;
}
```

This message includes the schema `name` and its xml `content`.
