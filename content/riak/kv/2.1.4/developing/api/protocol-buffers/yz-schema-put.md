---
title: "PBC Yokozuna Schema Put"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Yokozuna Schema Put"
    identifier: "pbc_yz_schema_put"
    weight: 124
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.1.4/dev/references/protocol-buffers/yz-schema-put
  - /riak/kv/2.1.4/dev/references/protocol-buffers/yz-schema-put
---

Create a new Solr [search schema]({{<baseurl>}}riak/kv/2.1.4/developing/usage/search-schemas).

## Request

```protobuf
message RpbYokozunaSchemaPutReq {
    required RpbYokozunaSchema schema =  1;
}
```

Each message must contain a `RpbYokozunaSchema` object structure.

```protobuf
message RpbYokozunaSchema {
    required bytes name    =  1;
    optional bytes content =  2;
}
```

This message *must* include both the schema `name` and its Solr [search schema]({{<baseurl>}}riak/kv/2.1.4/developing/usage/search-schemas) `content` as XML.

## Response

Returns a [RpbPutResp]({{<baseurl>}}riak/kv/2.1.4/developing/api/protocol-buffers/#message-codes) code with no data on success.
