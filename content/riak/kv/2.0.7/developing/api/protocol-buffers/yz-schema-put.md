---
title: "PBC Yokozuna Schema Put"
description: ""
project: "riak_kv"
project_version: "2.0.7"
menu:
  riak_kv-2.0.7:
    name: "Yokozuna Schema Put"
    identifier: "pbc_yz_schema_put"
    weight: 124
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.0.7/dev/references/protocol-buffers/yz-schema-put
  - /riak/kv/2.0.7/dev/references/protocol-buffers/yz-schema-put
canonical_link: "https://docs.basho.com/riak/kv/latest/developing/api/protocol-buffers/yz-schema-put"
---

Create a new Solr [search schema](/riak/kv/2.0.7/developing/usage/search-schemas).

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

This message *must* include both the schema `name` and its Solr [search schema](/riak/kv/2.0.7/developing/usage/search-schemas) `content` as XML.
