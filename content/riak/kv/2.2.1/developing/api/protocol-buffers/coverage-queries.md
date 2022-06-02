---
title: "PBC Coverage Queries"
description: ""
project: "riak_kv"
project_version: "2.2.1"
menu:
  riak_kv-2.2.1:
    name: "Coverage Queries"
    identifier: "pbc_coverage_queries"
    weight: 108
    parent: "apis_pbc"
version_history:
  in: "2.1.4+"
toc: true
aliases:
  - /riak/2.2.1/dev/references/protocol-buffers/coverage-queries
  - /riak/kv/2.2.1/dev/references/protocol-buffers/coverage-queries
---

Prepare for parallelizable
[secondary index queries](../secondary-indexes/) by requesting a
coverage plan. The response will be multiple slices of the cluster, as
identified by a TCP endpoint and an opaque binary to be included with
each 2i query.

## Request

```protobuf
message RpbCoverageReq {
    optional bytes type = 1;
    required bytes bucket = 2;
    optional uint32 min_partitions = 3;
    optional bytes replace_cover = 4;
    repeated bytes unavailable_cover = 5;
}
```

#### Required Parameters

Parameter | Description
:---------|:-----------
`bucket` | The name of the bucket in which the data is stored

#### Optional Parameters

Parameter | Description
:---------|:-----------
`type` | The name of the bucket type, if this bucket is not in the default (pre-2.0) bucket type.
`min_partitions` | The minimum number of cluster slices. `undefined` results in a direct map of the internal coverage plan, which targets the minimum number of nodes necessary to retrieve all data. An integer will be rounded up to the nearest power of 2 greater than or equal to the ring size.
`replace_cover` | If a client cannot reach the server designated by a previous coverage response, the opaque binary can be sent with a new coverage request via this parameter and a new plan component will be calculated and returned.
`unavailable_cover` | List of opaque binaries representing other unreachable endpoints to help Riak determine what servers the client cannot currently use.

## Response

The results of a coverage query are returned as a list of endpoints
with opaque binaries to be included with secondary index queries.

```protobuf
message RpbCoverageResp {
   repeated RpbCoverageEntry entries = 1;
}

message RpbCoverageEntry {
    required bytes ip = 1;
    required uint32 port = 2;
    optional bytes keyspace_desc = 3;
    required bytes cover_context = 4;
}
```

#### Values

Parameter | Description
:---------|:-----------
`ip` | The IP address of the server containing a subset of the data. Depending on the environment, this address may require local translation to deal with routing or firewall constraints.
`port` | The port to contact on the server.
`keyspace_desc` | A human-readable description of the keyspace. Not intended to be used programmatically except potentially for logging.
`cover_context` | The opaque binary to be used in secondary index queries (and possibly future coverage queries to indicate that this server appears offline or otherwise non-functional to the client).
