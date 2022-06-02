---
title: "PBC Data Type Store"
description: ""
project: "riak_kv"
project_version: 2.9.7
menu:
  riak_kv-2.9.7:
    name: "Data Type Store"
    identifier: "pbc_dt_store"
    weight: 116
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.9.7/dev/references/protocol-buffers/dt-store
  - /riak/kv/2.9.7/dev/references/protocol-buffers/dt-store
---

A request to update the value of a [Riak Data Type]({{<baseurl>}}riak/kv/2.9.7/developing/data-types).

## Request

A `DtUpdateReq` message requires that you specify the location of the
Data Type in Riak, which operations are to be performed, and whether the
Data Type's opaque context should be returned in the resulting
`DtUpdateResp`.

The `DtOp` value specifies which Data Type-specific operation is being
performed. More on that in the [PBC Data Type Union]({{<baseurl>}}riak/kv/2.9.7/developing/api/protocol-buffers/dt-union) document.

```protobuf
message DtUpdateReq {
    required bytes bucket = 1;
    optional bytes key    = 2;
    required bytes type   = 3;
    optional bytes context = 4;
    required DtOp  op = 5;
    optional uint32 w               =  6;
    optional uint32 dw              =  7;
    optional uint32 pw              =  8;
    optional bool   return_body     =  9 [default=false];
    optional uint32 timeout         = 10;
    optional bool   sloppy_quorum   = 11;
    optional uint32 n_val           = 12;
    optional bool   include_context = 13 [default=true];
}
```

#### Required Parameters

Parameter | Description
:---------|:-----------
`bucket` | The name of the bucket in which the Data Type is stored
`type` | The bucket type of the bucket in which the Data Type is stored, _not_ the type of Data Type (i.e. counter, set, or map). Learn more about [using bucket types]({{<baseurl>}}riak/kv/2.9.7/using/cluster-operations/bucket-types).

Also required is a `DtOp` message that specifies which operation is to
be performed, depending on whether the Data Type being updated is a
[counter]({{<baseurl>}}riak/kv/2.9.7/developing/api/protocol-buffers/dt-counter-store), [set]({{<baseurl>}}riak/kv/2.9.7/developing/api/protocol-buffers/dt-set-store), or [map]({{<baseurl>}}riak/kv/2.9.7/developing/api/protocol-buffers/dt-map-store).

```protobuf
message DtOp {
    optional CounterOp counter_op = 1;
    optional SetOp     set_op     = 2;
    optional MapOp     map_op     = 3;
}
```

#### Optional Parameters

{{% note title="Note on defaults and special values" %}}
All of the optional parameters below have default values determined on a
per-bucket basis. Please refer to the documentation on [setting bucket properties](../set-bucket-props) for more information.

Furthermore, you can assign an integer value to the `w`, `dw`, `pr`, and
`pw`, provided that that integer value is less than or equal to N, _or_
a special value denoting `one` (`4294967295-1`), `quorum`
(`4294967295-2`), `all` (`4294967295-3`), or `default` (`4294967295-4`).
{{% /note %}}

Parameter | Description
:---------|:-----------
`key` | The key where the Data Type is stored. If not specified, Riak will assign a random key and return that key to the client if `return_body` is set to `true`.
`context` | The opaque binary "context" that informs Riak which version of a data type the client has seen, analogous to [vector clocks]({{<baseurl>}}riak/kv/2.9.7/learn/glossary/#vector-clock)
`w` | Write quorum, i.e. how many replicas to write to before returning a successful response
`dw` | Durable write quorum, i.e. how many replicas to commit to durable storage before returning a successful response
`pw` | Primary write quorum, i.e. how many primary nodes must be up when the write is attempted
`return_body` | Whether to return the contents of the stored object. Defaults to `false`.
`timeout` | The timeout duration, in milliseconds, after which Riak will return an error message
`sloppy_quorum` | If this parameter is set to `true`, the next available node in the ring will accept requests if any primary node is unavailable
`n_val` | The number of nodes on which the value is to be stored
`include_context` | If `return_body` is set to `true`, the Data Type's opaque "context" will be returned to the client when the `DtUpdateResp` is sent to the client.

## Response

The response to a Data Type update request is analogous to
[`RpbPutResp`]({{<baseurl>}}riak/kv/2.9.7/developing/api/protocol-buffers/store-object) for KV operations. If the
`return_body` is set in the update request message (as explained above),
the message will include the opaque context of the Data Type (`context`)
and the new value of the Data Type _after_ the update has completed
(depending on whether the Data Type is a counter, set, or map). If no
key was specified in the update request, it will include the
Riak-assigned key (`key`).

```protobuf
message DtUpdateResp {
    optional bytes    key           = 1;
    optional bytes    context       = 2;
    optional sint64   counter_value = 3;
    repeated bytes    set_value     = 4;
    repeated MapEntry map_value     = 5;
}
```

Assuming `return_body` is set to `true`: if a counter is updated, the
response will include an integer as the `counter_value`; if a set is
updated, a list of binaries will be return as the `set_value`; and if a
map is updated, the returned `map_value` will be a `MapEntry` message.
That message takes the following form:

```protobuf
message MapEntry {
    required MapField field = 1;
    optional sint64   counter_value  = 2;
    repeated bytes    set_value      = 3;
    optional bytes    register_value = 4;
    optional bool     flag_value     = 5;
    repeated MapEntry map_value      = 6;
}
```




