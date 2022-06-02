---
title: "PBC Data Type Fetch"
description: ""
project: "riak_kv"
project_version: "2.0.1"
menu:
  riak_kv-2.0.1:
    name: "Data Type Fetch"
    identifier: "pbc_dt_fetch"
    weight: 114
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.0.1/dev/references/protocol-buffers/dt-fetch
  - /riak/kv/2.0.1/dev/references/protocol-buffers/dt-fetch
---

The equivalent of [`RpbGetReq`]({{<baseurl>}}riak/kv/2.0.1/developing/api/protocol-buffers/fetch-object) for [Riak Data Types]({{<baseurl>}}riak/kv/2.0.1/developing/data-types). This request results in a `DtFetchResp`
message (explained in the **Response** section below).

## Request

```protobuf
message DtFetchReq {
    required bytes bucket = 1;
    required bytes key    = 2;
    required bytes type  = 3;
    optional uint32 r             =  4;
    optional uint32 pr            =  5;
    optional bool   basic_quorum  =  6;
    optional bool   notfound_ok   =  7;
    optional uint32 timeout       =  8;
    optional bool   sloppy_quorum =  9;
    optional uint32 n_val         = 10;
    optional bool include_context = 11 [default=true];
}
```

#### Required Parameters

Parameter | Description
:---------|:-----------
`bucket` | The name of the bucket in which the Data Type is stored
`key` | The key where the Data Type is stored
`type` | The [Using Bucket Types]({{<baseurl>}}riak/kv/2.0.1/using/cluster-operations/bucket-types) of the bucket in which the Data Type is stored, _not_ the type of Data Type (i.e. counter, set, or map)

#### Optional Parameters

> **Note on defaults and special values**
>
> All of the optional parameters below have default values determined on a
per-bucket basis. Please refer to the documentation on [setting bucket properties]({{<baseurl>}}riak/kv/2.0.1/developing/api/protocol-buffers/set-bucket-props) for more information.

Furthermore, you can assign an integer value to the `r` and
`pr`, provided that that integer value is less than or equal
to N, _or_ a special value denoting `one`
(`4294967295-1`), `quorum`
(`4294967295-2`), `all`
(`4294967295-3`), or `default`
(`4294967295-4`).

Parameter | Description
:---------|:-----------
`r` | Read quorum, i.e. how many replicas need to agree when retrieving the object
`pr` | Primary read quorum, i.e. how many primary replicas need to be available when retrieving the object
`basic_quorum` | Whether to return early in some failure cases, e.g. when `r=1` and you get 2 errors and a success basic_quorum=true would return an error
`notfound_ok` | Whether to treat `not found` responses as successful reads for the purposes of R
`timeout` | The timeout duration, in milliseconds, after which Riak will return an error message
`sloppy_quorum` | If this parameter is set to `true`, the next available node in the ring will accept requests if any primary node is unavailable
`n_val` | The number of nodes to which the delete request will be sent
`include_context` | If this parameter is set to `true`, the Data Type's opaque "context" will be returned to the client

## Response

The response to a fetch request ([`DtFetchReq`]({{<baseurl>}}riak/kv/2.0.1/developing/api/protocol-buffers/dt-fetch)) is a `DtFetchResp` message.

```protobuf
message DtFetchResp {
    enum DataType {
        COUNTER = 1;
        SET     = 2;
        MAP     = 3;
    }

    optional bytes    context = 1;
    required DataType type    = 2;
    optional DtValue  value   = 3;
}
```

If the `include_context` option is specified, an opaque "context" value
will be returned along with the user-readable data. When sending an
update request, the client should send this context as well, just as one
would send a [vclock]({{<baseurl>}}riak/kv/2.0.1/learn/glossary/#vector-clock) for standard KV updates.

The type of the Data Type is specified in the `type` field, and must be
one of the three possible values of the `DataType` enum (`COUNTER`,
`SET`, or `MAP`).

The current value of the Data Type is contained in the `value` field,
which itself contains a `DtValue` message. This message will have the
following structure:

```protobuf
message DtValue {
    optional sint64   counter_value = 1;
    repeated bytes    set_value     = 2;
    repeated MapEntry map_value     = 3;
}
```

If the Data Type queried is a counter, it will return an integer value
for the counter; it a set, it will return the sets current value, in
bytes, if a map it will return a `MapEntry` message. `MapEntry` messages
are structured as follows:

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
