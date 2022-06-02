---
title: "PBC Delete Object"
description: ""
project: "riak_kv"
project_version: 2.9.7
menu:
  riak_kv-2.9.7:
    name: "Delete Object"
    identifier: "pbc_delete_object"
    weight: 107
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.9.7/dev/references/protocol-buffers/delete-object
  - /riak/kv/2.9.7/dev/references/protocol-buffers/delete-object
---

Delete an object in the specified [bucket type]({{<baseurl>}}riak/kv/2.9.7/using/cluster-operations/bucket-types)/bucket/key location.

## Request

```protobuf
message RpbDelReq {
    required bytes bucket = 1;
    required bytes key = 2;
    optional uint32 rw = 3;
    optional bytes vclock = 4;
    optional uint32 r = 5;
    optional uint32 w = 6;
    optional uint32 pr = 7;
    optional uint32 pw = 8;
    optional uint32 dw = 9;
    optional uint32 timeout = 10;
    optional bool sloppy_quorum = 11;
    optional uint32 n_val = 12;
    optional bytes type = 13;
}
```

#### Required Parameters

Parameter | Description |
:---------|:------------|
`bucket` | The name of the bucket in which the object is stored
`key` | The key under which the object is stored

#### Optional Parameters

{{% note title="Note on defaults and special values" %}}
All of the optional parameters below have default values determined on a
per-bucket basis. Please refer to the documentation on [setting bucket properties](../set-bucket-props) for more information.

Furthermore, you can assign an integer value to the `w`, `dw`, `pr`, and
`pw`, provided that that integer value is less than or equal to N, _or_
a special value denoting `one` (`4294967295-1`), `quorum`
(`4294967295-2`), `all` (`4294967295-3`), or `default` (`4294967295-4`).
{{% /note %}}

Parameter | Description |
:---------|:------------|
`rw` | How many replicas to delete before returning a successful response
`r` | Read quorum, i.e. how many replicas need to agree when retrieving the object
`w` | Write quorum, i.e. how many replicas to write to before returning a successful response
`pr` | Primary read quorum, i.e. how many primary replicas need to be available when retrieving the object
`pw` | Primary write quorum, i.e. how many primary nodes must be up when the write is attempted
`dw` | Durable write quorum, i.e. how many replicas to commit to durable storage before returning a successful response
`timeout` | The timeout duration, in milliseconds, after which Riak will return an error message
`vclock` | Opaque vector clock provided by an earlier `RpbGetResp` message Used to prevent deleting of objects that have been modified since the last GET request (sent as a byte array)
`sloppy_quorum` | If this parameter is set to `true`, the next available node in the ring will accept requests if any primary node is unavailable
`n_val` | The number of nodes to which the delete request will be sent
`type` | The bucket types associated with the object. If the bucket type is not specified, the `default` bucket type will be used, as is the case for all messages sent to Riak that have the bucket type as an optional parameter.

## Response

Only the message code is returned.

## Example

#### Request

```
Hex      00 00 00 12 0D 0A 0A 6E 6F 74 61 62 75 63 6B 65
         74 12 01 6B 18 01
Erlang <<0,0,0,18,13,10,10,110,111,116,97,98,117,99,107,101,116,18,1,107,24,1>>

RpbDelReq protoc decode:
bucket: "notabucket"
key: "k"
rw: 1

```

#### Response

```
Hex      00 00 00 01 0E
Erlang <<0,0,0,1,14>>

RpbDelResp - only message code defined
```




