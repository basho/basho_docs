---
title: "PBC Delete Object"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Delete Object"
    identifier: "pbc_delete_object"
    weight: 107
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.1.4/dev/references/protocol-buffers/delete-object
  - /riak/kv/2.1.4/dev/references/protocol-buffers/delete-object
canonical_link: "https://docs.basho.com/riak/kv/latest/developing/api/protocol-buffers/delete-object"
---

Delete an object in the specified [bucket type](/riak/kv/2.1.4/using/cluster-operations/bucket-types)/bucket/key location.

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

<div class="note">
<div class="title">Note on defaults and special values</div>
All of the optional parameters below have default values determined on a
per-bucket basis. Please refer to the documentation on <a
href="http://docs.basho.com/riak/kv/2.1.4/developing/api/protocol-buffers/set-bucket-props/">setting bucket
properties</a> for more information.

Furthermore, you can assign an integer value to the <code>rw</code>,
<code>r</code>, <code>w</code>, <code>pr</code>, <code>pw</code>, and
<code>dw</code>, provided that that integer value is less than or equal
to N, <em>or</em> a special value denoting <code>one</code>
(<code>4294967295-1</code>), <code>quorum</code>
(<code>4294967295-2</code>), <code>all</code>
(<code>4294967295-3</code>), or <code>default</code>
(<code>4294967295-4</code>).
</div>

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
