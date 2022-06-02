---
title: "PBC Store Object"
description: ""
project: "riak_kv"
project_version: "2.0.5"
menu:
  riak_kv-2.0.5:
    name: "Store Object"
    identifier: "pbc_store_object"
    weight: 106
    parent: "apis_pbc"
toc: true
aliases:
  - /riak/2.0.5/dev/references/protocol-buffers/store-object
  - /riak/kv/2.0.5/dev/references/protocol-buffers/store-object
---

Stores an object under the specified location, as determined by the
intended [key]({{<baseurl>}}riak/kv/2.0.5/learn/concepts/keys-and-objects), [bucket]({{<baseurl>}}riak/kv/2.0.5/learn/concepts/buckets), and [bucket type]({{<baseurl>}}riak/kv/2.0.5/developing/usage/bucket-types). A bucket must always be specified (via
`bucket`), whereas key (`key`) and bucket type (`type`) are optional. If
no key is specified, Riak will assign a random key to the object. If no
[bucket type]({{<baseurl>}}riak/kv/2.0.5/developing/usage/bucket-types) is assigned, Riak will assign
`default`, which means that the [default bucket configuration]({{<baseurl>}}riak/kv/2.0.5/configuring/reference/#default-bucket-properties) will be used.

#### Request

```protobuf
message RpbPutReq {
    required bytes bucket = 1;
    optional bytes key = 2;
    optional bytes vclock = 3;
    required RpbContent content = 4;
    optional uint32 w = 5;
    optional uint32 dw = 6;
    optional bool return_body = 7;
    optional uint32 pw = 8;
    optional bool if_not_modified = 9;
    optional bool if_none_match = 10;
    optional bool return_head = 11;
    optional uint32 timeout = 12;
    optional bool asis = 13;
    optional bool sloppy_quorum = 14;
    optional uint32 n_val = 15;
    optional bytes type = 16;
}
```

#### Required Parameters

Parameter | Description
:---------|:-----------
`bucket` | The name of the bucket, in bytes, in which the key/value is to reside
`content` | The new or updated contented of the object. Uses the same `RpbContent` message returned as part of an `RpbGetResp` message, documented in [PBC Fetch Object]({{<baseurl>}}riak/kv/2.0.5/developing/api/protocol-buffers/fetch-object)

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
`key` | The key to create/update. If not specified, Riak will generate a random key and return that key as part of the response to that request. 
`vclock` | Opaque vector clock provided by an earlier <code><a href="../../../../learn/concepts/causal-context">RpbGetResp</a></code> message. Omit if this is a new key or if you deliberately want to create a sibling.
`w` | Write quorum, i.e. how many replicas to write to before returning a successful response
`dw` | Durable write quorum, i.e. how many replicas to commit to durable storage before returning a successful response
`return_body` | Whether to return the contents of the now-stored object. Defaults to `false`.
`pw` | Primary write quorum, i.e. how many primary nodes must be up when the write is attempted
`return_head` | Return the metadata for the now-stored object without returning the value of the object
`timeout` | The timeout duration, in milliseconds, after which Riak will return an error message
`sloppy_quorum` | If this parameter is set to `true`, the next available node in the ring will accept requests if any primary node is unavailable
`n_val` | The number of nodes on which the value is to be stored

The `if_not_modified`, `if_none_match`, and `asis` parameters are set
only for messages sent between nodes in a Riak cluster and should not be
set by Riak clients.

#### Response

```bash
message RpbPutResp {
    repeated RpbContent contents = 1;
    optional bytes vclock = 2;
    optional bytes key = 3;
}
```

If `return_body` is set to `true` on the PUT request, the `RpbPutResp`
will contain the current object after the PUT completes, in `contents`,
as well as the object's [causal context]({{<baseurl>}}riak/kv/2.0.5/learn/concepts/causal-context), in the `vclock`
field. The `key` will be sent only if the server generated a random key
for the object.

If `return_body` is not set and no key is generated, the PUT response
will be empty.

## Example

#### Request

```
Hex      00 00 00 1C 0B 0A 01 62 12 01 6B 22 0F 0A 0D 7B
         22 66 6F 6F 22 3A 22 62 61 72 22 7D 28 02 38 01
Erlang <<0,0,0,28,11,10,1,98,18,1,107,34,15,10,13,123,34,102,111,111,34,58,34,
         98,97,114,34,125,40,2,56,1>>

RpbPutReq protoc decode:
bucket: "b"
key: "k"
content {
  value: "{"foo":"bar"}"
}
w: 2
return_body: true

```

#### Response

```
Hex      00 00 00 62 0C 0A 31 0A 0D 7B 22 66 6F 6F 22 3A
         22 62 61 72 22 7D 2A 16 31 63 61 79 6B 4F 44 39
         36 69 4E 41 68 6F 6D 79 65 56 6A 4F 59 43 38 AF
         B0 A3 DE 04 40 90 E7 18 12 2C 6B CE 61 60 60 60
         CA 60 CA 05 52 2C 2C E9 0C 86 19 4C 89 8C 79 AC
         0C 5A 21 B6 47 F9 20 C2 6C CD 49 AC 0D 77 7C A0
         12 FA 20 89 2C 00
Erlang <<0,0,0,98,12,10,49,10,13,123,34,102,111,111,34,58,34,98,97,114,34,125,
         42,22,49,99,97,121,107,79,68,57,54,105,78,65,104,111,109,121,101,86,
         106,79,89,67,56,175,176,163,222,4,64,144,231,24,18,44,107,206,97,96,
         96,96,202,96,202,5,82,44,44,233,12,134,25,76,137,140,121,172,12,90,33,
         182,71,249,32,194,108,205,73,172,13,119,124,160,18,250,32,137,44,0>>

RpbPutResp protoc decode:
contents {
  value: "{"foo":"bar"}"
  vtag: "1caykOD96iNAhomyeVjOYC"
  last_mod: 1271453743
  last_mod_usecs: 406416
}
vclock: "k316a```312`312005R,,351014206031L211214y254014Z!266G371
302l315I254rw|240022372 211,000"

```
