---
title: PBC Fetch Object
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Object/Key Operations"
moved: {
  '1.4.0-': '/references/apis/protocol-buffers/PBC-Fetch-Object'
}
---

Fetch an object from the specified bucket type/bucket/key location (specified by `bucket`, `type`, and `key`, respectively). If the bucket type is not specified, the `default` bucket type will be used, as is the case for all messages sent to Riak that have the bucket type as an optional parameter.

## Request

```protobuf
message RpbGetReq {
    required bytes bucket = 1;
    required bytes key = 2;
    optional uint32 r = 3;
    optional uint32 pr = 4;
    optional bool basic_quorum = 5;
    optional bool notfound_ok = 6;
    optional bytes if_modified = 7;
    optional bool head = 8;
    optional bool deletedvclock = 9;
    optional uint32 timeout = 10;
    optional bool sloppy_quorum = 11;
    optional uint32 n_val = 12;
    optional bytes type = 13;
}
```


## Optional Parameters

<div class="note">
<div class="title">Note on defaults and special values</div>
All of the optional parameters below have default values determined on a
per-bucket basis. Please refer to the documentation on <a href="/dev/references/protocol-buffers/set-bucket-props">setting bucket properties</a> for more information.

Furthermore, you can assign an integer value to the <tt>r</tt> and <tt>pr</tt> parameters, provided that that integer value is less than or equal to N, <em>or</em> a special value denoting <tt>one</tt> (<tt>4294967295-1</tt>), <tt>quorum</tt> (<tt>4294967295-2</tt>), <tt>all</tt> (<tt>4294967295-3</tt>), or <tt>default</tt> (<tt>4294967295-4</tt>).
</div>

Parameter | Description |
:---------|:------------|
`basic_quorum` | Whether to return early in some failure cases, e.g. when `r=1` and you get 2 errors and a success basic_quorum=true would return an error
`notfound_ok` | Whether to treat `not found` responses as successful reads for the purposes of R
`if_modified` | When a vclock is supplied as this option, the response will only return the object if the vclocks don't match
`head` | If set to `true`, Riak will return the object with the value(s) set as empty, which allows you to get the metadata without a potentially large value accompanying it
`deletedvclock` | If set to `true`, Riak will return the tombstone's vclock, if applicable
`timeout` | The timeout duration, in milliseconds, after which Riak will return an error message
`sloppy_quorum` | If this parameter is set to `true`, the next available node in the ring will accept requests if any primary node is unavailable

## Response

```protobuf
message RpbGetResp {
    repeated RpbContent content = 1;
    optional bytes vclock = 2;
    optional bool unchanged = 3;
}
```

#### Values

Value | Description
:-----|:-----------
`content` | The value plus metadata entries for the object. If there are siblings, there will be more than one entry. If the key is not found, the content will be empty.
`vclock` | The opaque vector clock that must be included in the `RpbPutReq`
to resolve the siblings
`unchanged` | If `if_modified` was specified in the GET request but the object
has not been modified, this will be set to `true`

The <tt>content</tt> entries hold the object value and any metadata. Below is the structure of a <tt>RpbContent</tt> message, which is included in GET/PUT responses (`RpbGetResp` (above) and `[[RpbPutResp|PBC Store Object]]`, respectively):

```protobuf
// Content message included in get/put responses
message RpbContent {
    required bytes value = 1;
    optional bytes content_type = 2;     // the media type/format
    optional bytes charset = 3;
    optional bytes content_encoding = 4;
    optional bytes vtag = 5;
    repeated RpbLink links = 6;          // links to other resources
    optional uint32 last_mod = 7;
    optional uint32 last_mod_usecs = 8;
    repeated RpbPair usermeta = 9;       // user metadata stored with the object
    repeated RpbPair indexes = 10;
    optional bool deleted = 11;
}
```


Each object can contain user-supplied metadata (`X-Riak-Meta-` in the HTTP
interface) consisting of a key/value pair. (e.g. `key=X-Riak-Meta-ACLvalue=users:r,administrators:f` would allow an application to store access
control information for it to enforce (*not* Riak)).

```protobuf
// Key/value pair - used for user metadata
message RpbPair {
    required bytes key = 1;
    optional bytes value = 2;
}
```


<div class="note">
<div class="title">Note on missing keys</div>
Remember: if a key is not stored in Riak, an <tt>RpbGetResp</tt> response
without the <tt>content</tt> and <tt>vclock</tt> fields will be returned. This should be mapped to whatever convention the client language uses to return not found. The Erlang client, for example, returns the atom <tt>{error, notfound}</tt>.
</div>

## Example

#### Request

```
Hex      00 00 00 07 09 0A 01 62 12 01 6B
Erlang <<0,0,0,7,9,10,1,98,18,1,107>>

RpbGetReq protoc decode:
bucket: "b"
key: "k"
```

#### Response

```
Hex      00 00 00 4A 0A 0A 26 0A 02 76 32 2A 16 33 53 44
         6C 66 34 49 4E 4B 7A 38 68 4E 64 68 79 49 6D 4B
         49 72 75 38 BB D7 A2 DE 04 40 E0 B9 06 12 1F 6B
         CE 61 60 60 60 CC 60 CA 05 52 2C AC C2 5B 3F 65
         30 25 32 E5 B1 32 EC 56 B7 3D CA 97 05 00
Erlang <<0,0,0,74,10,10,38,10,2,118,50,42,22,51,83,68,108,102,52,73,78,75,122,
         56,104,78,100,104,121,73,109,75,73,114,117,56,187,215,162,222,4,64,
         224,185,6,18,31,107,206,97,96,96,96,204,96,202,5,82,44,172,194,91,63,
         101,48,37,50,229,177,50,236,86,183,61,202,151,5,0>>

RpbGetResp protoc decode:
content {
  value: "v2"
  vtag: "3SDlf4INKz8hNdhyImKIru"
  last_mod: 1271442363
  last_mod_usecs: 105696
}
vclock: "k316a```314`312005R,254302[?e0%23452612354V267=312227005000"
```
