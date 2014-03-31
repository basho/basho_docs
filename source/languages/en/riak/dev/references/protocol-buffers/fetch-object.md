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

Fetch an object from Riak.

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
}
```


Optional Parameters

* **r** - (read quorum) how many replicas need to agree when
retrieving the object; possible values include a special number to
denote 'one' (4294967295-1), 'quorum' (4294967295-2), 'all'
(4294967295-3), 'default' (4294967295-4), or any integer <= N
([[default is defined per the bucket|PBC API#Set Bucket Properties]])
* **pr** - (primary read quorum) how many primary replicas need to be
available when retrieving the object; possible values include a
special number to denote 'one' (4294967295-1), 'quorum'
(4294967295-2), 'all' (4294967295-3), 'default' (4294967295-4), or any
integer <= N
([[default is defined per the bucket|PBC API#Set Bucket Properties]])
* **basic_quorum** - whether to return early in some failure cases (eg. when r=1
and you get 2 errors and a success basic_quorum=true would return an error)
([[default is defined per the bucket|PBC API#Set Bucket Properties]])
* **notfound_ok** - whether to treat notfounds as successful reads for the
purposes of R ([[default is defined per the bucket|PBC API#Set Bucket
Properties]])
* **if_modified** - when a vclock is supplied as this option only return the
object if the vclocks don't match
* **head** - return the object with the value(s) set as empty - allows you to
get the metadata without a potentially large value
* **deletedvclock** - return the tombstone's vclock, if applicable

## Response


```protobuf
message RpbGetResp {
    repeated RpbContent content = 1;
    optional bytes vclock = 2;
    optional bool unchanged = 3;
}
```


Values

* **content** - value+metadata entries for the object. If there are siblings
there will be more than one entry. If the key is not found, content will be
empty.
* **vclock** - vclock Opaque vector clock that must be included in *RpbPutReq*
to resolve the siblings.
* **unchanged** - if if_modified was specified in the get request but the object
has not been modified, this will be set to true

The content entries hold the object value and any metadata


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


Links store references to related bucket/keys and can be accessed through link
walking in MapReduce.


```protobuf
// Link metadata
message RpbLink {
    optional bytes bucket = 1;
    optional bytes key = 2;
    optional bytes tag = 3;
}
```

<div class="note">
<div class="title">Missing keys</div>
Remember: if a key is not stored in Riak, an <tt>RpbGetResp</tt> response
without content and vclock fields will be returned. This should be mapped to whatever convention the client language uses to return not found, e.g. the
Erlang client returns an atom <tt>{error, notfound}</tt>.
</div>
