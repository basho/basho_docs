---
title: PBC Store Object
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Object/Key Operations"
---

Stores an object under the specified bucket / key. Storing an object comes in
two forms, depending on whether you want to use a key of your choosing, or let
Riak assign a key to a new object.

#### Request

```bash
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
    optional bool return_head = 11;%
}
```


Required Parameters

* **bucket** - bucket key resides in
* **content** - new/updated content for object - uses the same RpbContent
message RpbGetResp returns data in and consists of metadata and a value.

Optional Parameters

* **key** - key to create/update. If this is not specified the server will
generate one.
* **vclock** - opaque vector clock provided by an earlier RpbGetResp message.
Omit if this is a new key or you deliberately want to create a sibling
* **w** - (write quorum) how many replicas to write to before
returning a successful response; possible values include a special
number to denote 'one' (4294967295-1), 'quorum' (4294967295-2), 'all'
(4294967295-3), 'default' (4294967295-4), or any integer <= N
([[default is defined per the bucket|PBC API#Set Bucket Properties]])
* **dw** - how many replicas to commit to durable storage before
returning a successful response; possible values include a special
number to denote 'one' (4294967295-1), 'quorum' (4294967295-2), 'all'
(4294967295-3), 'default' (4294967295-4), or any integer <= N
([[default is defined per the bucket|PBC API#Set Bucket Properties]])
* **return_body** - whether to return the contents of the stored object.
Defaults to false.
* **pw** - how many primary nodes must be up when the write is
 attempted; possible values include a special number to denote 'one'
 (4294967295-1), 'quorum' (4294967295-2), 'all' (4294967295-3),
 'default' (4294967295-4), or any integer <= N
 ([[default is defined per the bucket|PBC API#Set Bucket Properties]])
* **if_not_modified** - update the value only if the vclock in the supplied
object matches the one in the database
* **if_none_match** - store the value only if this bucket/key combination are
not already defined
* **return_head** - like *return_body" except that the value(s) in the object
are blank to avoid returning potentially large value(s)

#### Response


```bash
message RpbPutResp {
    repeated RpbContent contents = 1;
    optional bytes vclock = 2;        // the opaque vector clock for the object
    optional bytes key = 3;           // the key generated, if any
}
```


If returnbody is set to true on the put request, the RpbPutResp will contain the
current object after the put completes. The key parameter will be set only if
the server generated a key for the object but it will be set regardless of
returnbody. If returnbody is not set and no key is generated, the put response
is empty.


<div class="note"><p>N.B. this could contain siblings just like an RpbGetResp
does.</p></div>


#### Example

Request

```bash
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


Response

```bash
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
