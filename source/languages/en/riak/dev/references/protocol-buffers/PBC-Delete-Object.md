---
title: PBC Delete Object
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Object/Key Operations"
moved: {
  '1.4.0-': '/references/apis/protocol-buffers/PBC-Delete-Object/'
}
---

Deletes an object from the specified bucket / key.

## Request

```bash
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
}
```

Optional Parameters

* **rw** - how many replicas to delete before returning a successful
response; possible values include a special number to denote 'one'
(4294967295-1), 'quorum' (4294967295-2), 'all' (4294967295-3),
'default' (4294967295-4), or any integer <= N
([[default is defined per the bucket|PBC API#Set Bucket Properties]])
* **vclock** - opaque vector clock provided by an earlier RpbGetResp message.
Use to prevent deleting of objects that have been modified since the last get request
* **r** - (read quorum) how many replicas need to agree when retrieving the object; possible values include a special
number to denote 'one' (4294967295-1), 'quorum' (4294967295-2), 'all'
(4294967295-3), 'default' (4294967295-4), or any integer <= N
([[default is defined per the bucket|PBC API#Set Bucket Properties]])
* **w** - (write quorum) how many replicas to write to before returning a successful response; possible values include a special
number to denote 'one' (4294967295-1), 'quorum' (4294967295-2), 'all'
(4294967295-3), 'default' (4294967295-4), or any integer <= N
([[default is defined per the bucket|PBC API#Set Bucket Properties]])
* **pr** - (primary read quorum) how many primary replicas need to be available when retrieving the object; possible values include a special
number to denote 'one' (4294967295-1), 'quorum' (4294967295-2), 'all'
(4294967295-3), 'default' (4294967295-4), or any integer <= N
([[default is defined per the bucket|PBC API#Set Bucket Properties]])
* **pw** - how many primary nodes must be up when the write is attempted; possible values include a special
number to denote 'one' (4294967295-1), 'quorum' (4294967295-2), 'all'
(4294967295-3), 'default' (4294967295-4), or any integer <= N
([[default is defined per the bucket|PBC API#Set Bucket Properties]])
* **dw** - how many replicas to commit to durable storage before returning a successful response; possible values include a special
number to denote 'one' (4294967295-1), 'quorum' (4294967295-2), 'all'
(4294967295-3), 'default' (4294967295-4), or any integer <= N
([[default is defined per the bucket|PBC API#Set Bucket Properties]])

## Response

Only the message code is returned.

## Example

Request

```bash
Hex      00 00 00 12 0D 0A 0A 6E 6F 74 61 62 75 63 6B 65
         74 12 01 6B 18 01
Erlang <<0,0,0,18,13,10,10,110,111,116,97,98,117,99,107,101,116,18,1,107,24,1>>

RpbDelReq protoc decode:
bucket: "notabucket"
key: "k"
rw: 1

```


Response

```bash
Hex      00 00 00 01 0E
Erlang <<0,0,0,1,14>>

RpbDelResp - only message code defined
```
