---
title: PBC Set Bucket Properties
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Bucket Operations"
moved: {
  '1.4.0-': '/references/apis/protocol-buffers/PBC-Set-Bucket-Properties'
}
---

Set the properties for a bucket

<div class="note">
<div class="title">Note</div>
{{#1.4.0-}}
The PBC interface does not currently support all bucket properties. It is currently limited to <tt>allow_mult</tt> and <tt>n_val</tt>. Other bucket properties would need to be set using the [[HTTP API|HTTP Set Bucket Properties]].
{{/1.4.0-}}
{{#1.4.0+}}
Currently, all bucket properties can be modified through the PBC interface with two exceptions: <tt>datatype</tt> and <tt>consistent</tt>.
{{/1.4.0+}}
</div>

## Request

{{#1.4.0-}}

```bash
message RpbSetBucketReq {
    required bytes bucket = 1;
    required RpbBucketProps props = 2;
}
// Bucket properties
message RpbBucketProps {
    optional uint32 n_val = 1;
    optional bool allow_mult = 2;
}
```
{{/1.4.0-}}

{{#1.4.0+}}

```bash
message RpbSetBucketReq {
    required bytes bucket = 1;
    required RpbBucketProps props = 2;
    optional bytes type = 3;
}
// Bucket properties
message RpbBucketProps {
    optional uint32 n_val = 1;
    optional bool allow_mult = 2;
    optional bool last_write_wins = 3;
    repeated RpbCommitHook precommit = 4;
    optional bool has_precommit = 5 [default = false];
    repeated RpbCommitHook postcommit = 6;
    optional bool has_postcommit = 7 [default = false];
    optional RpbModFun chash_keyfun = 8;
    optional RpbModFun linkfun = 9;
    optional uint32 old_vclock = 10;
    optional uint32 young_vclock = 11;
    optional uint32 big_vclock = 12;
    optional uint32 small_vclock = 13;
    optional uint32 pr = 14;
    optional uint32 r = 15;
    optional uint32 w = 16;
    optional uint32 pw = 17;
    optional uint32 dw = 18;
    optional uint32 rw = 19;
    optional bool basic_quorum = 20;
    optional bool notfound_ok = 21;
    optional bytes backend = 22;
    optional bool search = 23;

    enum RpbReplMode {
        FALSE = 0;
        REALTIME = 1;
        FULLSYNC = 2;
        TRUE = 3;
    }

    optional RpbReplMode repl = 24;
    optional bytes search_index = 25;
    optional bytes datatype = 26;
}
```
{{/1.4.0+}}

Required Parameters

Parameter | Description |
:---------|:------------|
`bucket` | Bucket to set properties for |
`props` | Updated properties (only set properties to change) |
`n_val` | Current `n_val` for the bucket | {{#1.4.0-}}
`allow_mult` | Set to `true` if conflicts are returned to clients | {{#1.4.0-}}

## Response

Only the message code is returned.

## Example

Change `allow_mult` to true for the bucket `friends`:

Request

```bash
Hex      00 00 00 0E 15 0A 07 66 72 69 65 6E 64 73 12 02
         10 01
Erlang <<0,0,0,14,21,10,7,102,114,105,101,110,100,115,18,2,16,1>>

RpbSetBucketReq protoc decode:
bucket: "friends"
props {
  allow_mult: true
}

```

Response

```bash
Hex      00 00 00 01 16
Erlang <<0,0,0,1,22>>

RpbSetBucketResp - only message code defined
```
