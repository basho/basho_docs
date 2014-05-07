---
title: PBC Get Bucket Properties
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Bucket Operations"
moved: {
  '1.4.0-': '/references/apis/protocol-buffers/PBC-Get-Bucket-Properties'
}
---

Fetch a bucket's properties.

## Request

```protobuf
message RpbGetBucketReq {
    required bytes bucket = 1;
    optional bytes type = 2;
}
```

The bucket's name (`bucket`) must be specified. The [[bucket type|Using Bucket Types]] parameter (`type`) is optional. If it is not specified, the `default` bucket type will be used.

## Response

When an `RpbGetBucketReq` message is sent to Riak, it will respond with an `RpbGetBucketResp` message, which returns the bucket's properties:

```protobuf
message RpbGetBucketResp {
    required RpbBucketProps props = 1;
}
```

The `RpbBucketProps` value itself is structured as follows:

```protobuf
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
    optional bool consistent = 27;
}
```

#### Optional Response Values

Each `RpbBucketProps` message returns all of the properties associated with a particular bucket. Default values for bucket properties, as well as descriptions of all of the above properties, can be found in the [[configuration file|Configuration Files#Default-Bucket-Properties]] documentation.

It should be noted that the value of an `RpbBucketProps` message may include other message types, such as `RpbModFun` (specifying module-function pairs for bucket properties that require them) and `RpbCommitHook` (specifying the module-function pair and name of a commit hook). Those message types are structured like this:

```protobuf
message RpbModFun {
    required bytes module = 1;
    required bytes function = 2;
}

message RpbCommitHook {
    optional RpbModFun modfun = 1;
    optional bytes name = 2;
}
```

<div class="note">
<div class="title">Note on <tt>RpbReplMode</tt></div>
The <tt>RpbReplMode</tt> is of use only to users of Riak CS's <a href="http://docs.basho.com/riakcs/latest/cookbooks/MDC-Overview/">Multi-Datacenter Replication capabilities</a>.
</div>
