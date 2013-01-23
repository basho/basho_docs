---
title: PBC バケットのプロパティ
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Bucket Operations"
---

バケットのプロパティを得る

## リクエスト


```bash
message RpbGetBucketReq {
    required bytes bucket = 1;
}
```


必要なパラメータ

* **bucket** - プロパティの取得対象のバケット

## レスポンス


```bash
message RpbGetBucketResp {
    required RpbBucketProps props = 1;
}
// Bucket properties
message RpbBucketProps {
    optional uint32 n_val = 1;
    optional bool allow_mult = 2;
}
```


値

* **n_val** - バケットの現在の n_val
* **allow_mult** - クライアントに競合を返すときは、allow_mult を true にする

## サンプル

リクエスト

```bash
Hex      00 00 00 0B 13 0A 08 6D 79 62 75 63 6B 65 74
Erlang <<0,0,0,11,19,10,8,109,121,98,117,99,107,101,116>>

RpbGetBucketReq protoc decode:
bucket: "mybucket"

```


レスポンス

```bash
Hex      00 00 00 07 14 0A 04 08 05 10 01
Erlang <<0,0,0,7,20,10,4,8,5,16,1>>

RpbGetBucketResp protoc decode:
props {
  n_val: 5
  allow_mult: true
}

```