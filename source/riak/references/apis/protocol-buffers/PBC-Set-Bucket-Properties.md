---
title: PBC バケットのプロパティ設定
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Bucket Operations"
---

バケットのプロパティを設定する

<div class="note"><p>現時点では、PBC インタフェースは全てのバケットのプロパティに対応してはいません。
それは、現時点では、<code>allow_mult</code> と <code>n_val</code> に制限されています。
その他のバケットのプロパティは [[HTTP API|HTTP Set Bucket Properties]] で設定される必要があります。
</p>
</div>


## リクエスト


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


必要なパラメータ

* **bucket** - プロパティを設定するバケット
* **props** - 更新するプロパティ ｰ 変更したいプロパティのみをセットする
* **n_val** - バケットの現在の n_val
* **allow_mult** - allow_mult を true にすると、クライアントに競合が返る

## レスポンス

メッセージコードのみが返ります。

## サンプル

バケット "friends" の `allow_mult` が true になります。

リクエスト

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


レスポンス

```bash
Hex      00 00 00 01 16
Erlang <<0,0,0,1,22>>

RpbSetBucketResp - メッセージコードのみが示されます
```