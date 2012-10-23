---
title: PBC オブジェクトを削除する
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Object/Key Operations"
---

指定されたバケット / キーからオブジェクトを削除する


## リクエスト

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

オプション パラメータ

* **rw** - 成功のレスポンスが返る前に何個のレプリカを削除するか; 指定できる値は次の特殊な値 'one'(4294967295-1)、'quorum' (4294967295-2)、'all' (4294967295-3)、'default' (4294967295-4)、および N ([[デフォルトではバケットごとに定義|(PBCAPI#Set Bucket Properties]]) 以下の任意の整数です。
* **vclock** - 以前の RpbGetResp メッセージで提供された、見えないベクトルクロック。これは最後のGetリクエストから後で変更されたオブジェクトを削除してしまうことを防止するために使われます。
* **r** - (read quorum) オブジェクトを取得する際に、何個のレプリカが必要か; 指定できる値は次の特殊な値'one' (4294967295-1)、'quorum' (4294967295-2)、'all' (4294967295-3)、'default' (4294967295-4)、および N ([[デフォルトではバケットごとに定義|PBC API#Set Bucket Properties]]) 以下の任意の整数です。
* **w** - (write quorum) 成功のレスポンスが返る前に何個のレプリカに書きこむか; 指定できる値は次の特殊な値 'one' (4294967295-1)、'quorum' (4294967295-2)、'all' (4294967295-3)、'default' (4294967295-4)、および N ([[デフォルトではバケットごとに定義|PBC API#Set Bucket Properties]]) 以下の任意の整数です。
* **pr** - (primary read quorum) オブジェクトを取得する際に、何個のプライマリレプリカが必要か; 指定できる値は次の特殊な値 'one' (4294967295-1)、'quorum' (4294967295-2)、'all' (4294967295-3)、'default' (4294967295-4)、および N ([[デフォルトではバケットごとに定義|PBC API#Set Bucket Properties]]) 以下の任意の整数です。
* **pw** - 書き込みを行う時に何個のプライマリノードが動作していなければならないか; 指定できる値は次の特殊な値 'one' (4294967295-1)、'quorum' (4294967295-2)、'all' (4294967295-3)、'default' (4294967295-4)、および N ([[デフォルトではバケットごとに定義|PBC API#Set Bucket Properties]]) 以下の任意の整数です。
* **dw** - 成功のレスポンスを返す前に、何個のレプリカが確実にコミットされていなければならないか; 指定できる値は次の特殊な値 'one' (4294967295-1)、'quorum' (4294967295-2)、'all' (4294967295-3)、'default' (4294967295-4)、および N ([[デフォルトではバケットごとに定義|PBC API#Set Bucket Properties]]) 以下の任意の整数です。

## レスポンス

メッセージコードだけが返ります。

## サンプル

リクエスト

```bash
Hex      00 00 00 12 0D 0A 0A 6E 6F 74 61 62 75 63 6B 65
         74 12 01 6B 18 01
Erlang <<0,0,0,18,13,10,10,110,111,116,97,98,117,99,107,101,116,18,1,107,24,1>>

RpbDelReq protoc decode:
bucket: "notabucket"
key: "k"
rw: 1

```


レスポンス

```bash
Hex      00 00 00 01 0E
Erlang <<0,0,0,1,14>>

RpbDelResp - only message code defined
```
