---
title: PBC Search
project: riak
version: 1.2.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Query Operations"
---

ドキュメントのリストと統計情報を取得するために検索リクエストを送信します。

## リクエスト


```bash
message RpbSearchQueryReq {
  required bytes  q      =  1;
  required bytes  index  =  2;
  optional uint32 rows   =  3;
  optional uint32 start  =  4;
  optional bytes  sort   =  5;
  optional bytes  filter =  6;
  optional bytes  df     =  7;
  optional bytes  op     =  8;
  repeated bytes  fl     =  9;
  optional bytes  presort = 10;
}
```

必要なパラメータ

* **q** - インデックスが示すバケット
* **index** - 検索するインデックスの名前

オプション パラメータ

* **rows** - 結果の最大行数
* **start** - 開始オフセット。値を返す際に指定した数のキーがスキップされる
* **sort** - 検索結果のソート方法
* **filter** - インラインフィールドにクエリを追加してフィルタ サーチを行う
* **df** - スキーマファイルの `default_field` をオーバライドする
* **op** - スキーマファイルの `default_op` の設定を "and" または "or" でオーバライドする
* **fl** - フィールドの制限を返す return the fields limit
* **presort** - プリソート (key / score)


## レスポンス

サーチ クエリは RpbSearchDocs リストの 0 個以上の繰り返しとして結果が返ります。
RpbSearchDocs は、リクエスト パラメータに基づく、
0 個以上の key/value ペア (RpbPair) となります。
さらに、検索スコアの最大値と結果の数を返します。


```bash
// RbpPair is a generic key/value pair datatype used for other message types
message RpbPair {
  required bytes key = 1;
  optional bytes value = 2;
}
message RpbSearchDoc {
  repeated RpbPair fields = 1;
}
message RpbSearchQueryResp {
  repeated RpbSearchDoc docs      = 1;
  optional float        max_score = 2;
  optional uint32       num_found = 3;
}
```

値

* **docs** - 検索条件に一致したドキュメントのリスト
* **max_score** - 最大値
* **num_found** - 検索条件に一致した値の合計が返される


## サンプル

リクエスト

ここでは `pig` という文字の入った動物を検索します。
検索対象は先頭の 100 個で、`name` フィールドで結果をソートします。

```bash
RpbSearchQueryReq protoc decode:
q: "pig*"
index: "animals"
rows: 100
start: 0
sort: "name"

Hex     00 00 00 1A 1B 0A 04 70 69 67 2A 12 07 61 6E
        69 6D 61 6C 73 18 64 20 00 2A 04 6E 61 6D 65
Erlang  <<0,0,0,26,27,10,4,112,105,103,42,18,7,97,110,
          105,109,97,108,115,24,100,32,0,42,4,110,97,
          109,101>>
```

レスポンス

```bash
Hex     00 00 00 36 1B 0A 1D 0A 0D 0A 06 61 6E 69 6D
        61 6C 12 03 70 69 67 0A 0C 0A 04 6E 61 6D 65
        12 04 66 72 65 64 0A 12 0A 10 0A 06 61 6E 69
        6D 61 6C 12 06 70 69 67 65 6F 6E 18 02
Erlang  <<0,0,0,54,27,10,29,10,13,10,6,97,110,105,109,
          97,108,18,3,112,105,103,10,12,10,4,110,97,
          109,101,18,4,102,114,101,100,10,18,10,16,10,
          6,97,110,105,109,97,108,18,6,112,105,103,
          101,111,110,24,2>>

RpbSearchQueryResp protoc decode:
docs {
  fields {
    key: "animal"
    value: "pig"
  }
  fields {
    key: "name"
    value: "fred"
  }
}
docs {
  fields {
    key: "animal"
    value: "pigeon"
  }
}
num_found: 2
```
