---
title: PBC インデクス
project: riak
version: 1.2.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Query Operations"
---

セカンダリインデクスのクエリで比較されるキーのセットを要求する

## リクエスト


```bash
message RpbIndexReq {
    enum IndexQueryType {
        eq = 0;
        range = 1;
    }
    required bytes bucket = 1;
    required bytes index = 2;
    required IndexQueryType qtype = 3;
    optional bytes key = 4;
    optional bytes range_min = 5;
    optional bytes range_max = 6;
}
```


必要なパラメータ

* **bucket** - インデクスを得るバケット
* **index** - 使用するインデクス
* **qtype** - IndexQueryType を 0 (一致) または 1 (範囲) で指定する

インデクス クエリの方法は2種類あります。

* **eq** - 与えられた `key` と完全に一致
* **range** - 最小値と最大値の範囲内 (`range_min`, `range_max`)

オプション パラメータ

* **key** - 比較する値。qtype が eq の時にのみ使用される
* **range_min** - 比較範囲の最小値。qtype が range の時にのみ使用される
* **range_max** - 比較範囲の最大値。qtype が range の時ののみ使用される


## レスポンス

セカンダリインデクス クエリのリザルトは、リクエストパラメータと一致した、0個以上のキーの繰り返しリストとなります。


```bash
message RpbIndexResp {
    repeated bytes keys = 1;
}
```

値

* **keys** - インデクス リクエストと一致したキーのリスト


## サンプル

リクエスト

ここでは "farm" という名前のバケットで、"animal_bin" というインデクスにある "chicken" と一致するものを探します。

```bash
RpbIndexReq protoc decode:
bucket: "farm"
index: "animal_bin"
qtype: 0
key: "chicken"

Hex     00 00 00 1E 19 0A 04 66 61 72 6D 12 0A 61 6E 69
        6D 61 6C 5F 62 69 6E 18 00 22 07 63 68 69 63 6B 65 6E
Erlang  <<0,0,0,30,25,10,10,4,102,97,114,109,18,10,97,110,105,
          109,97,108,95,98,105,110,24,0,34,7,99,104,105,99,107,
          101,110>>
```

レスポンス

```bash
Hex     00 00 00 0F 1A 0A 03 68 65 6E 0A 07 72 6F 6F 73 74 65 72
Erlang  <<0,0,0,15,26,10,3,104,101,110,10,7,114,111,111,115,116,101,114>>

RpbIndexResp protoc decode:
keys: "hen"
keys: "rooster"
```
