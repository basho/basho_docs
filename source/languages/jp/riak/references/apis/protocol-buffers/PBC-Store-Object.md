---
title: PBC オブジェクトを格納する
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Object/Key Operations"
---

バケット / キーで示されたオブジェクトを格納します。
オブジェクトの格納には、自分で選択したキーを使う方法と、新しいオブジェクトを割り当てるときに Riak にキーを決めさせるのと、
2 つの方法があります。

#### リクエスト

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


必要なパラメータ

* **bucket** - 属しているバケット キー
* **content** - オブジェクトを、新規/更新する内容 - 同じ RpbContent メッセージを使い、
RpbGetRespの中にメタデータとバリューとを格納して返す

オプションパラメータ

* **key** - 作成/更新するキー。これが指定されなければ、
サーバが生成する。
* **vclock** - 以前の RpbGetResp メッセージで与えられた、暗黙のベクタークロック
これが新しいキー、あるいはあえて Sibling を作りたい時には指定しない。
* **w** - (write quorum) 書き込み成功のレスポンスを返すために、
いくつのレプリカを作らなければならないかを指定する;
指定できるのは、N ([[デフォルトではバケットごとに指定される|PBC API#Set Bucket Properties]]) 以下の整数、
あるいは特殊な値 'one' (4294967295-1)、'quorum' (4294967295-2)、
'all' (4294967295-3)、'default' (4294967295-4) である
* **dw** - 耐久記憶のために成功のレスポンスを返すために、
いくつのレプリカを保証しなければならないかを指定する;
指定できるのは、N ([[デフォルトではバケットごとに指定される|PBC API#Set Bucket Properties]]) 以下の整数、
あるいは特殊な値 'one' (4294967295-1)、'quorum' (4294967295-2)、
'all' (4294967295-3)、'default' (4294967295-4) である
* **return_body** - 格納されたオブジェクトの内容を返すか否か。
デフォルトは false である
* **pw** - 書き込みを行おうとするときに、
いくつのプライマリノードが動作していなければならないか;
指定できるのは、N ([[デフォルトではバケットごとに指定される|PBC API#Set Bucket Properties]]) 以下の整数、
あるいは特殊な値 'one' (4294967295-1)、'quorum' (4294967295-2)、
'all' (4294967295-3)、'default' (4294967295-4) である
* **if_not_modified** - 与えられたオブジェクトの vclock がデータベース内のデータと一致したときにのみ、
値を更新する
* **if_none_match** - バケット/キーの組み合わせがまだ定義されていないときにのみ、
値を格納する
* **return_head** - オブジェクト内の値が空白の時を除いて、*return_body" と同じで、
巨大な値を返すのを回避する

#### レスポンス


```bash
message RpbPutResp {
    repeated RpbContent contents = 1;
    optional bytes vclock = 2;        // the opaque vector clock for the object
    optional bytes key = 3;           // the key generated, if any
}
```


put リクエスト中の returnbody が true のとき、RpbPutResp には put が完了したオブジェクトの
次の、現在のオブジェクトを含みます。
key パラメータは、オブジェクトのキーをサーバが生成したときのみ、セットされますが、
returnbody には無条件でセットされます。
もしも returnbody がセットされておらず、キーが生成されなければ、put のレスポンスは空になります。


<div class="note"><p>特に注意: RpbGetResp のときと同様に siblings を含むことがあります。</p></div>


#### サンプル

リクエスト

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


レスポンス

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
