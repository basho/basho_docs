---
title: HTTP オブジェクトを格納する
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Object/Key Operations"
---

指定されたバケット / キーにオブジェクトを格納する。
オブジェクトの格納方法には、選択したキーを使うのか、Riakが新しいオブジェクトにキーをアサインするのかという2つの方法がある。

## リクエスト

```bash
POST /riak/bucket               # Riak-defined key, old format
POST /buckets/bucket/keys       # Riak-defined key, new format
PUT /riak/bucket/key            # User-defined key, old format
PUT /buckets/bucket/keys/key    # User-defined key, new format
```

古いクライアントとの互換性のために、`POST`はキーの指定による方法も受け付けます。

重要なヘッダ:

* `Content-Type` 格納するオブジェクトがセットされていなければならない。次のリクエストで受け取れるように、セットしておくこと。
* `X-Riak-Vclock` オブジェクトが既に存在するとき、ベクトルクロック オブジェクトが読み出しに付加される。
* `X-Riak-Meta-*` - オブジェクトと共に格納される任意の追加メタデータ ヘッダ
* `X-Riak-Index-*` - インデクスと作るべきオブジェクトのインデクスエントリ
[[セカンダリインデクスについての詳細|HTTP Secondary Indexes]]
* `Link` - 他のリソースへの、ユーザおよびシステム定義のリンク [[リンクについて知る|Links]]

オプショナルヘッダ (`PUT`に対してのみ有効):

* `If-None-Match`、`If-Match`、`If-Modified-Since`、`If-Unmodified-Since`は、条件付きリクエスト セマンティクスを呼び出し、既存のオブジェクトの`ETag`および`Last-Modified`とマッチします。
これらは修正されたオブジェクトが上書きされるのを防止することができます。
比較に失敗したときは、`412 Precondition Failed`が返ります。これは同時書き込みを妨げるものではありません。つまり、複数のリクエストが同時に行われたとして、すべて true の場合のみ有効です。

オプショナル クエリ パラメータ:

* `w` (write quorum) 書き込み成功のレスポンスを返す前に、何個のレプリカが書き込まれなければならないか(デフォルトはバケット レベルで定義される)
* `dw` (durable write quorum) 成功のレスポンスを返す前に、何個のレプリカが確実に格納されたか(デフォルトはバケット レベルで定義される)
* `pw` 書き込みを行う前に何個のプライマリ レプリカがオンラインにならなくてはいけないか(デフォルトはバケット レベルで定義される)
* `returnbody=[true|false]` 格納されたオブジェクトの内容を返すか否か

*<ins>このリクエストはボディ(エンティティ)を含まなくてはならない</ins>*

## レスポンス

正常ステータスコード:

* `201 Created` (キー無しで指定された場合y)
* `200 OK`
* `204 No Content`
* `300 Multiple Choices`

主なエラーコード:

* `400 Bad Request` - たとえば、r、ｗ、dw パラメータが無効(> N)なとき
* `412 Precondition Failed` じうけんリクエストヘッダのどれかが一致しなかった場合(上述)

重要なヘッダ:

* `Location` 新しく作られたオブジェクトへの相対URL(キー無しで指定されたとき)

`returnbody=true`のとき、すべてのレスポンスヘッダは [[HTTPフェッチオブジェクト|HTTP-Fetch-Object]] があることを期待しています。
オブジェクトをフェッチするときのように、兄弟が存在する、あるいは操作中に作成されると、`300 Multiple Choices`が返り、レスポンスは同じようにしょりされます。

## サンプル: キー無しで保存する

```bash
$ curl -v -d 'this is a test' -H "Content-Type: text/plain" http://127.0.0.1:8098/riak/test
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> POST /riak/test HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
> Content-Type: text/plain
> Content-Length: 14
>
< HTTP/1.1 201 Created
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Location: /riak/test/bzPygTesROPtGGVUKfyvp2RR49
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/json
< Content-Length: 0
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
```

## サンプル: キーありで格納する

```bash
$ curl -v -XPUT -d '{"bar":"baz"}' -H "Content-Type: application/json" -H "X-Riak-Vclock: a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==" http://127.0.0.1:8098/riak/test/doc?returnbody=true
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> PUT /riak/test/doc?returnbody=true HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
> Content-Type: application/json
> X-Riak-Vclock: a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==
> Content-Length: 13
>
< HTTP/1.1 200 OK
< X-Riak-Vclock: a85hYGBgymDKBVIszMk55zKYEhnzWBlKIniO8kGF2TyvHYIKfwcJZwEA
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Link: </riak/test>; rel="up"
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/json
< Content-Length: 13
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
{"bar":"baz"}
```
