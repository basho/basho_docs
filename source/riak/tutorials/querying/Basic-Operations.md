---
title: Basic Requests
project: riak
version: 0.10.0+
document: tutorials
toc: true
audience: beginner
keywords: [querying, api, http]
prev: ["クエリする", "index.html"]
up:   ["クエリする", "index.html"]
next: "[[MapReduce]]"
---

Riak での操作のほとんどはキーにバリューをセットする、あるいは取得することです。このセクションでは Riak の [[HTTP API]] の使い方を説明しますが、中心となることは Riak の [[プロトコルバッファクライアント API|PBC API]] インタフェースと同じことです。

Riak は Erlang、Java、PHP、Python、Ruby、C/C++ 用の [[クライアントライブラリをサポート|Client Libraries]] しています。さらに、[[コミュニティ管理のプロジェクト|Communiy Developed Libraries and Projects]] として、.NET、Node.js、Python (および Twisted)、Griffon、Small Talk、Perl、Scala、Clojure、等々があります。

## HTTP の例題

HTTP 経由で Riak にリクエストを送る場合、以下の点に注意してください。

1. すべてのリクエストには *X-Riak-ClientId* ヘッダを含めてください。これはクライアントをユニークに識別できるどんな文字列でも構いません。[[ベクタークロック]] としてオブジェクトの変更を追跡するために使います。
2. バケット、キー、リンクにはエスケープなしのスラッシュを含んではいけません。URL エスケープライブラリを使用するか、スラッシュを `%2F` に置き換えてください。

## オブジェクトを読む

バケットからキーを取得するための基本的なコマンドは次のようになります。

```
GET /riak/bucket/key
```

オブジェクトはレスポンスのボディに含まれて返ります(もしあれば)。

Riak は、content-type の受け渡しのための `Accept` (siblings を取り扱うときは [[HTTP API での sibling の用例|HTTP Fetch Object#Siblings examples]] を参照) や、条件付きリクエストの `If-None-Match`/`Etag`、`If-Modified-Since`/`Last-Modified` のような、HTTP で定義された多くのヘッダを理解します。

さらに Riak は多くのクエリパラメータを受け付けます。GET リクエストの R 値 (オブジェクトの取得が成功するためには何個のレプリカが応答する必要があるか。R 地の詳細は Fast Track チュートリアルの最後のセクションにあります) をセットするための `r` などです。クエリパラメータ `r` を省略したときは、デフォルトの `r=2` が採用されます。

正常時のレスポンスコード:

* `200 OK`
* `300 Multiple Choices`
* `304 Not Modified`

主なエラーコード:

* `404 Not Found`

### エラーコードの例

この例では `test` バケットから `doc2` キーをリクエスト(GET)します。

```bash
$ curl -v http://127.0.0.1:8091/riak/test/doc2
```

キー `doc2` が存在しない(あなたはまだ作成していませんから) という、*404 Not Found* が返ります。

## 既存のキー、またはユーザ定義のキーにオブジェクトを格納する

往々にしてアプリケーションは、独自の方法でデータからキーを生成します。このとき、データの格納は容易です。リクエストは基本的にこのようになります。

```
PUT /riak/bucket/key
```

キーを追加するとバケットが自動的に生成されることを思い出してください。わざわざバケットを "create" する必要はありません(バケットおよびそのプロパティにちてはこのページの下の方で説明します)。

リクエストヘッダによっては PUT が必要です。

* `Content-Type` は、格納するオブジェクトにセットしなければなりません。次回にリクエストしたときにこれがセットされています。
* `X-Riak-Vclock` は、存在するオブジェクトを読み出したときに、ベクタークロックが付加されます。オブジェクトが新規の場合は、このヘッダは無視されます。

PUT の際は、その他のヘッダはオプションです

* `X-Riak-Meta-YOUR_HEADER` オブジェクトと一緒に格納される、任意のメタデータヘッダです。
* `Link` ユーザおよびシステム定義の、他のリソースに対するリンクです。詳しくは [[リンク|Links]] を参照してください。

GET リクエストが `r` クエリパラメータをサポートするのと同様に、PUT リクエストでもこれらのパラメータをサポートしています。

* `r` 既存のオブジェクトに書きこむ前に、何個のレプリカが合意擦る必要があるか *(整数値、デフォルトは 2)*
* `w` 何個のレプリカに書き込めれば成功のレスポンスを返すか *(整数値、デフォルトは 2)*
* `dw` how many replicas to commit to durable storage before returning a successful response *(integer, default is 0)*
* `returnbody` whether to return the contents of the stored object *(boolean, default is false)*

Normal status codes:

* `200 OK`
* `204 No Content`
* `300 Multiple Choices`

### Example

If `returnbody=true`, any of the response headers expected from a GET request may be present. Like a GET request, `300 Multiple Choices` may be returned if siblings existed or were created as part of the operation, and the response can be dealt with similarly.

Try running this in a terminal.

```bash
$ curl -v -XPUT -d '{"bar":"baz"}' -H "Content-Type: application/json" \
  -H "X-Riak-Vclock: a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==" \
  http://127.0.0.1:8091/riak/test/doc?returnbody=true
```

## Store a new object and assign random key

If your application would rather leave key-generation up to Riak, issue a POST request to the bucket URL instead of a PUT to a bucket/key pair:

```
POST /riak/bucket
```

If you don't pass Riak a "key" name after the bucket, it will know to create one for you.

Supported headers are the same as for bucket/key PUT requests, though *X-Riak-Vclock* will never be relevant for these POST requests.  Supported query parameters are also the same as for bucket/key PUT requests.

Normal status codes:

* `201 Created`

This command will store an object, in the bucket "test" and assign it a key:

```bash
$ curl -v -d 'this is a test' -H "Content-Type: text/plain" \
  http://127.0.0.1:8091/riak/test
```

In the output, the *Location* header will give the you key for that object. To view the newly created object, go to `http://127.0.0.1:8091/*_Location_*` in your browser.

If you've done it correctly, you should see the value (which is "this is a test").

## Delete an object

Lastly, you'll need to know how to delete keys.

The command, as you can probably guess, follows a predictable pattern and looks like this:

```
DELETE /riak/bucket/key
```

Normal status codes:

* `204 No Content`
* `404 Not Found`

404 responses are _normal_ in the sense that DELETE operations are idempotent and not finding the resource has the same effect as deleting it.

```bash
$ curl -v -X DELETE http://127.0.0.1:8091/riak/test/test2
```
