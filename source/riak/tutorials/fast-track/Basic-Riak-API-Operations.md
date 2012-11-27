---
title: Riak 基本 API の操作
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, fast-track]
prev: ["Building a Dev Environment", "Building-a-Development-Environment.html"]
up:   ["The Riak Fast Track", "index.html"]
next: ["MapReduce Queries", "Loading-Data-and-Running-MapReduce-Queries.html"]
---

このモジュールでは、Riak HTTP API を使用します。

## オブジェクト/キー操作

Riak はデータをバケット、キー、バリューとして管理します。バリュー(またはオブジェクト)はユニークなキーで識別され、各キー/バリュー ペアはバケットに格納されます。バケットは基本的に Riak 内のフラットなネームスペースです。複数のバケットに同一のキーネームを許したり、バケットごとにレプリケーションファクタや、pre/post-commit フックを設定する、ということにはあまり意味はありません。

Riak での操作のほとんどが、キーのバリューをセットするか、取得することです。このセクションでは、Riak HTTP API をどのように使うのかを説明します。また、Erlan、Java、PHP、Python、Ruby、C/C++ 用に[[サポートされているクライアントライブラリ|Client Libraries]]についても説明します。さらに[[コミュニティでサポートしているプロジェクト|Community Developed Libraries and Projects]]として、.NET、Node.js、Python(および Twisted)、Griffon、Small Talk、Perl、Scala、Clojure、その他もろもろがあります。

### 必要な知識

* *Client ID* - すべてのリクエストには *X-Riak-ClientId* ヘッダを含むべきで、これはクライアントをユニークに識別できるいかなる文字列でも構いません。[[ベクタークロック|Riak Glossary#Vercor Clock]] でオブジェクトの変化を追跡するのに使用します。
* *URL Escaping* - バケット、キー、リンク指定には、エスケープなしのスラッシュを含めてはいけません。URSエスケープ ライブラリを使うか、スラッシュを %2F に置き換えてください。

### オブジェクトを読む

バケットから指定したキーを取得するときは、このようになります。

```bash
GET /riak/bucket/key
```

レスポンスのボディにはオブジェクトの内容が含まれます(もしあれば)。

Riak は content-type ネゴシエーションの *Accept* のような、多くの HTTP 定義のヘッダを認識します。siblings を取り扱うときは、[[HTTP API における sibling のサンプル|HTTP Fetch Object#Siblings examples]] を、さらに条件つきリクエストには *If-None-Mach*/*ETag*、*If-Modified-Since*/*Last-Modified* を参照してください。

Riak はさまざまなクエリパラメータも受け付けます。GET リクエストにおける R-value は *r* で設定します(R value というのは、オブジェクトを取得する際に、レスポンスが成功するにはなんこのレプリカの一致が必要かを示すものです。R value については、Fast Track チュートリアルの最後のセクションで詳しく説明します)。*r* クエリパラメータを省略すると、Riak はデフォルトの *r=2* を採用します。

正常時のレスポンスコード:

* *200 OK*
* *300 Multiple Choices*
* *304 Not Modified*

主なエラーコード:

* *404 Not Found*

早い話、このコマンドを試してみてください。これは "test" というバケットから "doc2" というキーをリクエスト(GET)します。

```bash
$ curl -v http://127.0.0.1:8091/riak/test/doc2
```

ここでは *404 Not Found* が返り、"doc2" というキーがない(まだ作っていませんから！)ことを示します。

### 既存、またはユーザ定義のキーでオブジェクトを格納する

アプリケーションはしばしば、独自の方法でデータのキーを生成します。このとき、データの格納は簡単です。リクエストはこのようになります。

```bash
PUT /riak/bucket/key
```

<div class="info"><code>POST</code> は互換性のために残されている、有効な動詞です。</div>

キーを追加すると、バケットが自動的に作成されることに注意してください。バケットを明示的に "create" する必要はありません(バケットおよびそのプロパティについては、このページの後のほうで説明します)。

PUT にはリクエストヘッダが必要です:

* *Content-Type* は、オブジェクトを格納するためにセットしなければなりません。次回のリクエストでどのような形で受け取りたいのかをセットします。
* *X-Riak-Vclock* オブジェクトがすでに存在すれば、読み出しの際にベクタークロックが付加されます。新規オブジェクトの場合は、このヘッダは無視されます。

その他のリクエストヘッダはオプションです:

* *X-Riak-Meta-_YourHeader_* オブジェクトと共に格納できる任意のメタデータ ヘッダです。
* *Link* ユーザおよびシステム定義済みの、他のリソースへのリンクです。[[リンク|links]] についての詳細。

GET リクエストでの "r" クエリパラメータと同様に、PUT リクエストでもこれらのパラメータをサポートしています:

* *r* 書き込みの前にオブジェクトを取得するとき、何個のレプリカが一致する必要があるか(整数値、デフォルトは 2)
* *w* 成功のレスポンスを返す前に何個のレプリカへ書きこむか(整数値、デフォルトは 2)
* *dw* 成功のレスポンスを返す前に何個のレプリカを耐久記憶として保証するか(整数値、デフォルトは 0)
* *returnbody* 格納されているオブジェクトのコンテンツを返すか否か(ブーリアン文字列、デフォルトは "false")

正常時のステータスコード:

* *200 OK*
* *204 No Content*
* *300 Multiple Choices*

*returnbody=true* のとき、GET リクエストではなんらかのレスポンスへっだがあることを期待しています。siblings があったとき、あるいは操作の結果生成されたとき、レスポンスが同じだったときには GET リクエストのように、*300 Multiple Choices* を返します。

それではここで、ターミナルで試してみましょう。


```bash
$ curl -v -XPUT -d '{"bar":"baz"}' -H "Content-Type: application/json" \
  -H "X-Riak-Vclock: a85hYGBgzGDKBVIszMk55zKYEhnzWBlKIniO8mUBAA==" \
  http://127.0.0.1:8091/riak/test/doc?returnbody=true
```

### 神きオブジェクトを格納し、ランダムキーをアサインする

Riak に渡すキーを、アプリケーションで生成しない場合は、バケット/キー ペアではなく、バケットの URL に POST リクエストを発行します。

```bash
POST /riak/bucket
```

バケットの後に "key" を与えなければ、あなたの代わりにキーを生成しろということになります。

サポートしているヘッダは、バケット / キーの PUT リクエストと同様で、*X-Riak-Vclock* は PUT リクエストに影響を与えません。サポートしているクエリパラメータも、バケット/キー リクエストと同様です。

正常時のステータスコード:

* *201 Created*

このコマンドは、バケット "test" の中にオブジェクトを格納し、キーをアサインします:

```bash
$ curl -v -d 'this is a test' -H "Content-Type: text/plain" \
  http://127.0.0.1:8091/riak/test
```

出力内で、*Location* ヘッダでオブジェクトのキーを返します。作成された最新のオブジェクトを見るには、ブラウザで `http://127.0.0.1:8091/*_Location_*` にアクセスしてください。

正しくできていれば、バリュー(ここでは "this is a test") を確認できるはずです。

### オブジェクトを削除する

最後に、キーの削除方法を知っておく必要があります。

推測通り、そのコマンドは、前に出てきたとおりで、このようになります:

```bash
DELETE /riak/bucket/key
```

DELETE 操作時の正常レスポンスコードは *204 No Content* および *404 Not Found* です。

404 レスポンスは "normal" で、DELETE 操作の結果、リソースが削除されたために見つからないという意味です。

試してください:

```bash
$ curl -v -X DELETE http://127.0.0.1:8091/riak/test/test2
```

## バケットのプロパティと操作

バケット基本的に Riak 内のフラットなネームスペースです。複数のバケットに同一のキーネームを許したり、バケットごとに設定可能です。

<div class="info"><div class="title">バケットをいくつまで作れますか？</div>
現在のところ、バケットにはコストがかからないと想定されています、デフォルトのバケットプロパティを変更しない限りは。バケットのプロパティを変更すると、ゴシップとして周囲のクラスタに伝播する、つまりネットワーク中にデータが流れることになります。言い換えれば、デフォルトのバケットプロパティを使えば、フリーです。
</div>

### バケットのプロパティを変更する

バケットを "create" する必要はありません。キーが追加された時に作られ、すべてのキーが削除された時に消え去ります。

キーにネームスペースを割り当てることに加えて、バケットへのバリューの格納方法といった、バケットのプロパティも定義します。

プロパティをセットするには、バケットの URL に PUT を発行します:

```bash
PUT /riak/bucket
```

リクエストボディは "props" というエントリ 1 つの JSON オブジェクトにしてください。未定義のバケットプロパティは無視されます。

重要なヘッダ:

* Content-Type: *application/json*

バケットで気をつけなければならない重要なプロパティ:

* *n_val* - バケット内にいくつのレプリカを作るか(デフォルトは 3)。*n_val* は 0 以上、リング内のパーティション数以下の整数値です。

<div class="note">バケットにキーを追加した後になって *n_val* を変更することは、失敗する原因となり、望ましくありません。というのは、新しいバリューはまだ、所定のパーティションにレプリカが作られていないかもしれません。</div>

* *allow_mult* - _true_ または _false_ (デフォルトは _false_ )。Riak は、同時書き込みやネットワークのパーティショニングなどに起因する、あらゆる sibling オブジェクトを管理します。*allow_mult* を false にすると、クライアントが受け取るのは、タイムスタンプが最も新しいオブジェクトのみとなります。

それではバケットのプロパティを変えてみましょう。以下を PUT すると、"test" という名前の新しいバケットを作成し、n_val は 5 に変更されます。

```bash
$ curl -v -XPUT -H "Content-Type: application/json" -d '{"props":{"n_val":5}}' \
  http://127.0.0.1:8091/riak/test
```

### バケットを GET

HTTP API を使って、バケットプロパティ および/または キーを取得("GET")する方法です:

```bash
GET /riak/bucket_name
```

簡単ですね(パターンがわかりましたか？)

オプション クエリ パラメータ:

* *props=true|false* - バケットプロパティを返すか否か(デフォルトは "true")
* *keys=true|false|stream* - バケットに格納されているキーを返すか否か(デフォルトは "false")。*keys=stream* レスポンスの振る舞いの詳細は [[HTTP API's list keys|HTTP List Keys]] を参照

次はこのコマンドを実行してください。ここでは、さっきセットしたばかりのバケット情報を GET してみます。


```bash
$ curl -v http://127.0.0.1:8091/riak/test
```

ブラウザで `http://127.0.0.1:8091/riak/test` にアクセスすれば、バケットの情報を見ることができます。

以上が HTTP API がどのように働くのかの基本です。HTTP API のページ(下記のリンク)を熟読されることを強くお勧めします。そこには HTTP インタフェースを使用するにあたって、覚えておくべきヘッダ、パラメータ、ステータスについての詳細が記載されています。


<div class="title">このセクションのさらなる解説</div>

* [[HTTP API の詳細|HTTP API]]
* [[プロトコルバッファ API|PBC API]]
* [[レプリケーションの詳細|Replication]]
* [どうしてベクタークロックが簡単なのか|Why Vector Clocks are Easy](http://blog.basho.com/2010/01/29/why-vector-clocks-are-easy/)
* [どうしてベクタークロックはややこしいのか|Why Vector Clocks are Hard](http://blog.basho.com/2010/04/05/why-vector-clocks-are-hard/)

