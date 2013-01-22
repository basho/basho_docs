---
title: RiakCS オブジェクトを格納
project: riakcs
version: 1.2.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
---

`PUT Object` はバケットにオブジェクトを追加します。PUT Object は部分的なオブジェクトを追加するのではいので、成功のレスポンスがあればそれは、オブジェクト全体がバケットに追加されたことを意味します。

*ノート:* この操作を行うためには、オブジェクトに対する WRITE パーミッションが必要です。

Riak CS は分散型システムです。複数のオブジェクトから同時に書き込みのリクエストを受けると、システムは全てを書こうとしますが、最後のオブジェクトだけが書きこまれます。必要であれば、バージョニング機能か、アプリケーションでのオブジェクトのロック機能を構築してください。

ネットワークで転送中にデータが壊れることを防止するために、Content-MD5 ヘッダを使って、Riak CS にオブジェクトのMD5値を比較させることができます。値が一致しなければ、そのオペレーションはエラーを返します。さらに、PUT Object 操作は MD5 を計算し、返された MD5 と ETag を比較することができます。

*ノート*: リクエストボディに先駆けて送られるリクエストヘッダで、HTTP ステータスコードの `100-continue` を使うようにアプリケーションを作ることができます。これによって、認証エラーやリダイレクトのように、メッセージがヘッダで拒否されたときに、メッセージボディを送信しないですみます。

## アクセス パーミッション
PUT Object は、特定のアカウントやグループに、オブジェクトに対する指定したパーミッションを与えます。リクエストヘッダを使い、以下の2つの方法でアカウントやグループにパーミッションを与えることができます。

* x-amz-acl リクエストヘッダに定義済みの ACL を指定する。定義済み ACL についての詳細は [[here|http://docs.amazonwebservices.com/AmazonS3/latest/dev/ACLOverview.html#CannedACL]] を参照。
* Amzon S3 で使われている ACL パーミッションと対応している、x-amz-grant-read、x-amz-grant-write、x-amz-grant-read-acp、x-amz-grant-write-acp、x-amz-grant-full-control というヘッダで、明示的にアクセスパーミッションを指定する。

<div class="note">
<div class="title">Note</div>
定義済みの ACL または明示的なアクセスパーミッション設定のいずれかを使います。両方ではありません。
</div>

## リクエスト

### リクエストの書式

```
PUT /ObjectName HTTP/1.1
Host: bucketname.data.example.com
Date: date
Authorization: signature_value
```

### リクエストのヘッダ

PUT Object は、すべての操作に共通なリクエストヘッダに加えて、次のリクエストヘッダを提供します。

**Content-Length** - オブジェクトのバイト サイズ。このヘッダは必要である。

* *種別*: 文字列
* *デフォルト*: なし
* *制約*: なし

**Content-MD5** - RFC 1864 に従ったヘッダが無いメッセージの、base-64 でエンコードした、128 ビットの MD5 ダイジェスト。このヘッダはオプションではあるが、Content-MD5 ヘッダは、データが元のデータと相違ないことを確認するために利用できる。

* *種別*: 文字列
* *デフォルト*: なし
* *制約*: なし

**Content-Type** - コンテントのフォーマットを表す、標準的な MIME タイプ。

* *種別*: 文字列
* *デフォルト*: binary/octet-stream
* *有効な値*: 100-continue
* *制約*: なし

**Expect** - アプリケーション中で `100-continue` を使用すると、肯定的応答を受けるまでリクエストボディは送信されない。すなわち、メッセージがヘッダ上で拒否された場合はメッセージボディは送信されない。

* *種別*: 文字列
* *デフォルト*: なし
* *有効な値*: 100-continue
* *制約*: なし

**x-amz-meta-*** - オブジェクトに格納される、ユーザ指定のメタデータフィールド。

* *種別*: 文字列
* *デフォルト*: なし
* *制約*: なし

#### パーミッション リクエストヘッダ

**x-amz-acl** - このリクエストヘッダは、バケット作成時に、定義済みの ACL を指定します。定義済み ACL は、個々のアカウントまたは定められたグループに、指定したパーミッションを与えます。

* *種別*: 文字列
* *有効な値*: private | public-read | public-read-write | authenticated-read | bucket-owner-read | bucket-owner-full-control
* *制約*: なし

## サンプル

### リクエストのサンプル

`basho_docs` というバケットに、`basho-process.jpg` というオブジェクトを格納するためのリクエストです。

```
PUT /basho-process.jpg HTTP/1.1
Host: basho_docs.data.basho.com
Date: Fri, 01 Jun  2012 12:00:00 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=
Content-Type: text/plain
Content-Length: 201445
Expect: 100-continue
[201445 bytes of object data]
```

### リクエストのサンプル

```
HTTP/1.1 200 OK
Date: Fri, 01 Jun  2012 12:00:00 GMT
ETag: "32cf731c97645a398434535f271b2358"
Content-Length: 0
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```

### 定義済みアクセス パーミッションでリクエストする例

このリクエストでは `x-amz-acl` ヘッダを使って定義済み ACL を指定して、全員に READ パーいっションを与えます。

```
...Object data in the body...
PUT draftschedule.jpg HTTP/1.1
Host: myBucket.data.basho.com
x-amz-date: b24cf9553547f8b395dd038b34a81474
x-amz-acl: public-read
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=
Content-Length: 300
Expect: 100-continue
Connection: Keep-Alive

...Object data in the body...
```

### 定義済みアクセス パーミッションのレスポンス例

```
HTTP/1.1 200 OK
Date: b24cf9553547f8b395dd038b34a81474
ETag: "b24cf9553547f8b395dd038b34a81474"
Content-Length: 0
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```