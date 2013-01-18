---
title: RiakCS バケットを格納
project: riakcs
version: 1.2.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
---

`PUT Bucket` は新しいバケットを作成します。バケット作成のリクエストを行ったユーザが、バケットのオーナになります。匿名リクエストではバケットを作成することはできません。

*ノート:* バケットを作成するには、認証リクエストに必要な、有効なキーIDが必要です。

## バケットに名前をつける

いちばんいいのは、DNSに準拠したバケット名を付けることです。DNS準拠のバケット名は、仮想ホストスタイルのリクエストでバケットを確定できます。

DNS準拠のバケット名とするためには、以下のルールに則る必要があります。

* 3文字以上63文字以内にすること
* 1つ以上のラベルをつけ、ラベル間はピリオド(.)で区切ること
* 小文字または数字で始めること。小文字または数字で終わること。小文字、数字、ダッシュが使用できる。
* IPアドレス(例 192.168.9.2)みたいなものは不可

## アクセス パーミッション

PUT Bucket は、指定したアカウントまたはグループにバケットのパーミッションを指定することができます。以下の2つの方法で、リクエストヘッダでアカウントまたはグループを指定してパーミッションを与えることができます。

* リクエストヘッダの x-amz-acl に定義済みのACLを指定する。定義済みACLについての詳細は [[here|http://docs.amazonwebservices.com/AmazonS3/latest/dev/ACLOverview.html#CannedACL]] 参照
* ヘッダで、Amzaon S3 互換のACLパーミッションに対応した x-amz-grant-read、x-amz-grant-write、x-amz-grant-read-acp、x-amz-grant-write-acp、x-amz-grant-full-control を明示する

*ノート*: 定義済みのACLを指定するか、アクセスパーミッションを明示するかのいずれかを使います。両方ではありません。

## リクエスト

### リクエストの書式

```
PUT / HTTP/1.1
Host: bucketname.data.basho.com
Content-Length: length
Date: date
Authorization: signature_value

	<CreateBucketConfiguration xmlns="http://data.basho.com/doc/2012-06-01/">
	  <LocationConstraint>BucketRegion</LocationConstraint>
	</CreateBucketConfiguration>
```
<div class="note"><div class="title">ノート</div>このサンプルには複数のリクエストヘッダが含まれています。リクエストヘッダ セクションには完全なヘッダのリストがあります。</div>

### リクエスト パラメータ

この操作ではリクエスト パラメータを使用しません。

### リクエストヘッダ

PUT Bucket は、全ての操作に共通なリクエストヘッダに加えて、次のリクエストヘッダを提供します。

**x-amz-acl** - このリクエストヘッダは、バケット作成時に、定義済みのACLを指定します。定義済みACLは、個々のアカウントまたは定められたグループに、指定したパーミッションを与えます。

* *種別*: 文字列
* *有効な値*: private | public-read | public-read-write | authenticated-read | bucket-owner-read | bucket-owner-full-control

### レスポンスの要素

PUT Bucket はレスポンス要素を返しません。

## サンプル

### リクエストのサンプル

`basho_docs` という名前のバケットを作成するリクエストです。

```
PUT / HTTP/1.1
Host: basho_docs.data.basho.com
Content-Length: 0
Date: Fri, 01 Jun  2012 12:00:00 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=
```

### レスポンスのサンプル

```
HTTP/1.1 200 OK
Date: Fri, 01 Jun  2012 12:00:00 GMT
Content-Length: 0
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```

### 定義済みACLを使ってアクセスパーミッションを設定するリクエストのサンプル

このリクエストでは `basho_docs` という名前のバケットを作成し、ACL を private に設定します。

```
PUT / HTTP/1.1
Host: basho_docs.data.basho.com
Content-Length: 0
x-amz-acl: private
Date: Fri, 01 Jun  2012 12:00:00 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=
```

### 定義済みACLをバケットに与えたときのレスポンスのサンプル

```
HTTP/1.1 200 OK
Date: Fri, 01 Jun  2012 12:00:00 GMT

Location: /basho_docs
Content-Length: 0
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```
