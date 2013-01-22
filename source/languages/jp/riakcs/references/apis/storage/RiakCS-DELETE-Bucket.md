---
title: RiakCS バケットを削除
project: riakcs
version: 1.2.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
---

`DELETE Bucket` は、URIで指定されたバケットを削除します。

<div class="note"><div clas="title">ノート</div>バケットを削除する前に、バケット内のすべてのオブジェクトを削除しておくこと</div>

## リクエスト

### リクエストの書式

```
DELETE / HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signature_value
```

## レスポンス

バケットの削除では、共通のレスポンスヘッダのみを使用し、いかなるレスポンス エレメントも返しません。

## サンプル

### リクエストのサンプル

"projects" という名前のバケットを削除します。

```
DELETE / HTTP/1.1
Host: projects.data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
Authorization: AWS QMUG3D7KP5OQZRDSQWB6:4Pb+A0YT4FhZYeqMdDhYls9f9AM=
```

### レスポンスのサンプル

```
HTTP/1.1 204 No Content
Date: Wed, 06 Jun 2012 20:47:15 +0000
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```
