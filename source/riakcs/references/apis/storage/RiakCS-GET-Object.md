---
title: RiakCS オブジェクトを取得する
project: riakcs
version: 1.2.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
---

`GET Object` ではRiak CSのストレージからオブジェクトを取得します。

*ノート:* この操作を行うためには、オブジェクトに対する READ アクセス権限が必要です。匿名ユーザが READ アクセス権限を持つと、認証ヘッダを使わずにオブジェクトを取得することができます。

Get Objectはオブジェクトを取得します。

## リクエスト

### リクエストの書式

```
GET /objectName HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signature_value
Range:bytes=byte_range
```

## サンプル

### リクエストのサンプル

以下のリクエストでは、`basho-process.jpg` というオブジェクトが返ります。

```
GET /basho-process.jpg HTTP/1.1
Host: bucket.data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
Authorization: AWS AKIAIOSFODNN7EXAMPLE:0RQf4/cRonhpaBX5sCYVf1bNRuU=
```

### レスポンスのサンプル

```
HTTP/1.1 200 OK
Date: Wed, 06 Jun 2012 20:48:15 GMT
Last-Modified: Wed, 06 Jun 2012 13:39:25 GMT
ETag: "3327731c971645a398fba9dede5f2768"
Content-Length: 611892
Content-Type: text/plain
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
[611892 bytes of object data]
```
