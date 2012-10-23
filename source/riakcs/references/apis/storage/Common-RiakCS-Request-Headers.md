---
title: RiakCS 共通リクエストヘッダ
project: riakcs
version: 1.2.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
---

Riak CS の REST リクエストすべてに共通なヘッダを示します。

**Authorization** - 認証リクエストに必要な情報。このヘッダは匿名リクエストの場合には必要がない。

**Content-Length** - RFC 2616 に従ったヘッダが無いメッセージの長さ。このヘッダは、XML をロードする操作および PUT で必要となる。

**Content-Type** - リソースのコンテントタイプ。

**Content-MD5** - RFC 1864 に従ったヘッダが無いメッセージの、base-64 でエンコードした 128 ビット MD5 ダイジェスト。このヘッダはオプションではあるが、Content-MD5 ヘッダは、データが元のデータと相違ないことを確認するために利用できる。

**Date** - リクエスト側の現在の日時。たとえば Fri, 01 Jun 2012 12:00:00 GMT。`Authorization` ヘッダを使うときには `x-amz-date` または `Date` ヘッダを使用しなければならない。

**Expect** - アプリケーション中で `100-continue` を使用すると、肯定的応答を受けるまでリクエストボディは送信されない。すなわち、メッセージがヘッダ上で拒否された場合はメッセージボディは送信されない。

* *有効な値*: 100-continue

**Host** - パス形式のリクエストでは、値は `data.basho.com` のようになる。仮想形式のリクエストでは、値は `backetname.data.basho.com` のようになる。

このヘッダは、HTTP/1.0 のリクエストではオプションですが、HTTP 1.1では必要です。

**x-amz-date** - リクエスト側の現在の日時。たとえば Fri, 01 Jun 2012 12:00:00 GMT。`Authorization` ヘッダを使うときは、`x-amz-date` または `Date` ヘッダを使用しなければならない。両方が指定された場合は、このヘッダの値が優先される。
