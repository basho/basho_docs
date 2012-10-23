---
title: RiakCS 共通レスポンスヘッダ
project: riakcs
version: 1.2.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
---

Riak CS の REST レスポンス全てに共通なヘッダを示します。

**Content-Length** - レスポンスボディのバイトサイズ

* *種別*: 文字列
* *デフォルト*: なし

**Connection** - サーバへの接続がオープンか、クローズか

* *種別*: Enum
* *Valid Values*: open|close
* *デフォルト*: なし

**Date** - Riak CS が応答した日付と時間。たとえば Fri, 01 Jun 2012 12:00:00 GMT

* *種別*: 文字列
* *デフォルト*: なし

**ETag** - エンティティ タグは、オブジェクトの MD5 ハッシュで、メタデータではなくオブジェクトの内容が変化したことを示すものです。ETag は、オブジェクトが作成されたときにセットされます。


* *種別*: 文字列

**Server** - レスポンスを行ったサーバの名前

* *種別*: 文字列
* *デフォルト*: なし
