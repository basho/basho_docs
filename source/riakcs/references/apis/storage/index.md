---
title: RiakCS Storage API
project: riakcs
version: 1.2.0+
document: api
toc: true
index: true
audience: advanced
keywords: [api, http]
---


ストレージAPIはAmazon S3 REST APIと互換性があります。これはリストにあるオペレーションは、一般的なS3ライブラリやツールを使って実行できるということです。

## サービスレベル オペレーション

* [[サービスを取得|RiakCS GET Service]] - リクエストを送ったユーザがオーナであるすべてのバケットのリストを返す

## バケットレベル オペレーション

* [[バケットを取得|RiakCS GET Bucket]] - バケット内のオブジェクトのリストを返す
* [[バケットのACLを取得|RiakCS GET Bucket ACL]] - バケットに関連付けられているACLを返す
* [[バケットを格納|RiakCS PUT Bucket]] - 新規バケットを作成する
* [[バケットのACLを格納|RiakCS PUT Bucket ACL]] - バケットにACLパーミッションを設定する
* [[バケットを削除|RiakCS DELETE Bucket]] - バケットを削除する

## オブジェクトレベル オペレーション

* [[オブジェクトを取得|RiakCS GET Object]]- オブジェクトを取得する
* [[オブジェクトのACLを取得|RiakCS GET Object ACL]] - オブジェクトに関連付けられているACLを返す
* [[オブジェクトを格納|RiakCS PUT Object]] - バケットにオブジェクトを格納する
* [[オブジェクトのACLを格納|RiakCS PUT Object ACL]] - オブジェクトに関連付けられているACLを設定する
* [[オブジェクトのヘッダを取得|RiakCS HEAD Object]] - オブジェクトのメタデータ(オブジェクトの内容すべてではない)を取得する
* [[オブジェクトを削除|RiakCS DELETE Object]]- オブジェクトを削除する

## 共通ヘッダ

* [[RiakCS 共通リクエストヘッダ|Common RiakCS Request Headers]]
* [[RiakCS 共通レスポンスヘッダ|Common RiakCS Response Headers]]


