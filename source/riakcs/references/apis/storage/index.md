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


ストレージ API は Amazon S3 REST API と互換性があります。これはリストにあるオペレーションは、一般的な S3 ライブラリやツールを使って実行できるということです。

## API 機能の比較

以下の表は現時点での Amazon S3 機能の対応状況です。

<table>
    <tr>
        <th WIDTH="50%">機能</th>
        <th WIDTH="20%">状況</th>
        <th WIDTH="30%">メモ</th>
    </tr>
    <tr>
        <td>GET Service(認証ユーザが全てのバケットのリストを取る)</td>
        <td>対応</td>
        <td></td>
    </tr>
    <tr>
        <td>DELETE Bucket</td>
        <td>対応</td>
        <td></td>
    </tr>
    <tr>
        <td>PUT Bucket</td>
        <td>対応</td>
        <td></td>
    </tr>



    <tr>
        <td>Bucket Lifecycle</td>
        <td>非対応</td>
        <td></td>
    </tr>
    <tr>
        <td>Policy (Buckets, Objects)</td>
        <td>近日</td>
        <td>今後のリリースにて</td>
    </tr>
    <tr>
        <td>Bucket Website</td>
        <td>非対応</td>
        <td></td>
    </tr>
    <tr>
        <td>Bucket ACLs (GET, PUT)</td>
        <td>対応</td>
        <td></td>
    </tr>
    <tr>
        <td>Bucket Location</td>
        <td>非対応</td>
        <td></td>
    </tr>
    <tr>
        <td>Bucket Notification</td>
        <td>非対応</td>
        <td></td>
    </tr>
    <tr>
        <td>Bucket Object Versions</td>
        <td>非対応</td>
        <td></td>
    </tr>
    <tr>
        <td>GET Bucket Info (HEAD)</td>
        <td>対応</td>
        <td></td>
    </tr>
    <tr>
        <td>Bucket Request Payment</td>
        <td>非対応</td>
        <td></td>
    </tr>
    <tr>
        <td>PUT Object</td>
        <td>対応</td>
        <td></td>
    </tr>
    <tr>
        <td>PUT Object (Copy)</td>
        <td>近日</td>
        <td>今後のリリースにて</td>
    </tr>
    <tr>
        <td>DELETE Object</td>
        <td>対応</td>
        <td></td>
    </tr>
    <tr>
        <td>GET Object</td>
        <td>対応</td>
        <td></td>
    </tr>
    <tr>
        <td>Object ACLs (GET, PUT)</td>
        <td>対応</td>
        <td></td>
    </tr>
    <tr>
        <td>HEAD Object</td>
        <td>対応</td>
        <td></td>
    </tr>
    <tr>
        <td>POST Object</td>
        <td>非対応</td>
        <td></td>
    </tr>
    <tr>
        <td>Copy Object</td>
        <td>近日</td>
        <td>今後のリリースにて</td>
    </tr>
    <tr>
        <td>Multipart Uploads</td>
        <td>近日</td>
        <td>今後のリリースにて</td>
    </tr>
</table>

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


