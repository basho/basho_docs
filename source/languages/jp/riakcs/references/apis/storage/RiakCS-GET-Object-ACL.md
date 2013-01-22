---
title: RiakCS オブジェクトのACLを取得する
project: riakcs
version: 1.2.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
---

`GET Object acl` は、オブジェクトのアクセスコントロールリスト(ACL)を返すために、`acl` というサブリソースを使用します。

*ノート:* この操作を行うためには、オブジェクトに対する READ_ACP アクセス権限が必要です。

## リクエスト

### リクエストの書式

```
GET /ObjectName?acl HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signature_value
```

### リクエスト パラメータ

GET Object acl ではリクエスト パラメータを使用しません。

## レスポンスの要素

**AccessControlList** - ACL情報のコンテナ (Grant, Grantee, Permission)

* *種別*: コンテナ
* *継承*: AccessControlPolicy

**AccessControlPolicy** - ACL権限に設定する要素

* *種別*: 文字列
* *継承*: なし

**DisplayName** - バケット オーナの表示名

* *種別*: 文字列
* *継承*: AccessControlPolicy.Owner

**Grant** - `Grantee` および `Permission` 用のコンテナ

* *種別*: コンテナ
* *継承*: AccessControlPolicy.AccessControlList

**Grantee** - 権限を与えられるユーザの `ID`、`Emailaddress`、サブジェクトの`uri`

* *種別*: 文字列
* *継承*: AccessControlPolicy.AccessControlList.Grant

**ID** - バケット オーナのID

* *種別*: 文字列
* *継承*: AccessControlPolicy.Owner|AccessControlPolicy.AccessControlList.Grant

**Owner** - バケット オーナ情報のコンテナ

* *種別*: コンテナ
* *継承*: AccessControlPolicy

**Permission** - `Grantee` に与えるバケットの権限

* *種別*: 文字列
* *有効な値*: FULL_CONTROL|WRITE|READ_ACP
* *継承*: AccessControlPolicy.AccessControlList.Grant

## サンプル

### リクエストのサンプル

このリクエストでは、`basho-process.jpg` というオブジェクtのACLを返します。

```
GET /basho-process.jpg?acl HTTP/1.1
Host:bucket.data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
Authorization: AWS QMUG3D7KP5OQZRDSQWB6:4Pb+A0YT4FhZYeqMdDhYls9f9AM=
```

### レスポンスのサンプル

```
HTTP/1.1 200 OK
Date: Wed, 06 Jun 2012 20:47:15 GMT
Last-Modified: Mon, 04 Jun 2012 12:00:00 GMT
Content-Length: 124
Content-Type: text/plain
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)

  <AccessControlPolicy>
    <Owner>
      <ID>24ef09aa099d10f75aa57c8caeab4f8c8e7faeebf76c078efc7c6caea54ba06a</ID>
      <DisplayName>UserName@basho.com</DisplayName>
    </Owner>
    <AccessControlList>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                          xsi:type="CanonicalUser">
          <ID>24ef09aa099d10f75aa57c8caeab4f8c8e7faeebf76c078efc7c6caea54ba06a</ID>
          <DisplayName>UserName@basho.com</DisplayName>
        </Grantee>
        <Permission>FULL_CONTROL</Permission>
      </Grant>
    </AccessControlList>
  </AccessControlPolicy>
```
