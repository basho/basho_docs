---
title: RiakCS サービスを取得
project: riakcs
version: 1.2.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
---

`GET Service` は *authenticated* でリクエストを送信したユーザが所有し、*認証(authenticated)* されている、すべてのバケットのリストを返します。

*ノート:* GET Service では、他のユーザが作成したバケットのリストは取得できません。同様に匿名で作られたバケットもリストされません。

## リクエスト

### リクエストの書式

```
GET / HTTP/1.1
Host: data.basho.com
Date: date
Authorization: signature_value
```

## レスポンスの要素

**Bucket** - バケット情報のコンテナ

* *種別*: コンテナ
* *子*: Name,CreationDate
* *継承*: ListAllMyBucketsResult.Buckets

**Buckets** - 1つ以上のバケットのコンテナ

* *種別*: コンテナ
* *子*: バケット
* *継承*: ListAllMyBucketsResult

**CreationDate** - バケットが作成された日付

* *種別*: 日付 (フォーマット yyyy-mm-ddThh:mm:ss.timezone, 例: 2012-06-03T15:4548:02.000Z)
* *継承*: ListAllMyBucketsResult.Buckets.Bucket

**DisplayName** - バケット オーナの表示名

* *種別*: 文字列
* *継承*: ListAllMyBucketsResult.Owner

**ID** - バケット オーナのユーザID

* *種別*: 文字列
* *継承*: ListAllMyBucketsResult.Owner

**ListAllMyBucketsResult** - レスポンス用のコンテナ

* *種別*: コンテナ
* *子*: Owner, Buckets
* *継承*: なし

**Name** - バケットの名前

* *種別*: 文字列
* *継承*: ListAllMyBucketsResult.Buckets.Bucket

**Owner** - バケットのオーナ情報用のコンテナ

* *種別*: コンテナ
* *継承*: ListAllMyBucketsResult

## サンプル

### リクエストのサンプル

サービス エンドポイント(この例では data.basho.com)上でのGET操作で、リクエストの送信者が所有する全てのバケットのリストが返される。

```
Host: data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
Authorization: AWS QMUG3D7KP5OQZRDSQWB6:4Pb+A0YT4FhZYeqMdDhYls9f9AM=
```

### レスポンスのサンプル

```xml
  <?xml version="1.0" encoding="UTF-8"?>
  <ListAllMyBucketsResult xmlns="http://data.basho.com/2012-06-12">
    <Owner>
      <ID>324ABC0713CD0B420EFC086821BFAE7ED81442C</ID>
      <DisplayName>"foobar</DisplayName>
    </Owner>
    <Buckets>
      <Bucket>
        <Name>projects</Name>
        <CreationDate>2011-05-10T14:10:15.000Z</CreationDate>
      </Bucket>
      <Bucket>
        <Name>templates</Name>
        <CreationDate>2011-05-10T14:18:25.000Z</CreationDate>
      </Bucket>
    </Buckets>
  </ListAllMyBucketsResult>
```
