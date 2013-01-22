---
title: RiakCS バケットを取得
project: riakcs
version: 1.2.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
---

`GET Bucket` ではバケット内のオブジェクトのリスト(すべて、または1,000まで)を返します。

*ノート:* この操作を行うためには、READアクセス権限がなければいけません。

## リクエスト

### リクエストの書式

```
GET / HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signature_value
```

### リクエストのパラメータ

バケット内のオブジェクトの一部を返すために次のパラメータを使用します。

**prefix** - どの文字列から始まるキーを返すかを指定する

キーの先頭の文字で、バケット内のオブジェクトをグループ分けすることができます。

* *種別*: 文字列
* *デフォルト*: なし

## レスポンスの要素

**Contents** - レスポンスとして返される、オブジェクトのメタデータ

* *種別*: XMLメタデータ
* *継承*: ListBucketResult

**CommonPrefixes** - もしあるなら、`Prefix`と、その次の部分を分ける`delimiter`文字列

リクエストに`delimiter`が含まれていた場合は、`CommonPrefixes`を含むレスポンスのみが返されます。`CommonPrefixes`は、`Prefix`で指定されたキーを、ディレクトリにおけるサブディレクトリのようにしてキーをリストします。たとえば`Prefix`が、*projects/* で、`delimiter` が */* だとすると、*projects/marketing/2012* の共通前置部(common prefix)は *projects/marketing/* となります。共通前置部を持つキーが一気に返され、その数が数えられます(`MaxKeys`で制限されます)。


* *種別*: String
* *継承*: ListBucketResult

**Delimiter** - `prefix`と`delimiter`の最初の部分が一致したキーが、`CommonPrefixes`の集合として、1回のリザルトで一気に返されます。それ以外の者についてはレスポンスに返りません。


* *種別*: 文字列
* *継承*: ListBucketResult

**DisplayName** - オブジェクトのオーナの表示名


* *種別*: 文字列
* *継承*: ListBucketResult.Contents.Owner

**ETag** - エントリタグはオブジェクトのMD5ハッシュで、オブジェクトのメタデータではなく、内容の変化を反映します。


* *種別*: 文字列
* *継承*: ListBucketResult.Contents

**ID** - オブジェクトのオーナのユーザID


* *種別*: 文字列
* *継承*: ListBucketResult.Contents.Owner

**IsTruncated** - すべてのリザルトを返した(`true`)のか、一部だけ(`false`)なのかを示します。これは`MaxKeys`によってリザルトとして返せる上限が制限されるからです。


* *種別*: 文字列
* *継承*: boolean

**Key** - オブジェクトのキー


* *種別*: 文字列
* *継承*: ListBucketResult.Contents

**LastModified** - オブジェクトが最後に変更されたときの日付と時刻


* *種別*: Date
* *継承*: ListBucketResult.Contents

**Marker** - バケット内での、オブジェクトのリストの開始位置


* *種別*: 文字列
* *継承*: ListBucketResult

**MaxKeys** - レスポンスボディで返されるキーの最大数


* *種別*: 文字列
* *継承*: ListBucketResult

**Name** - バケットの名前


* *種別*: 文字列
* *継承*: ListBucketResult

**Owner** - バケットのオーナ


* *種別*: 文字列
* *子*: DisplayName, ID
* *継承*: ListBucketResult.Contents|CommonPrefixes

**Prefix** - 前置部で始まるキー


* *種別*: 文字列
* *継承*: ListBucketResult

**Size** - オブジェクトのバイト サイズ


* *種別*: 文字列
* *継承*: ListBucketResult.Contents

**StorageClass** - 常に STANDARD とする


* *種別*: 文字列
* *継承*: ListBucketResult.Contents

## サンプル

### リザルトのサンプル
`projects` というバケット内のオブジェクトを返させるためのリクエストです。

```
GET / HTTP/1.1
Host: projects.data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
Authorization: AWS QMUG3D7KP5OQZRDSQWB6:4Pb+A0YT4FhZYeqMdDhYls9f9AM=
Content-Type: text/plain
```

### レスポンスのサンプル

```xml
  <?xml version="1.0" encoding="UTF-8"?>
  <ListBucketResult xmlns="http://data.basho.com/2012-06-12">
      <Name>projects</Name>
      <Prefix/>
      <Marker/>
      <MaxKeys>1000</MaxKeys>
      <IsTruncated>false</IsTruncated>
      <Contents>
          <Key>scheduleQ1.jpg</Key>
          <LastModified>2012-06-01T09:20:03.000Z</LastModified>
          <ETag>"f77127731fba39869dede5c9645a3328"</ETag>
          <Size>519226</Size>
          <StorageClass>STANDARD</StorageClass>
          <Owner>
              <ID>324ABC0713CD0B420EFC086821BFAE7ED81442C</ID>
              <DisplayNamefoobar</DisplayName>
          </Owner>
      </Contents>
      <Contents>
         <Key>scheduleQ2.jpg</Key>
           <LastModified>2012-06-02T11:02:42</LastModified>
          <ETag>"645a39851b2cf27731c974f535343328"</ETag>
          <Size>990102</Size>
          <StorageClass>STANDARD</StorageClass>
          <Owner>
              <ID>324ABC0713CD0B420EFC086821BFAE7ED81442C</ID>
              <DisplayName>foobar</DisplayName>
          </Owner>
      </Contents>
  </ListBucketResult>
```

### リクエスト パラメータを使用した、リクエストのサンプル

このサンプルリクエストは、`projects` バケット内で、`IT`で始まるキーを最大100個リストさせるリクエストで、結果として`ITdb`からのキーが返ります。

```
GET ?prefix=IT HTTP/1.1
Host: projects.data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
Authorization: AWS QMUG3D7KP5OQZRDSQWB6:4Pb+A0YT4FhZYeqMdDhYls9f9AM=
```

### リクエスト パラメータで返るレスポンスのサンプル

```
HTTP/1.1 200 OK
x-amz-id-2: gyB+3jRPnrkN98ZajxHXr3u7EFM67bNgSAxexeEHndCX/7GRnfTXxReKUQF28IfP
x-amz-request-id: 3B3C7C725673C630
Date: Wed, 06 Jun 2012 20:48:15 GMT
Content-Type: application/xml
Content-Length: 302
Connection: close
Server: BashoData

  <?xml version="1.0" encoding="UTF-8"?>
  <ListBucketResult xmlns="http://data.basho.com/2012-06-12/">
    <Name>projects</Name>
    <Prefix>IT</Prefix>
    <Marker></Marker>
    <MaxKeys>1000</MaxKeys>
    <IsTruncated>false</IsTruncated>
    <Contents>
      <Key>ITdb</Key>
      <LastModified>2012-06-01T09:20:03.000Z</LastModified>
      <ETag>"f77127731fba39869dede5c9645a3328"</ETag>
      <Size>29493</Size>
      <StorageClass>STANDARD</StorageClass>
      <Owner>
        <ID>B420EFC086821B324ABC0713CD0FAE7ED81442C</ID>
        <DisplayName>richardp</DisplayName>
       </Owner>
    </Contents>
    <Contents>
      <Key>ITstorage</Key>
      <LastModified>2012-04-14T04:20:10.000Z</LastModified>
      <ETag>"a96f00ad9f27c3828ef3fdf83fc9ac7f"</ETag>
      <Size>4</Size>
      <StorageClass>STANDARD</StorageClass>
       <Owner>
        <ID>324ABC0713CD0B420EFC086821BFAE7ED81442C</ID>
        <DisplayName>foobar</DisplayName>
      </Owner>
   </Contents>
  </ListBucketResult>
```
