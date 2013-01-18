---
title: RiakCS バケットのACLを格納
project: riakcs
version: 1.2.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
---

`PUT Bucket acl` は、アクセスコントロールリスト(ACL)を使って既存のバケットにパーミッションを設定するために、`acl` というサブリソースを使用します。

*ノート:* この操作を行うために、バケットに対する WRITE_ACP アクセス権限が必要です。

`PUT Bucket acl` でバケットのパーミッションを設定するために2つの方法を用意しています。

* リクエストボディで ACL を指定する
* リクエストヘッダを使ってパーミッションを指定する

*ノート*: リクエストボディで ACL を指定するか、リクエストヘッダを使うかのいずれか一方です。同時ではありません。

## リクエスト

### リクエストの書式

ここではACLをリクエストボディで設定するときの書式を示します。ヘッダセクションにはこれで使う以外のヘッダが含まれています。

```
PUT /?acl HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signatureValue

  <AccessControlPolicy>
    <Owner>
      <ID>ID</ID>
      <DisplayName>EmailAddress</DisplayName>
    </Owner>
    <AccessControlList>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser">
          <ID>ID</ID>
          <DisplayName>EmailAddress</DisplayName>
        </Grantee>
        <Permission>Permission</Permission>
      </Grant>
      ...
    </AccessControlList>
  </AccessControlPolicy>
```

### リクエストパラメータ

この操作ではリクエスト パラメータを使用しません。

### リクエストヘッダ
`PUT Bucket acl` は、全ての操作に共通なリクエストヘッダに加えて、次のリクエストヘッダを提供します。

**x-amz-acl** - このリクエストヘッダは、バケット作成時に、定義済みの ACL を指定します。定義済み ACL は、個々のアカウントまたは定められたグループに、指定したパーミッションを与えます。


* *種別*: String
* *有効な値*: private | public-read | public-read-write | authenticated-read | bucket-owner-read | bucket-owner-full-control
* *デフォルト*: private

### リクエストの要素

リクエストボディでACLを指定するときには、以下の要素を使わなければいけません。

**AccessControlList** - ACL情報 (Grant(譲渡物), Grantee(受領者), Permission(パーミッション)) のコンテナ

* *種別*: Container
* *継承*: AccessControlPolicy

**AccessControlPolicy** - 設定する ACL パーミッションの要素を入れる

* *種別*: コンテナ
* *継承*: なし

**DisplayName** - バケット オーナの表示名

* *種別*: 文字列
* *継承*: AccessControlPolicy.Owner

**Grant** - `Grantee` と `Permission` 用のコンテナ

* *種別*: コンテナ
* *継承*: AccessControlPolicy.AccessControlList

**Grantee** - パーミッションを譲り受けるユーザの `ID`、`Emailaddress`、サブジェクトの`uri`

* *種別*: 文字列
* *継承*: AccessControlPolicy.AccessControlList.Grant

**ID** - バケット オーナのID

* *種別*: 文字列
* *継承*: AccessControlPolicy.Owner|AccessControlPolicy.AccessControlList.Grant

**Owner** - バケット オーナの情報

* *種別*: コンテナ
* *継承*: AccessControlPolicy

**Permission** - バケットのパーミッションは `Grantee` に与えられる

* *種別*: 文字列
* *有効な値*: FULL_CONTROL|WRITE|WRITE_ACP|READ|READ_ACP
* *継承*: AccessControlPolicy.AccessControlList.Grant

リクエストの要素では、次の方法で誰にパーミッションを与えるのかを指定します。

* *emailAddress*: アカウントのメールアドレス

```
  <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CustomerByEmail">
    <EmailAddress>user1@basho.com</EmailAddress>
  </Grantee>
```

ユーザは、メールアドレスから CanonicalUser として扱われます。`GET Object acl` のレスポンスでは、ユーザを CanonicalUser として示します。

* *id*: アカウントのユーザID

```
  <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser">
    <ID>ID</ID>
    <DisplayName>GranteesEmail</DisplayName>
  </Grantee>
```

ID方式では、DisplayName はオプションで、リクエスト内では無視されます。

* *uri*: グループを定義する URI

```
  <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Group">
    <URI>http://data.basho.com/groups/AuthenticatedUsers<URI>
  </Grantee>
```

### レスポンスの要素

PUT Bucket acl はレスポンスの要素を返しません。

## サンプル

### リクエストボディにアクセスパーミッションを指定した例
このサンプルでは、リクエストボディの ACL で basho_docs という既存のバケットにアクセスパーミッションを割り当てます。さらに、バケットオーナはフルコントロールできるように、次のようにリクエストを行います。

* AllUsers グループにはバケットへの READ パーミッションを与える
* Dev グループにはバケットへの WRITE パーミッションを与える
* メールアドレスで識別されるアカウントへ WRITE_ACP パーミッションを与える
* 正規のユーザIDで識別されるアカウントへ READ_ACP パーミッションを与える

```
PUT /?acl HTTP/1.1
Host: basho_docs.data.basho.com
Content-Length: 1660202
x-amz-date: Fri, 01 Jun  2012 12:00:00 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=

  <AccessControlPolicy xmlns="http://data.basho.com/doc/2012-04-05/">
    <Owner>
      <ID>BucketOwnerCanonicalUserID</ID>
      <DisplayName>OwnerDisplayName</DisplayName>
    </Owner>
    <AccessControlList>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser">
          <ID>852b113e7a2f25102679df27bb0ae12b3f85be6BucketOwnerCanonicalUserID</ID>
          <DisplayName>OwnerDisplayName</DisplayName>
        </Grantee>
        <Permission>FULL_CONTROL</Permission>
      </Grant>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Group">
          <URI xmlns="">http://acs.data.basho.com/groups/global/AllUsers</URI>
        </Grantee>
        <Permission xmlns="">READ</Permission>
      </Grant>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Group">
          <URI xmlns="">http://acs.data.basho.com/groups/global/Dev</URI>
        </Grantee>
        <Permission xmlns="">WRITE</Permission>
      </Grant>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="AmazonCustomerByEmail">
          <EmailAddress xmlns="">user1@basho.com</EmailAddress>
        </Grantee>
        <Permission xmlns="">WRITE_ACP</Permission>
      </Grant>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser">
          <ID xmlns="">f30716ab7115dcb44a5ef76e9d74b8e20567f63TestAccountCanonicalUserID</ID>
        </Grantee>
        <Permission xmlns="">READ_ACP</Permission>
      </Grant>
    </AccessControlList>
  </AccessControlPolicy>
```

### レスポンスのサンプル

```
HTTP/1.1 200 OK
Date: Fri, 01 Jun  2012 12:00:00 GMT
Content-Length: 0
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```
