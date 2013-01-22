---
title: RiakCS オブジェクトのACLを格納
project: riakcs
version: 1.2.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
---

`PUT Object acl` は、バケット内の既存のオブジェクトにアクセスコントローリリスト(ACL)パーミッションを設定するために、`acl` サブクラスを使用します。

*ノート:* この操作を行うために、オブジェクトに対する WRITE_ACP アクセス権限が必要です。

`PUT Object acl` はオブジェクトのパーミッションを設定するために2つの方法を用意しています。

* リクエストボディで ACL を指定する
* リクエストヘッダを使ってパーミッションを指定する

*ノート*: リクエストボディで ACL を指定するか、リクエストヘッダを使うかのいずれか一方です。同時ではありません。

## リクエスト

### リクエストの書式

ここでは ACL をリクエストボディで設定するときの書式を示します。ヘッダセクションにはコレで使う以外のヘッダが含まれています。

```
PUT /ObjectName?acl HTTP/1.1
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

この操作ではリクエストパラメータを使用しません。

### リクエストヘッダ

`PUT Object acl` では、すべての操作に共通なリクエストヘッダに加えて、次のリクエストヘッダを提供します。

**x-amz-acl** - 今リクエストヘッダは、オブジェクト作成時に、定義済みの ACL を指定します。定義済み ACL は、個々のアカウントまたは定められたグループに、指定したパーミッションを与えます。

* *種別*: 文字列
* *有効な値*: private | public-read | public-read-write | authenticated-read | bucket-owner-read | bucket-owner-full-control
* *デフォルト*: private

### リクエストの要素

リクエストボディで ACL を指定するときは、以下の要素を使わなければいけません。

**AccessControlList** - ACL 情報用のコンテナ (Grant, Grantee, Permission).

* *種別*: コンテナ
* *継承*: AccessControlPolicy

**AccessControlPolicy** - 設定する ACL パーミッションの要素を入れる

* *種別*: コンテナ
* *継承*: なし

**DisplayName** - オブジェクト オーナの表示名

* *種別*: 文字列
* *継承*: AccessControlPolicy.Owner

**Grant** - `Grantee` と `Permission` 用のコンテナ

* *種別*: コンテナ
* *継承*: AccessControlPolicy.AccessControlList

**Grantee** - 誰がパーミッションを受け取るかというサブジェクトThe subject who is being granted permissions.

* *種別*: 文字列
* *有効な値*: DisplayName|EmailAddress|AuthenticatedUser
* *継承*: AccessControlPolicy.AccessControlList.Grant

**ID** - オブジェクト オーナの ID

* *種別*: 文字列
* *継承*: AccessControlPolicy.Owner|AccessControlPolicy.AccessControlList.Grant

**Owner** - オブジェクト オーナの情報

* *種別*: コンテナ
* *継承*: AccessControlPolicy

**Permission** - `Grantee` に与えられるパーミッション

* *種別*: 文字列
* *有効な値*: FULL_CONTROL|WRITE_ACP|READ|READ_ACP
* *継承*: AccessControlPolicy.AccessControlList.Grant

リクエストの要素では、次の方法で誰にパーミッションを与えるのかを指定します。

* *emailAddress*: アカウントのメールアドレス

```
  <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CustomerByEmail">
    <EmailAddress>user1@basho.com</EmailAddress>
  </Grantee>
```

ユーザはメールアドレスから CanonicalUser として解決されます。`GET Object acl` のレスポンスでは、ユーザが CanonicalUser として示されます。

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

PUT Bucket acl はレスポンス要素を返しません。

## サンプル

### リクエストボディにアクセスパーミッションを指定した例

このサンプルでは、リクエストボディの ACL で `basho-process.jpg` という既存のオブジェクトにアクセスパーミッションを割り当てます。さらに、バケットオーナはフルコントロールできるように、ユーザIDで特定されるアカウントにフルコントロールの権限を与えます。

```
PUT /basho-process.jpg?acl HTTP/1.1
Host: basho_docs.data.basho.com
Date: Fri, 01 Jun  2012 12:00:00 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=
Content-Length: 124

  <AccessControlPolicy>
    <Owner>
      <ID>75aa57f09aa0c8caeab4f8c24e99d10f8e7faeebf76c078efc7c6caea54ba06a</ID>
      <DisplayName>user1@basho.com</DisplayName>
    </Owner>
    <AccessControlList>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser">
          <ID>75aa57f09aa0c8caeab4f8c24e99d10f8e7faeeExampleCanonicalUserID</ID>
          <DisplayName>user2@basho.com</DisplayName>
        </Grantee>
        <Permission>FULL_CONTROL</Permission>
      </Grant>
    </AccessControlList>
  </AccessControlPolicy>
```

### レスポンスのサンプル

バージョン管理が有効な場合のレスポンスのサンプルです。

```
HTTP/1.1 200 OK
Date: Fri, 01 Jun  2012 12:00:00 GMT
Last-Modified:Fri, 01 Jun  2012 10:30:15 GMT
Content-Length: 0
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```

### ヘッダでアクセス パーミッションを設定するためのリクエストの例

以下は、全員にオブジェクトの読み出しアクセスを与えるために、x-amz-acl というリクエストヘッダに、定義済みの ACL (public_read) を設定しています。

```
PUT basho-process.jpg?acl HTTP/1.1
Host: examplebucket.data.basho.com
x-amz-acl: public-read
Accept: */*
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=
Host: data.basho.com
Connection: Keep-Alive
```

### ヘッダでパーミッションを設定したときのレスポンスのサンプル

```
HTTP/1.1 200 OK
x-amz-id-2: ZDsjJI9E3ke4WK56w5YegkbG6RWPxNQHIQ0CjrjyRVFZhEbabXnBO9w5G7Dmxsgk
x-amz-request-id: 827BD84C13B255B1
Date:  Fri, 01 Jun  2012 12:00:00 GMT
Content-Length: 0
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```
