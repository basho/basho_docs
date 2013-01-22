---
title: アカウントの管理
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator]
---

## ユーザアカウントを作成する

ユーザアカウントの作成は、HTTP POST または PUT で、ユニークな email アドレスとユーザ名を使って行います。たとえば、

```bash
curl -H 'Content-Type: application/json' -X POST http://localhost:8080/riak-cs/user --data '{"email":"foobar@example.com", "name":"foo bar"}'
```

<div class="note"><div class="title">ノート</div>
デフォルトでは、管理者のみが新しいユーザアカウントを作成することができます。もしも、認証なしにユーザアカウントを作る必要があるなら、Riak CS の <tt>app.config</tt> に、<tt>{anonymous_user_creation, true}</tt> をセットしなければなりません。
</div>

ユーザのデータは JSON か XML で提出します。このとき、その形式と Content-Type ヘッダの値とは一致させるべきです。

JSON と XML による入力フォームのサンプルを示します。

JSON:

```json
{
  "email" : "foobar@example.com",
  "name" : "foo bar"
}
```

XML:

```xml
<User>
  <Email>foobar@example.com</Email>
  <Name>foo bar</Name>
</User>
```

レスポンスは JSON か XML で、次のようになるはずです。

JSON:

```json
{
    "email": "foobar@example.com",
    "display_name": "foobar"
    "key_id": "324ABC0713CD0B420EFC086821BFAE7ED81442C",
    "key_secret": "5BE84D7EEA1AEEAACF070A1982DDA74DA0AA5DA7",
    "name": "foo bar",
    "id":"8d6f05190095117120d4449484f5d87691aa03801cc4914411ab432e6ee0fd6b",
    "buckets": []
}
```

XML:

```xml
<User>
  <Email>foobar@example.com</Email>
  <DisplayName>foobar</DisplayName>
  <KeyId>324ABC0713CD0B420EFC086821BFAE7ED81442C</KeyId>
  <KeySecret>5BE84D7EEA1AEEAACF070A1982DDA74DA0AA5DA7</KeySecret>
  <Name>foo bar</Name>
  <Id>8d6f05190095117120d4449484f5d87691aa03801cc4914411ab432e6ee0fd6b</Id>
  <Buckets></Buckets>
</User>
```

ユーザアカウントができたら、Riak CS に認証リクエストを行うために `key_id` と `key_secret` を使うことができます。それには、デフォルトでは `~/.s3cmd` フォルダにある、s3cmd 設定ファイルに、key_id と key_secret 値を追加します。

* JSON \`key_id\` -> \`~/.s3cmd\` \`access_key\` key_id_value_here
* JSON \`key_secret\` -> \`~/.s3cmd\` \`secret_key\` key_secret_value_here

`id` フィールドで示された正規の ID は、ACL パーミッションが与えられたとき、あるいは無効にされたときに、ユーザの email アドレスを識別する代替手段として使うことができます。たとえば `--acl-grant` や、`--acl-revoke` オプションを `s3cmd setacl` にしたときなどです。

* JSON `id` -> `USER_CANONICAL_ID`

## ユーザ アカウント情報を取得する
正しく署名されたリクエストを **riak-cs/user** リソースに送信することで、ユーザが自身のアカウント情報を得ることができます。さらに、管理者は、管理者権限でもって、システム内のあらゆるユーザの情報をリクエストできます。ユーザは自分自身のアカウントの情報取得だけが許可されています。

適切な証明書が `.s3cfg` ファイルに設定されていると仮定すると、s3cmd リクエストでは次のような情報が取得できます。

    s3cmd get s3://riak-cs/user -

管理者証明を使うと他のユーザの情報も次のように所得できます。

    s3cmd -c ~./s3cfg-admin get s3://riak-cs/user/XQKMYF4UL_MMTDFD6NCN

この例では、XQKMYF4UL_MMTDFD6NCN というのが、管理者が取得しようとしているユーザの key_id です。

## すべてのユーザのリストを取得する
管理者はシステム内の全てのユーザアカウントのリストを取得することができます。これを実行するには、正しい署名付きの HTTP GET リクエストを **riak-cs/users** リソースに送ります。管理者以外のユーザからのユーザリスト取得リクエストは、拒否され、403 Forbidden エラーが返ります。このリクエストは s3cmd では正しく動きませんが、s3-curl のように特化しすぎていないツールを使って実行できます。

ユーザリストをリクエストする URL の例はこのようになります。

    GET http://data.example.com/riak-cs/users -

.s3curl 設定ファイルで管理者の ID が正しく証明されているとして、s3-curl を使うには次のようにします。

    s3curl --id admin -- http://data.mystorage.me/riak-cs/users

デフォルトでは、すべてのユーザのリストには、有効なアカウントと無効なアカウントの両方が含まれます。ステータスクエリのパラメータで有効、または無効を指定して、有効/無効なアカウントだけにフィルタすることができます。

## ユーザアカウントを有効/無効にする
ユーザは **/riak-cs/user** へ PUT することで、自身のアカウントを無効にすることができます。この PUT には、status フィールドに disabled という値を持たせなければなりません。JSON フォーマットと XML フォーマットが利用できます。それぞれの例を示します。Content-Type ヘッダは適切に指定しておいてください。管理ユーザは **/riak-cs/user/<user-key-id>** に PUT して、ユーザアカウントを無効にしたり、再び有効にしたりすることができます。ユーザは、いったん無効にした自分のアカウントを有効にし直すことはできません。

JSON によるステータス更新の例:

```json
{
  "status" : "enabled"
}
```

XML によるステータス更新の例:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<UserUpdate>
<Status>disabled</Status>
</UserUpdate>
```

## 新規ユーザの証明書を発行する
**/riak-cs/user** に適切な JSON か XML ドキュメントを PUT して、ユーザアカウントの key_secret を再発行することができます。管理ユーザは **/riak-cs/user/<key-id>** への PUT が可能です。

ドキュメントは次のようになります。

JSON の例:

```json
{"new_key_secret":true}
```

XML の例:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<UserUpdate>
  <NewKeySecret>true</NewKeySecret>
</UserUpdate>
```

<div class="note">
<div class="title">ノート</div>
`new_key_secret` フィールドは、同じリクエストの中で、他のユーザの更新情報と結合されてしまうことがあります。現時点では、これ以外にサポートしているフィールドは status だけですが、将来はもっと追加される予定です。未サポートのフィールドは無視されます。
</div>
