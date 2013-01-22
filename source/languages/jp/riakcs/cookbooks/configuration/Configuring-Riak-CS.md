---
title: Riak CS の設定
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, configuration]
---

## 管理ユーザを指定する

管理ユーザは、バケットの作成、課金情報の取得などの権限を持っています。それ以外は他のユーザアカウントと違いはありません。

<div class="note"><div class="title">ノート</div>
管理ユーザを作る前に、まず Riak CS の <tt>app.config</tt> で <tt>{anonymous_user_creation, true}</tt> をセットしてください。管理ユーザを作成後は、これを disable にするべきです。
</div>

管理ユーザのアカウントを作るには、管理ユーザにしたいアカウントから HTTP POST を行います。以下に例を示します:

```
curl -H 'Content-Type: application/json' \
  -X POST http://localhost:8080/user \
  --data '{"email":"foobar@example.com", "name":"admin user"}'
```

JSON でのレスポンスは次のようになります:

```json
{
"Email": "foobar@example.com",
"DisplayName": "adminuser"
"KeyId": "324ABC0713CD0B420EFC086821BFAE7ED81442C",
"KeySecret": "5BE84D7EEA1AEEAACF070A1982DDA74DA0AA5DA7",
"Name": "admin user",
"Id":"8d6f05190095117120d4449484f5d87691aa03801cc4914411ab432e6ee0fd6b",
"Buckets": []
}
```

`Content-Type` を `application/xml` とすることで、オプションで XML を送信、受信することができます。

管理ユーザができたら、Riak CS システム内の各ノードに管理ユーザの証明を指定しなければなりません。管理ユーザの証明書は `etc/riak-cs` ディレクトリにある `app.config` ファイルにて行います。`admin_key` にダブルクォートでくくった `key_id` の文字列を貼り付けます。`admin_secret` 変数には以下のように `key_secret` 文字列を貼り付けます。

```
%% Admin user credentials
 {admin_key, "LXAAII1MVLI93IN2ZMDD"},
 {admin_secret, "5BE84D7EEA1AEEAACF070A1982DDA74DA0AA5DA7"},
```

## パブリックアドレスでのリッスン

ノードが 1 つであれば、リッスンするアドレスの設定を変える必要はありません。Riak CS は単純にローカルホストからのリクエストをリッスンしているからです。システムに複数のノードがあるのなら、Riak CS が他のノードからのリクエストをリッスンできるように、IP アドレスとポートを設定します。

パブリックアドレスの設定は、`/etc/riak-cs` ディレクトリ内の Riak CS の `app.config` ファイルにあります。設定は、このファイルの Riak CS の設定セクションにあります。

__riak_ip__: 127.0.0.1 から、接続したい Riak ノードの IP アドレスに書き換えます。

プロトコルバッファを使うために別なポートに設定するのであれば、以下のポート設定を変更しなければなりません:

__riak_pb_port__: 8087 から、`app.config` ファイルの `pb_port` 変数にあるポート番号に書き換えます。

<div class="note"><div class="title">ノート</div>ここで設定した IP アドレスは、app.config ファイルの pb_ip 変数の値と一致していなければなりません。サーバに複数のネットワーク インタフェース カード(NIC)があるときは、どれか 1 つの NIC の IP アドレスを使います。すべてに対してリッスンさせたいのなら、riak_ip を "0.0.0.0" にしてください。</div> 

ポート番号を変更したら、Riak をリスタートしてください。

## Stanchion ノードを指定する
ノードが 1 つであれば、Stanchion の設定を変更する必要はありません。Stanchion はローカルホストで動いているからです。システムに複数のノードがあるなら、Stanchion ノード用に IP アドレスとポート、さらに SSL を有効にするかどうかを設定します。

Stanchion 設定は、/etc/Riak-CS ディレクトリ内の app.config ファイルにあります。設定は、このファイルの Riak CS の設定セクションにあります。

__stanchion_ip__: 127.0.0.1 から、Stanchion ノードの IP アドレスに書き換えます。

別なポートを使うように Stanchion を設定したければ、以下のポート設定を変更しなければなりません:

__stanchion_port__: 8085 から、`app.config` ファイルの `stanchion_port` 変数にあるポート番号に書き換えます。

__stanchion_ssl__ 変数は、デフォルトでは false になっています。SSL を使いたいのならば、この変数を true に変更してください。

## ノードの IP アドレスを指定する
Riak CS のノードの IP アドレスを設定することができます。コードをデバッグするとき、どのようなリクエストから派生したノードなのかを識別したい時に便利です。IP アドレスの設定は、`/etc/riak-cs` ディレクトリ内の `vm.args` 設定ファイルで行います。

初期状態では、Riak CS のノード IP アドレスは、次のようにローカルホストとして設定されています。

```
## Name of the riak node
-name riak_cs@127.0.0.1
```

127.0.0.1 を Riak CS のノードの IP アドレスに置き換えます。

## SSL を有効にする
`app.config` ファイルの以下の行のコメントを外します:

```erlang
%%{ssl, [
%%    {certfile, "./etc/cert.pem"},
%%    {keyfile, "./etc/key.pem"}
%%   ]},
```

ダブルクォート内のテキストを SSL エンクリプションファイルのパスおよびファイル名に置き換えます。

## その他の設定
`app.config` ファイルには、ログファイルを作るか否か、それをどこに保存するのかといった、その他の設定項目があります。デフォルトは通常の使用でよく使われる値になっています。
