---
title: インストール後のメモ
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, upgrading]
prev: "[[Riak をソースからインストールする|Installing Riak From Source]]"
up:   "[[インストールとアップグレード]]"
---

Riak をインストールしたら、次にやりたいことは各ノードの生死と確認と、リクエストが確実に届いているかです。

以下は Riak ノードが正常に動作しているかどうかを確認する一般的な方法です。ノードが動いていることを確認して、実際に運用開始する前に、
以下の **それからどうするの？** セクションのリソースをチェックしてください。

## Riak ノードを開始する

<div class="note"><div class="title">ソースからインストールした時のメモ</div>
<p>ソースコードからコンパイルした Riak ノードを開始するために、
インストールディレクトリから Riak バイナリディレクトリを PATH に追加することができます。</p>
<p>例えば、Riak をソースから `/home/riak` というディレクトリでコンパイルしたとします。
この時は PATH にバイナリディレクトリ(`/home/riak/rel/riak/bin`)を追加します。
これによってパッケージからインストールした時と同様に Riak コマンドを実行できるようになります。</p></div>

Riak を起動するには `riak start` コマンドを使います:

```bash
riak start
```

正常に起動した時には何も表示されません。
ノードスタート時に何らかの問題があれば、標準エラーにエラーメッセージが出力されます。

付属の Erlang コンソールで Riak を走らせるときは:

```bash
riak console
```

通常 Riak ノードは、デバッグのため、
あるいはトラブルシューティングのためにスタートアップシーケンスよりも詳細な情報を得るために、コンソールモードで開始します。
なお、Riak をこの方法で起動したときは、
コンソールを閉じるまで存在する、フォアグラウンドプロセスとなることに気をつけてください。

コンソールを閉じるには、Erlang プロンプトからこのコマンドを発行します:

```erlang
q().
```

ノードが起動したら、`riak ping` コマンドで起動を確認することができます。

```bash
riak ping
pong
```

ノードが動いていれば、このコマンドにより **pong** というレスポンスが返ります。
何らかの理由でノードに到達出来なかったときは **pang** になります。

<div class="note"><div class="title">オープンファイルの制限</div>
オープンファイルの制限(`ulimit -n`)を調整しないと、Riak の起動時に警告が出ることに気をつけてください。
Riak を走らせるときは、オペレーティングシステムのデフォルトのオープンファイル数を増やしておくことをお勧めします。
[[オープンファイルの制限|Open Files Limit] を参照してください。</div>

## 正しく動いているか？

個々の Riak ノードの準備ができたか、そしてデータを読み書きできるかをテストする簡単な方法は、
`riak-admin test` コマンドの実行です:

```bash
riak-admin test
```

正常なときは `riak-admin test` によってこのように出力されるはずです:

```text
Attempting to restart script through sudo -H -u riak
Successfully completed 1 read/write cycle to 'riak@127.0.0.1'
```

`curl` コマンドラインツールで Riak が動いているかどうかをテストすることもできます。
ノード上で Riak が動いていたら、このコマンドで `test` バケットとそのプロパティを取得
してください。

```bash
curl -v http://127.0.0.1:8098/riak/test
```

上記の例で `127.0.0.1` はあなたの Riak ノードの IP アドレスまたは完全なドメイン名に置き換えてください。
このコマンドによって、次のようなレスポンスが返るはずです。

```text
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /riak/test HTTP/1.1
> User-Agent: curl/7.21.6 (x86_64-pc-linux-gnu)
> Host: 127.0.0.1:8098
> Accept: */*
>
< HTTP/1.1 200 OK
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
< Date: Wed, 26 Dec 2012 15:50:20 GMT
< Content-Type: application/json
< Content-Length: 422
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
{"props":{"name":"test","allow_mult":false,"basic_quorum":false,
 "big_vclock":50,"chash_keyfun":{"mod":"riak_core_util",
 "fun":"chash_std_keyfun"},"dw":"quorum","last_write_wins":false,
 "linkfun":{"mod":"riak_kv_wm_link_walker","fun":"mapreduce_linkfun"},
 "n_val":3,"notfound_ok":true,"old_vclock":86400,"postcommit":[],"pr":0,
 "precommit":[],"pw":0,"r":"quorum","rw":"quorum","small_vclock":50,
 "w":"quorum","young_vclock":20}}
```

上記は、成功レスポンス (HTTP 200 OK) と、verbose オプションによる追加の詳細情報が出力されています。
また、Riak の test バケットのプロパティも含まれています。

## これからどうするの？

Riak ノードはすでに動いています。

この後は、以下のリソースをチェックしてください。

* [[The Riak Fast Track]]: ノード3個のクラスタをセットアップし、Riak の主要な機能を知るためのガイド
* お好みのプログラミング言語で Riak を使用するためには [[クライアントライブラリ|Client Libraries]] を参照
* [[サンプルデータ|Sample Data]] を得る
* Riak クラスタに [[ノードを追加する|Basic Cluster Setup]]
* [[Riak のコンセプトについての高度な説明|Concepts]]
* [[毎日のシステムオペレーションについて|Operators]]
