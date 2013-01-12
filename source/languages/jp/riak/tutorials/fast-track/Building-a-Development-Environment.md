---
title: 開発環境を構築する
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, fast-track]
prev: "[[Riak とは|What is Riak]]"
up:   "[[The Riak Fast Track]]"
next: "[[Riak の基本 API の操作]]"
versions: false
interest: [
"[[Installing and Upgrading]]",
"[[Open Files Limit]]",
"<a href='http://basho.com/resources/downloads/'>Download Riak</a>",
"<a href='https://github.com/basho/rebar/wiki'>Rebar Documentation</a>"
]
---

このセクションではローカルマシンに Riak をインストールして、4 つのノードのクラスタを作ります。プロダクションの開発に対して Basho は [最低 5 つのノードを推奨しています](http://basho.com/blog/technical/2012/04/27/Why-Your-Riak-Cluster-Should-Have-At-Least-Five-Nodes/)。はっきり言うと、このチュートリアルでは 4 にしています。

## 依存性

Riak をソースからビルドするには、Erlang R15B01 以降が必要です。Basho によるビルド済 Riak パッケージの最新版は、[ダウンロードディレクトリ](http://basho.com/resources/downloads/) にあり、この中には Erlang のランタイムも組み込まれています。とはいえ、このチュートリアルではソースからのビルドを行いますので、Erlang をまだインストールしていない場合は、[[Erlang のインストール|Installing Erlang] を参照してください。

ビデオでの解説をお望みでしたら、こちらに Linux 上で Erlang をソースからインストールした様子のビデオがあります。

<iframe src="http://player.vimeo.com/video/42421349" width="500" height="269" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>

## Riak ソースのダウンロードとインストール

以下のリンクはプラットフォーム別のダウンロードとインストール方法です。

* [[Debian および Ubuntu|Installing on Debian and Ubuntu#Installing Riak From Source]]
* [[RHEL および CentOS|Installing on RHEL and CentOS#Installing From Source]]
* [[Mac OS X|Installing on Mac OS X#From Source]]
* [[SUSE|Installing on SUSE]]
* [[ソースから|Installing Riak from Source]] *(to be used on an unlisted-operating system)*

## Riak をビルドする

Riak をコピーしました。それではビルドしましょう。`riak` ディレクトリに行き、`make all` を実行してください。

```bash
$ cd riak-1.2.1
$ make all
```

`make all` はあなたに代わって Riak の依存関係を調べるので、手間が省けます。これには少々時間がかかります。

## Rebar を使って 4 つのノードを開始する

Riak のビルドが完了しました。Erlang アプリケーションのパッケージおよびビルドシステムである [Rebar](https://github.com/basho/rebar) を使って、4 つの自己内蔵型 Riak ノードを作ります。後日、Riak をプロダクションに移行するとき、Rebar はビルド済の Riak パッケージをデプロイ用マシンに展開してくれます。でも今回は、4つのノードを作るだけにしましょう。以上のことを行うには、次のようにします。

```bash
$ make devrel
```

`dev` ディレクトリができました。そのディレクトリへ移り、中身を確認して下さい。

```bash
$ cd dev; ls
```

次のようになるはずです。

```bash
dev1       dev2       dev3       dev4
```

`dev` で始まる各ディレクトリは、Riak ノードを含む完全なパッケージです。各ノードを動かす必要があります。`dev1` を動かしましょう。

```bash
$ dev1/bin/riak start
```

<div class="note">
<div class="title">ulimit の警告</div>

このとき、オープンファイルハンドル(ulimit)の数を増やすようにという警告メッセージが出るはずです。プラットフォーム別の操作方法について [[オープンファイルの制限|Open Files Limit]] を参照してください。

</div>

続いて `dev2`、`dev3`、`dev4` で同じようにします。

```bash
$ dev2/bin/riak start
$ dev3/bin/riak start
$ dev4/bin/riak start
```

## Riak ノードが動いているかテストする

用意したノードを走らせたら、次はそれらがきちんと動いているかどうかをテストします。それにはプロセスリストを見るだけです。次のようにします。

```bash
$ ps aux | grep beam
```

Riak ノード 4つの詳細が出るはずです。

## ノードをクラスタに join させる

次のステップは、4つのノードを 1つのクラスタに join させることです。これには Riak Admin ツールを使います。具体的には、`dev2`、`dev3`、`dev4`を`dev1`に join させます。

```bash
$ dev2/bin/riak-admin cluster join dev1@127.0.0.1
$ dev3/bin/riak-admin cluster join dev1@127.0.0.1
$ dev4/bin/riak-admin cluster join dev1@127.0.0.1
```

Riak 1.2 以前をご存知なら、これがどんなに大変かわかるはずです。それぞれの動作を一致させるために、ノードの管理者はいろいろと操作しなければなりません。`join`のような Riak Admin のクラスタコマンドは、複数のコマンドを並べたものになります。上記のジョインを行うには、まず`plan`を行わねばなりません。

```bash
$ dev2/bin/riak-admin cluster plan
```

plan は何をするべきかの概要と、実行後のクラスタの状態を表示します。

```bash
=============================== Staged Changes ================================
Action         Nodes(s)
-------------------------------------------------------------------------------
join           'dev2@127.0.0.1'
join           'dev3@127.0.0.1'
join           'dev4@127.0.0.1'
-------------------------------------------------------------------------------


NOTE: Applying these changes will result in 1 cluster transition

###############################################################################
                         After cluster transition 1/1
###############################################################################

================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
valid     100.0%     25.0%    'dev1@127.0.0.1'
valid       0.0%     25.0%    'dev2@127.0.0.1'
valid       0.0%     25.0%    'dev3@127.0.0.1'
valid       0.0%     25.0%    'dev4@127.0.0.1'
-------------------------------------------------------------------------------
Valid:4 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

Transfers resulting from cluster changes: 48
  16 transfers from 'dev1@127.0.0.1' to 'dev4@127.0.0.1'
  16 transfers from 'dev1@127.0.0.1' to 'dev3@127.0.0.1'
  16 transfers from 'dev1@127.0.0.1' to 'dev2@127.0.0.1'
```

最後に、バッチをコミットします。

```bash
$ dev2/bin/riak-admin cluster commit
```

<div class="info">
<div class="title">riak-admin について</div>

riak-admin は Riak の管理ツールです。これはノードのスタートとストップ以外のあらゆる操作、たとえばクラスタを join させたり、leave させたり、データのバックアップ、さらに一般的なクラスタの操作をするのに使われます。詳しくは [[riak-admin|Command Line Tools#riak-admin]] をお読みください。

</div>

## クラスタのテストとデータの追加

これで 4つの Riak クラスタが動きました。次はこれらが正しく働いているか確認をしましょう。これにはいくつかの方法があります。簡単な方法は、member-status コマンドを使うことです。

```bash
$ dev2/bin/riak-admin member-status
```

これによっておおまかなクラスタの様子と、各ノードが管理するリングの割合が表示されます。

```bash
================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
valid      25.0%      --      'dev1@127.0.0.1'
valid      25.0%      --      'dev2@127.0.0.1'
valid      25.0%      --      'dev3@127.0.0.1'
valid      25.0%      --      'dev4@127.0.0.1'
-------------------------------------------------------------------------------
Valid:4 / Leaving:0 / Exiting:0 / Joining:0 / Down:0
```

お望みであれば、Riak クラスタにファイルを追加し、正しく動いていることをテストしてください。たとえば画像を追加し、それがアクセスできるかを確認します。初めに画像をコピーします、まだ用意していなければ。

```bash
$ cp ~/image/location/image_name.jpg .
```

この画像を curl コマンドで Riak に PUT します。

```bash
$ curl -XPUT HTTP://127.0.0.1:8091/riak/images/1.jpg \
  -H "Content-type: image/jpeg" --data-binary @image_name.jpg
```

それから、画像が格納されたかを確認します。これには画像を PUTで したときの URL をコピーして、ブラウザにペーストするだけです。画像が現れるはずです。

これで 4つのノードの Riak クラスタが動いているはずです。おめでとうございます！　それほど難しくなかったですよね？

<div class="note"><div class="title">HTTP インタフェースのポート</div>上記の設定では、HTTP インタフェースがポート `8091-8093` をリッスンするようにノードをセットしています。ノードのデフォルトポートは`8098`です。デフォルト以外を使うのであれば、他の言語クライアントにそれを知らせる必要があります。</div>


参考

* [Rebar ドキュメント](https://github.com/basho/rebar/wiki)
