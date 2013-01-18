---
title: FreeBSD にインストールする
project: riak
version: 1.2.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, freebsd]
prev: "[[Mac OS X にインストールする|Installing on Mac OS X]]"
up:   "[[インストールとアップグレード]]"
next: "[[SmartOS にインストールする|Installing on SmartOS]]"
download: 
  key: freebsd
  name: "FreeBSD"
---

AMD64 アーキテクチャの FreeBSD へバイナリパッケージから、あるいはソースコードからビルドして、Riak をインストールすることができます。

## バイナリパッケージからのインストール

<div class="info"><div class="title">Note</div>Riak 1.2 のバイナリパッケージは、FreeBSD version 9 をサポートしています。なお、ユーザからの報告によると、いくつかのバージョンの FreeBSD でも、Riak をソースからビルドするのに成功しているとのことです。</div>

Riak をバイナリパッケージからインストールするのは、もっとも簡単な方法であり、依存関係も少なく、ソースからビルドするよりも素早く終わらせることができます。

### 事前に必要なもの、および依存関係

*riak* 以外のユーザで Riak コマンドラインツールを実行するためには、`sudo` でインストールする必要があります。Riak  パッケージをインストールする前に、パッケージまたは ports コレクションとして `sudo` がインストールされていることを確認してください。

Riak バイナリパッケージは、OpenSSL のパッケージバージョンにも依存しています。FreeBSD 9 に Riak 1.2 をインストールする前に、`openssl-1.0.0_7` をパッケージまたは ports コレクションからインストールする必要があります。

### インストール

Riak バイナリパッケージは `pkg_add` リモートオプションを使って、リモートからインストールすることができます。この例では `riak-1.2.1-FreeBSD-amd64.tbz` をインストールします。

```bash
sudo pkg_add -r http://s3.amazonaws.com/downloads.basho.com/riak/1.2/1.2.1/freebsd/9/riak-1.2.1-FreeBSD-amd64.tbz
```

Riak がインストールできたら、インストール状況とドキュメントについての情報が表示されます。

```text
Thank you for installing Riak.

Riak has been installed in /usr/local owned by user:group riak:riak

The primary directories are:

    {platform_bin_dir, "/usr/local/sbin"}
    {platform_data_dir, "/var/db/riak"}
    {platform_etc_dir, "/usr/local/etc/riak"}
    {platform_lib_dir, "/usr/local/lib/riak"}
    {platform_log_dir, "/var/log/riak"}

These can be configured and changed in the platform_etc_dir/app.config.

Add /usr/local/sbin to your path to run the riak, riak-admin, and search-cmd
scripts directly.

Man pages are available for riak(1), riak-admin(1), and search-cmd(1)
```

もしもこのメッセージではなく、インストール中に、次のような OpenSSL に関するエラーが出たときは、

```text
Package dependency openssl-1.0.0_7 for /tmp/riak-1.2.1-FreeBSD-amd64.tbz not found!
```

**事前に必要なもの、および依存関係** セクションで説明したとおりに、パッケージまたは ports コレクションから、必要な OpenSSL バージョンをインストールしているかを確認してください。

## ソースからインストールする

Riak をソースから FreeBSD にインストールするのは、ビルドに必要な依存関係 (Erlang など) が確実で、けれどもバイナリパッケージからのインストールより時間がかかる方法です。

ソースからインストールするというのは、設定に関して大きな柔軟性があり、データルートの位置、バージョン依存関係に基づいた細かいコントロールが可能だということです。

### 事前に必要なもの、および依存関係

ソースから Riak をビルドし、インストールするときは、ビルドを行う前に事前に必要なソフトウェアをインストールしなければなりません。

現在、以下のソフトウェアがインストールされていなければ、あらかじめパッケージまたは ports コレクションからインストールしておいてください。

* Erlang ([[Erlang のインストール|Installing Erlang]] で説明したように、Kerl 経由でインストールすることもできます。
* Curl
* Git
* OpenSSL (version 1.0.0_7)
* Python
* sudo

### インストール
初めに、インストールしたいバージョンを [Basho downloads](http://basho.com/resources/downloads/) からダウンロードします。

次に、unpack し、ビルドを行います。

```bash
tar zxf <riak-x.x.x>
cd riak-x.x.x
gmake rel
```

ビルドが完了すると `rel/riak` ディレクトリができ、完全な Riak ノード環境、設定、データ、ログディレクトリが含まれています。

```text
bin               # Riak バイナリ
data              # Riak データおよびメタデータ
erts-5.9.2        # Erlang ランタイムシステム
etc               # Riak 設定
lib               # サードパーティ ライブラリ
log               # オペレーション ログ
releases          # リリース情報
```

1台のマシン上に1つのクラスタとして動ける、4ノードの開発環境を作りたいのであれば、`rel` ターゲットの代わりに `devrel` ターゲットを指定します。

```bash
gmake devrel
```

## 次のステップは？
次の項目をチェックしてください。

* [[インストール後のメモ|Post Installation]]: インストール後に Riak の状態をチェックする
* [[The Riak Fast Track]]: ノード3個のクラスタを1つセットアップし、Riak の主要な機能を知るためのガイド
* [[Basic Cluster Setup]]: ノード1つから、Google よりも巨大なノードにするまでのガイド

## 参照

* [Basho ダウンロード](http://basho.com/resources/downloads/)
* [[Riak コマンドラインツール|Command-Line-Tools]]
* [[インストールとアップグレード]]
* [[Erlang のインストール|Installing Erlang]]
* [FreeBSD パッケージシステムを使う](http://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/packages-using.html)
* [FreeBSD の Ports コレクションを使う](http://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/ports-using.html)
