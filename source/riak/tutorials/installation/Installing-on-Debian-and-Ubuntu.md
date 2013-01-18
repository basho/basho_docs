---
title: Debian および Ubuntu にインストールする
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, debian, ubuntu, linux]
prev: "[[Erlang をインストールする|Installing Erlang]]"
up:   "[[インストールとアップグレード]]"
next: "[[RHEL および CentOS にインストールする|Installing on RHEL and CentOS]]"
download: 
  key: debian
  name: "Debian or Ubuntu"
---

Riak は Debian または Ubuntu ベースのシステムへ、バイナリパッケージまたは [[ソースコードをコンパイル|Installing Riak from Source]] してインストールすることができます。以下のステップは、**Debian version 6.05** および **Ubuntu version 12.04** で Riak が動作することを確認しています。

Apt-Get からインストール
-----------------------

Riak を楽にインストールしたいだけでしたら、`apt-get` を使ってください。

最初に署名キーを用意しなければなりません。

```bash
curl http://apt.basho.com/gpg/basho.apt.key | sudo apt-key add -
```

次に Basho のリポジトリを apt のソース率とに追加します（アップデートもしてください）。

```
sudo bash -c "echo deb http://apt.basho.com lsb_release -sc main > /etc/apt/sources.list.d/basho.list"
sudo apt-get update
```

これで Riak をインストールします。

```bash
sudo apt-get install riak
```

これだけです。

パッケージからのインストール
-----------------------

手動で deb パッケージをインストールしたいときは、以下の手順に従ってください。

### Ubuntu には SSL ライブラリが必要

Riak には現在 libssl version 0.9.8 が必要で、
これは最近の Ubuntu にはデフォルトではインストールされていません。
Ubuntu 上でパッケージから Riak をインストールする前に、`libssl0.9.8` パッケージをインストールしてください。
このバージョンの libssl はすでにインストールされている他のバージョンの libssl と
共存させることができます。

libssl version 0.9.8 パッケージをインストールするには、
次のコマンドを実行します。

```bash
sudo apt-get install libssl0.9.8
```

libssl パッケージをインストールしたら、ターゲットとするプラットフォームにあわせて、
次のコマンドを使ってビルド済みのパッケージから Riak をインストールしてください。

### Riak 64-bit のインストール

#### Ubuntu Lucid Lynx (10.04)

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/ubuntu/lucid/riak_1.2.1-1_amd64.deb
sudo dpkg -i riak_1.2.1-1_amd64.deb
```

#### Ubuntu Natty Narwhal (11.04)

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/ubuntu/natty/riak_1.2.1-1_amd64.deb
sudo dpkg -i riak_1.2.1-1_amd64.deb
```

#### Ubuntu Precise Pangolin (12.04)

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/ubuntu/precise/riak_1.2.1-1_amd64.deb
sudo dpkg -i riak_1.2.1-1_amd64.deb
```

### Riak 32-bit のインストール

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/ubuntu/lucid/riak_1.2.1-1_i386.deb
sudo dpkg -i riak_1.2.1-1_i386.deb
```
<div class="note"><div class="title">Riak のアップグレード</div>Riak パッケージをアップグレードする場合、さらに "riak" という名前のユーザがいるけれどもホームディレクトリが無い場合、Riak を実行する前に `/var/lib/riak` というホームディレクトリを作成し、`chown riak:riak /var/lib/riak` を実行してください。</div>


ソースから Riak をインストールする
---------------------------

はじめに、Riak が依存するものを apt を使ってインストールします。

```bash
sudo apt-get install build-essential libc6-dev-i386 git
```

Riak には [Erlang](http://www.erlang.org/) R15B01 が必要です。*ノート: 今のところ、Erlang version R15B02 は使わないでください。[riak-admin status](https://github.com/basho/riak/issues/227) コマンドでエラーを起こします。*
まだ Erlang がインストールされていなければ、先にインストールしておいてください
 (参照: [[Erlang のインストール|Installing Erlang]])

Erlang をインストールしたら、Riak をダウンロードして、インストールします。

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/riak-1.2.1.tar.gz
tar zxvf riak-1.2.1.tar.gz
cd riak-1.2.1
make rel
```

ビルドが成功すれば、
`rel/riak` ディレクトリに新しい Riak ビルドが出来ているはずです。

次のステップは？
-----------

これで Riak がインストールされました。次のリソースをチェックしてください。

-   [[インストール後のメモ|Post Installation]]: インストール後に Riak の状態をチェックする
-   [[The Riak Fast Track]]: ノード3個のクラスタを1つセットアップし、
    Riak の主要な機能を知るためのガイド
-   [[Basic Cluster Setup]]:
    ノード1つから、Google よりも巨大なノードにするまでのガイド
	