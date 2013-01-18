---
title: Mac OS X にインストールする
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, osx]
prev: "[[RHEL および CentOS にインストールする|Installing on RHEL and CentOS]]"
up:   "[[インストールとアップグレード]]"
next: "[[FreeBSD にインストールする|Installing on FreeBSD]]"
download: 
  key: osx
  name: "Mac OS X"
---

Mac OS X 10.5 および 10.6 では以下のステップで動作することが確認されています。ソースまたはコンパイル済みの tarball をダウンロードして、インストールしてください。

## インストール方法
  * コンパイル済み tarball
  * Homebrew
  * ソース

<div class="note"><div class="title">OS X での ulimit</div>OS X で開けるファイルハンドルの数は非常に少なく設定されています。このためバックエンドで使えるファイルハンドルも、動作できますが、ほんのわずかです。制限を除くための詳細は [[オープン ファイルの制限|Open Files Limit]] を参照してください。</div>

## コンパイル済み tarball から
コンパイル済み tarball から Riak を実行するには、プラットフォームに合わせて以下のコマンドを実行してください。

### 64-bit
```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/osx/10.4/riak-1.2.1-osx-x86_64.tar.gz
tar xzvf riak-1.2.1-osx-x86_64.tar.gz
```

### 32-bit
```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/osx/10.4/riak-1.2.1-osx-i386.tar.gz
tar xzvf riak-1.2.1-osx-i386.tar.gz
```

untar したら、riak ディレクトリに cd して、bin/riak start で Riak ノードを起動できます。

### 32-bit
現在は64ビットバイナリのみが存在します。

## Homebrew
<div class="note">Homebrew での Riak レシピはコミュニティがサポートしているものです。このため、常に最新の Riak パッケージが使われているとは限りません。現在のレシピが最新のコードをサポートしているかを確認してください(なお、たとえそうでなくてもがっかりしないでください)。</div>

Homebrew でのインストールは簡単です。

```bash
brew install riak
```

もし Erlang がまだ無ければ、Homebrew はそれをインストールします。

## ソースから
Mac 付属の CD、または [[Apple の開発者用ウェブサイト|http://developper.apple.com/]] から XCode ツールをインストールしなければなりません。

<div class="note">Riak は Clang ではコンパイルできません。デフォルトの C/C++ コンパイラが GCC になっていることを確認してください。</div>

Riak には [[Erlang|http://www.erlang.org/]] R15B01 が必要です。*ノート: 今のところ、Erlang version R15B02 は使わないでください。[riak admin status](https://github.com/basho/riak/issues/227) コマンドでエラーを起こします。*

まだ Erlang がインストールされていなければ、[[Erlang のインストール]] を参照してください。ご心配なく、とても簡単です！

次に、ソース ディストリビューションをダウンロードし、unpack します。

```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/riak-1.2.1.tar.gz
tar zxvf riak-1.2.1.tar.gz
cd riak-1.2.1
make rel
```

ビルド時に "incompatible architecture(アーキテクチャが合いません)" というエラーが出たときは、Erlang のアーキテクチャがシステムのそれ(Snow Leopard 以降は64ビット、その他は32ビット)と合致しているかを確認してください。

## 次のステップは？
次の項目をチェックしてください。

  * [[インストール後|Post Installation Notes]]: インストール後に Riak の状態をチェックする
  * [[The Riak Fast Track]]: ノード3個のクラスタを1つセットアップし、Riak の主要な機能を知るためのガイド
  * [[Basic Cluster Setup]]: ノード1つから、Google よりも巨大なノードにするまでのガイド
