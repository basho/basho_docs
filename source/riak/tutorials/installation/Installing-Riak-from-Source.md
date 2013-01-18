---
title: Riak をソースからインストールする
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, suse]
prev: "[[AWS Marketplace にインストールする|Installing on AWS Marketplace]]"
up:   "[[インストールとアップグレード]]"
next: "[[インストール後|Post Installation]]"
download: 
  key: source
  name: "any OS in Source Form"
---

パッケージが用意されていないプラットフォームへインストールしたい時、あるいは Riak のプロジェクトに何らかの寄与をしたいときなどは、ソースから Riak をインストールすることできます。

## 依存関係
Riak には [Erlang](http://www.erlang.org/) R15B01 が必要です。*ノート: 今のところ、Erlang version R15B02 は使わないでください。[riak-admin status](https://github.com/basho/riak/issues/227) コマンドでエラーを起こします。*

まだ Erlang がインストールされていなければ、[[Erlang のインストール|Installing Erlang]] を参照してください。ご心配なく、とても簡単です！

<div class='note'>Riak は Clang ではコンパイルできません。デフォルトの C/C++ コンパイラが GCC になっていることを確認してください。</div>

## インストール
以下の手順は完全な、自己内蔵型の Riak ビルドを `$RIAK/rel/riak` に作ります。`$RIAK` は、ソースを unpack した、あるいはコピーした場所です。

### ソースパッケージからのインストール
Riak のソースパッケージを [[Download Center|http://basho.com/resources/downloads/]] からダウンロードし、ビルドします。

{{#1.2.0}}

```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.0/riak-1.2.0.tar.gz
tar zxvf riak-1.2.0.tar.gz
cd riak-1.2.0
make rel
```

{{/1.2.0}}
{{#1.2.1}}

```bash
curl -O http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/riak-1.2.1.tar.gz
tar zxvf riak-1.2.1.tar.gz
cd riak-1.2.1
make rel
```

{{/1.2.1}}

<div class='note'> `fatal: unable to connect to github.com` というエラーが出たときは、下記インターネットにアクセスできないシステムでのビルドについてを参照してください。 </div>

### 閉じたネットワークでのインストール
ソースからのビルド時の `fatal: unable to connect to github.com` というエラーは、Github へアクセスできないときに起こります。セキュリティ上の理由からポートが閉じられている、あるいはインターネットアクセスができないコンピュータ上でソースをビルドしている、のいずれかでしょう。この問題を解決するには、ソースの tarball にファイルを1つ追加する必要があります。
The error `fatal: unable to connect to github.com` when building from source is caused by building on a system with no network connection to Github. Either the port is turned off for security reasons, or the source build is happening on a computer with no outside internet access.  To rectify this problem, an additional file will need to be deployed along with the source tarball.

Riak のバージョンによって、以下の `leveldb` をダウンロードしてください:

  * **1.2.1**: `https://github.com/basho/leveldb/zipball/1.2.2p5`
  * **1.2.0**: `https://github.com/basho/leveldb/zipball/2aebdd9173a7840f9307e30146ac95f49fbe8e64`
  * **1.1.4**: `https://github.com/basho/leveldb/zipball/14478f170bbe3d13bc0119d41b70e112b3925453`

以下の説明では Riak 1.2.0 の使用を想定しています。バージョンに合わせて読み替えてください。

ビルドエラーが起きたシステムにファイルを追加したら、以下のコマンドを実行します。

```bash
$ mv 2aebdd9173a7840f9307e30146ac95f49fbe8e64 riak-1.2.0/deps/eleveldb/c_src/leveldb.zip
$ cd riak-1.2.0/deps/eleveldb/c_src/
$ unzip leveldb.zip
$ mv basho-leveldb-* leveldb
$ cd ../../../
$ make rel
```

### GitHub からインストール
[[Riak Github repository|http://github.com/basho/riak]] には、Riak をソースからビルドし、インストールするためのより詳しい情報が含まれています。ソースから Riak を clone し、ビルドするには、以下のステップに従ってください:

[[Git|http://git-scm.com/]] を使ってレポジトリを clone し、ビルドします:

```bash
git clone git://github.com/basho/riak.git
cd riak
make rel
```

## プラットフォーム依存の説明
プラットフォームに合わせた説明を参照してください:

  * [[Debian および Ubuntu にインストールする|Installing on Debian and Ubuntu]]
  * [[Mac OS X にインストールする|Installing on Mac OS X]]
  * [[RHEL および CentOS にインストールする|Installing on RHEL and CentOS]]
  * [[SUSE にインストールする|Installing on SUSE]]

上記のリスト以外のプラットフォームで Riak を走らせており、何かヘルプが必要な場合は、Riak メーリングリストに参加の上、質問してください。Riak についてお手伝いできることは私たちの喜びです。

### Windows
現在、Riak は Microsoft Windows をサポートしていません。

## 次のステップは？
次の項目をチェックしてください。

* [[インストール後のメモ|Post Installation]]: インストール後に Riak の状態をチェックする
* [[The Riak Fast Track]]: ノード3個のクラスタをセットアップし、Riak の主要な機能を知るためのガイド
* [[Basic Cluster Setup]]: ノード1つから、Google よりも巨大なノードにするまでのガイド
