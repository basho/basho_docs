---
title: Erlang のインストール
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, erlang]
prev: "[[インストールとアップグレード]]"
up:   "[[インストールとアップグレード]]"
next: "[[Debian および Ubuntu にインストールする|Installing on Debian and Ubuntu]]"
---

Riak 1.2 および 1.2.1 は [[Erlang|http://erlang.org/]] R15B01 を要求します。*ノート: 今のところ Erlang version R15B02 は使わないでください。[[riak-admin status]](https://github.com/basho/riak/issues/227) コマンドでエラーを起こします。*

Riak 1.0 は [[Erlang|http://erlang.org]] R14B03 以降を要求します。Riak 1.0 以前のバージョンでは R14B02 以降は機能しません。

Riak 0.12 以前のバージョンでは、R14 シリーズの Erlang は機能しません。Erlang をビルドし、インストールするためには、GNU 互換のビルドシステムおよび、開発環境関連の ncurses と openssl が必要です。

<div class="note">
<div class="title">Erlang のバージョンについて</div>
Debian、Ubuntu、Mac OS X、RHEL、CentOS 用の Riak バイナリパッケージには、Erlang ディストリビューションが含まれているので、ソースから Erlang をビルドする必要はありません。しかし、<strong>[[The Riak Fast Track]] を最後まで行いたいならば、Erlang をダウンロードして、インストールしなければなりません。</strong>
</div>

## kerl を使ったインストール

[kerl](https://github.com/spawngrid/kerl) スクリプトを使って、別のバージョンの Erlang を簡単にインストールすることができます。おそらくこれが、Erlang をソースからインストールする最も簡単な方法で、通常はほんのいくつかのコマンドだけでそれが行えます。kerl をインストールするには次のコマンドを実行します。

```bash
curl -O https://raw.github.com/spawngrid/kerl/master/kerl; chmod a+x kerl
```

Mac OS X 上で Erlang を64ビットとしてコンパイルするには、`configure` コマンドを使って kerl に正しいフラグを渡す必要があります。一番簡単な方法は、`~/.kerlrc` ファイルに以下の項目を作ります:

```text
KERL_CONFIGURE_OPTIONS="--disable-hipe --enable-smp-support --enable-threads
                        --enable-kernel-poll  --enable-darwin-64bit"
```

Erlang を FreeBSD/Solaris システム (SmartOS を含む) 上で構築するには、上述の `--disable-hipe` オプションを使って HIPE を無効にするべきです。

GNU/Linux 上で kerl を構築するには、ソースからビルドするのと同じ前提条件があります。

Erlang のリリースを決めるのはコマンド1つです。Riak version 1.2 の時には、Erlang R15B01 をビルドし、使うべきです。

```bash
./kerl build R15B01 r15b01
```

これで Erlang のディストリビューションをビルドし、Erlang をインストールするために手動で行わなければならない全てのステップを実行してくれます。

ビルドに成功したら、次のようにしてそのビルドをインストールします:

```bash
./kerl install r15b01 ~/erlang/r15b01
. ~/erlang/r15b01/activate
```

最後のラインは `/opt/erlang/r15b01` にインストールした Erlang ビルドを有効化するものです。有効なコマンドの詳細は [[kerl readme|https://github.com/spawngrid/kerl]] を参照してください。

Erlang をソースコードからインストールしたい場合は、次のようにします。

## GNU/Linux にインストール
多くの GNU/Linux ディストリビューション用には、最新の Erlang リリースはありません。このため、**ソースからインストールする必要があります。 **

まず、対応するビルドシステムがあり、`ncurses` と `openssl` 開発ライブラリがインストールされているかを確認してください。Debian/Ubuntu ではこのコマンドをつかいます:

```bash
sudo apt-get install build-essential libncurses5-dev openssl libssl-dev
```

RHEL/CentOS ではこのコマンドです:

```bash
sudo yum install gcc glibc-devel make ncurses-devel openssl-devel autoconf
```

次に、Erlang をダウンロードし、ビルドし、インストールします。

```bash
wget http://erlang.org/download/otp_src_R15B01.tar.gz
tar zxvf otp_src_R15B01.tar.gz
cd otp_src_R15B01
./configure && make && sudo make install
```

## Mac OS X にインストールする
OS X に Erlang をインストールする方法はいくつかあります。ソースから、あるいは Homebrew や MacPorts を使うことができます。

### ソース
ソースからビルドするには、Mac に付属の CD あるいは Apple [[開発者ウェブサイト|http://developper.apple.com/]] から XCode ツールをインストールしなければなりません。

まず、ソースをダウンロードし、unpack します。

```bash
curl -O http://erlang.org/download/otp_src_R15B01.tar.gz
tar zxvf otp_src_R15B01.tar.gz
cd otp_src_R15B01
```

次に、Erlang を設定します。

**Mountain Lion (OS X 10.8) および Lion (OS X 10.7)**
Mountain Lion (OS X 10.8) または Lion (OS X 10.7) をお使いでしたら、LLVM (デフォルト) または GCC で Erlang をコンパイルできます。

LLVM を使う:

```text
CFLAGS=-O0 ./configure --disable-hipe --enable-smp-support --enable-threads \
--enable-kernel-poll --enable-darwin-64bit
```

GCC を使いたい:

```text
CC=gcc-4.2 CPPFLAGS='-DNDEBUG' MAKEFLAGS='-j 3' \
./configure --disable-hipe --enable-smp-support --enable-threads \
--enable-kernel-poll --enable-darwin-64bit
```

**Snow Leopard (OS X 10.6)**
Intel プロセサ上で、Snow Leopard (OS X 10.6) または Leopard (OS X 10.5) をお使いでしたら:

```bash
./configure --disable-hipe --enable-smp-support --enable-threads \
--enable-kernel-poll  --enable-darwin-64bit
```

非 Intel プロセサ、あるいは古いバージョンの OS X の時:

```bash
./configure --disable-hipe --enable-smp-support --enable-threads \
--enable-kernel-poll
```

ビルドとインストール:

```bash
make && sudo make install
```

sudo パスワードが要求されるはずです。

### Homebrew
Homebrew を使って Riak をインストールしたいときは、[[Mac OS X インストールドキュメント|Installing-on-Mac-OS-X]] に従ってください。Erlang は自動的にインストールされます。

Homebrew で Erlang を別個にインストールしたいときは、このコマンドを使います:

```bash
brew install erlang
```

### MacPorts
MacPorts でのインストールは簡単です。

```bash
port install erlang +ssl
```
