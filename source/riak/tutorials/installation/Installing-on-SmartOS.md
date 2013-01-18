---
title: SmartOS にインストールする
project: riak
version: 1.1.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, smartos]
prev: "[[FreeBSD にインストールする|Installing on FreeBSD]]"
up:   "[[インストールとアップグレード]]"
next: "[[SUSE にインストールする|Installing on SUSE]]"
download: 
  key: smartos
  name: "SmartOS"
---

Riak 1.2 は以下のステップで、SmartOS version <strong>joyent_20120614T184600Z</strong> 上で動作することが確認されています。Riak ノードをルートユーザとして SmartOS にインストールする方法を示します。

## オープンファイルの制限
インストール作業を行う前に、システムのオープンファイルの制限を、推奨の最小値である **4096**、あるいはそれ以上であることを確実にしてください。現在の制限値はこのようにしてチェックできます:

```bash
ulimit -a
```

この制限を *セッションの間だけ* 一時的に増やすには、次のコマンドを使います:

```bash
ulimit -n 65536
```

この値をシステムリスタート後にも恒久的にしたいときは、`/etc/system` に、以下を追加します:

```text
set rlim_fd_max=65536
```

## ダウンロードとインストール
初めに、最新版の SmartOS 用 Riak バイナリパッケージをダウンロードします。本稿の例では、Riak 1.2 をダウンロードします。

```bash
curl http://s3.amazonaws.com/downloads.basho.com/riak/1.2/1.2.1/smartos/11/riak-1.2.1-SmartOS-i386.tgz
```

次に、パッケージをインストールします。

```
pkg_add riak-1.2.1-SmartOS-i386.tgz
```

パッケージをインストールしたら、Riak と Erlang の Port Mapper Deamon (epmd) サービスを有効にします。

```bash
svcadm -v enable -r riak
```

サービスを有効にしたら、最後にそれらが動いているかを確認します。

```
svcs -a | grep -E 'epmd|riak'
```

上のコマンドの出力は次のようになるはずです。

```text
online    17:17:16 svc:/network/epmd:default
online    17:17:16 svc:/application/riak:default
```

サービスが **online** 状態になっていたら、Riak に ping を打ちます。

```bash
riak ping
```

ノードが生きていて到達可能であれば、ping を打つと、`pong` レスポンスが返ります。`pang` レスポンスは、ノードが生きているけれど問題があるということです。ノードが生きていないけれど到達可能なときは、その代わりに *not responding to pings* エラーを返します。

全てのレスポンスが、Riak は生きていて稼働中だとなれば、Riak が正常にインストールでき、SmartOS のサービスとして設定できたということになります。

次のステップは？
-----------

Riak がインストールできました。次の項目をチェックしてください。

-   [[インストール後のメモ|Post Installation]]: インストール後に Riak の状態をチェックする
-   [[The Riak Fast Track]]: ノード3個のクラスタを1つセットアップし、
    Riak の主要な機能を知るための
    ガイド
-   [[Basic Cluster Setup]]: ノード1つから、Google よりも巨大なノードにするまでのガイド
