---
title: SUSE にインストールする
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, suse]
prev: "[[SmartOS にインストールする|Installing on SmartOS]]"
up:   "[[インストールとアップグレード]]"
next: "[[Windows Azure にインストールする|Installing on Windows Azure]]"
---

SuSE で Riak を動かすには以下のステップに従ってください。

Riak は、以下の x86/x86_64 SuSE 環境へは、コミュニティサポート経由で、非公式にインストールすることができます。

* SLES11-SP1
* OpenSUSE 11.2
* OpenSUSE 11.3
* OpenSUSE 11.4

Riak パッケージとその依存関係 (基本 Erlang を含む) は OpenSUSE Build Service (http://build.opensuse.org) の Zypper リポジトリにあります。

(以下のコマンドでは root として実行することを仮定しています)

## Riak zypper リポジトリを追加する

```bash
$ zypper ar http://download.opensuse.org/repositories/server:/database/$distro Riak
```
$distro は以下のいずれかです:
* SLE_11_SP1
* openSUSE_11.2
* openSUSE_11.3
* openSUSE_11.4

_ノート: システムにリポジトリを追加して、始めてそれを使うとき、GPG キーの認証が要求されます。_

## Riak パッケージをインストール

```bash
$ zypper in riak
```

これによって、まだインストールされていなければ Erlang も含めて、自動的に依存関係が解決されます。

## (オプション) アップデートを受け取れるように、riak レポジトリの "refresh" を有効にする

```bash
$ zypper mr -r Riak
```

## 次のステップは？

次の項目をチェックしてください。

* [[インストール後のメモ|Post Installation]]: インストール後に Riak の状態をチェックする
* [[Riak Fast Track|The-Riak-Fast-Track]]: 4ノードクラスタをセットアップし、Riak の主要な機能を知るガイド
* [[Basic Cluster Setup|Basic Cluster Setup]]: ノード1つから、Google よりも巨大なノードにするまでのガイド

