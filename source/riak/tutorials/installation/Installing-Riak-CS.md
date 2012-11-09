---
title: Riak CS のインストール
project: riakcs
version: 0.10.0+
document: tutorial
toc: true
index: true
audience: beginner
keywords: [installing]
---

完全に動作する Riak CS システムには、Riak CS、Stanchion、および Riak が含まれています。Riak CS は 64 ビット プラットフォームでのみ動作します。サポートしているオペレーティングシステムは、Ubuntu 10.04、Ubuntu 11.04、CentOS 5、CentOS 6 です。Riak CS は Microsoft Windows には対応していません。Riak CS はシングルノードとしてインストールしても、自動デプロイ ツールを使用しても構いません。

ビデオがお好きであれば、こちらの [[ビデオ|http://player.vimeo.com/video/42654313]] で、Riak CS の一般的なインストールのデモをご覧いただけます。

## Riak CS をノードにインストール
ライセンス版の Riak CS カスタマは、Basho のヘルプデスク ウェブサイトの [downloads](https://help.basho.com/forums/20747106-riak-cs-downloads) セクションから、Basho が提供している証明書を Riak CS に適用することができます。

Riak EE、Stanchion、Riak CS をダウンロードしたら、オペレーティングシステムのパッケージ管理コマンドを使ってそれらをインストールして下さい。

<div class="note"><div class="title">ノート</div><strong>Riak CS は TCP ポート 80 を直接扱うようには設計されていません。また、直接それを公共のインターネットに公開するような運用をしてはいけません。</strong>その代わりに、Riak CS と外の世界とを、専用のデバイスや、<a href="http://haproxy.1wt.eu">HAProxy</a> や <a href="http://wiki.nginx.org/Main">Nginx</a> といったロードバランサつなぐといった解決を考慮してください。</div>

### Ubuntu 上に Riak CS をインストールする
以下のコマンドで、Debian または Ubuntu が走っているマシンに Riak CS をインストールできます。

```bash
sudo dpkg -i <riak-cs-package.deb>
```

`<riak-ee-package.deb>` を、インストールするパッケージの実際のファイル名に置き換えてください。

### CentOS 上に Riak CS をインストールする
以下のコマンドで、CentOS が走っているマシンに Riak CS をインストールできます。

```bash
rpm -Uvh <riak-cs-package.rpm>
```

`<riak-cs-package.rpm>` を、インストールするパッケージの実際のファイル名に置き換えてください。

## Stanchion のインストール
Riak CS システムでは、Stanchion はシステム内の１つのノードにだけインストールします。複数のノードで Stanchion が走っていると、Riak CS のノードが複数の Stanchion ノードと通信するように設定されているときに問題を引き起こす可能性があります。この場合、バケット名およびユーザの email アドレスのユニークさが阻害され、その結果、予期せぬ動作が起きる可能性があります。ご使用のオペレーティングシステムに該当するセクションのコマンドを使って、Stanchion をインストールしたいノードに Stanchion パッケージのバイナリをインストールして下さい。

### Ubuntu 上に Stanchion をインストールする
以下のコマンドで、Ubuntu が走っているマシンに Stanchion をインストールできます。

```bash
sudo dpkg -i <riak-cs-package.deb>
```
`<riak-cs-package.deb>` を、インストールするパッケージの実際のファイル名に置き換えてください。

### CentOS 上に Stanchion をインストールする

以下のコマンドで、Red Had linux または CentOS が走っているマシン上に Stanchion をインストールできます。

```bash
sudo rpm -Uvh <stanchion-package.rpm>
```

`<stanchion-package.rpm>` を、インストールするパッケージの実際のファイル名に置き換えてください。


<div class="note"><div class="title">ノート</div>CentOS はデフォルトで Security-Enhanced Linux (SELinux) が有効になっています。インストール中にエラーが発生したときは、SELinux を無効にしてみてください。</div>

## Riak のインストール
まだ Riak をインストールしたことがなければ、[[Riak Installation|http://wiki.basho.com/Installation.html]] のドキュメントに従ってください。

## 次にすること
Riak CS と Riak のインストールが完了したら、[[Riak CS の設定|Configuration]] を学んでください。
