---
title: Riak CS Quick Start Guide
project: riakcs
version: 1.2.0+
document: tutorial
toc: true
index: true
audience: beginner
keywords: [installing]
---

以下に Riak CS をテスト環境にインストールする際のガイドを示します。このガイドではシステム / サービスのチューニングについて説明しません。また、
特定の環境に対してのインストールの最適化も行いません。

## 1番目のノードをインストール
**ステップ １: システムのファイルオープンの上限を上げる**

Riak は通常のオペレーション中に多数のファイルハンドルをオープンします。システムのオープンファイル数を増やす方法の詳細は [[オープンファイルの制限|Open-Files-Limit]] ドキュメントを参照してください。

あなたがルートユーザであれば、次のコマンドで *現在のセッション* に対して、システムのオープンファイル数を増やすことができます。

    ulimit -n 65536

ルートおよび riak ユーザのために、この設定を恒久的にしておきたいときは `/etc/security/limits.conf` に保存する必要があります。

    # Riak CS のための ulimit 設定
    root soft nofile 65536
    root hard nofile 65536
    riak soft nofile 65536
    riak hard nofile 65536

**ステップ ２: パッケージをダウンロードし、インストール**

このガイドではパッケージのダウンロード、および Riak CS API との対話に `curl` を使用しますので、確実にインストールしておきましょう。

    sudo apt-get install -y curl

Ubuntu 11.10 以降を使用しているのであれば、`libssl0.9.8` パッケージも必要です。詳しいことは [[Debian および Ubuntu でのインストール|Installing-on-Debian-and-Ubuntu]] を参照してください。

    sudo apt-get install -y libssl0.9.8

さあ、Riak と Riak CS パッケージを取りましょう。これが最初のノードになるので、Stanchion パッケージもインストールしましょう。

ライセンス版の Riak CS カスタマは、Basho のヘルプデスク ウェブサイトの [downloads](https://help.basho.com/forums/20747106-riak-cs-downloads) セクションから、Basho が提供している証明書を Riak CS に適用することができます。

Riak EE、Stanchion、Riak CS をダウンロードしたら、オペレーティングシステムのパッケージ管理コマンドを使ってそれらをインストールしてください。

最初に Riak EE をインストールします:

**RHEL6**:

    rpm -Uvh <riak-ee-package.rpm>

`<riak-ee-package.rpm>` を、インストールするパッケージの実際のファイル名に置き換えてください。

**Ubuntu Lucid**:

    sudo dpkg -i <riak-ee-package.deb>

`<riak-ee-package.deb>` を、インストールするパッケージの実際のファイル名に置き換えてください。

次に、Riak CS をインストールします:

RHEL6:

    rpm -Uvh <riak-cs-package.rpm>

`<riak-cs-package.rpm>` を、インストールするパッケージの実際のファイル名に置き換えてください。

Ubuntu Lucid:

    sudo dpkg -i <riak-cs-package.deb>

`<riak-cs-package.deb>` を、インストールするパッケージの実際のファイル名に置き換えてください。

最後に Stanchion をインストールします:

RHEL 6:

    sudo rpm -Uvh <stanchion-package.rpm>

`<stanchion-package.rpm>` を、インストールするパッケージの実際のファイル名に置き換えてください。

Ubuntu Lucid:

    sudo dpkg -i <stanchion-package.deb>

`<stanchion-package.deb>` を、インストールするパッケージの実際のファイル名に置き換えてください。

**ステップ ３: サービスの設定を行い、サービスとして起動する**

Riak 設定のためにいくつかすることがあります。`/etc/riak/app.config` を編集します。
まず、`riak_core` セクションに次の行を追加する必要があります。
セクションはこのように始まっています。

```erlang
{riak_core, [
```

このセクションに、このような行を追加します。

```erlang
{default_bucket_props, [{allow_mult, true}]},
```

次に、Riak は Bitcask がデフォルトのバックエンドとして設定されています。
これを Riak CS のカスタム バックエンドに変える必要があります。

`/etc/riak/app.config` の次の行を変更します。

    {storage_backend, riak_kv_bitcask_backend}

このようにします。

    {add_paths, ["/usr/lib64/riak-cs/lib/riak_cs-1.2.2/ebin"]},
    {storage_backend, riak_cs_kv_multi_backend},
    {multi_backend_prefix_list, [{<<"0b:">>, be_blocks}]},
    {multi_backend_default, be_default},
    {multi_backend, [
        {be_default, riak_kv_eleveldb_backend, [
            {max_open_files, 50},
            {data_root, "/var/lib/riak/leveldb"}
        ]},
        {be_blocks, riak_kv_bitcask_backend, [
            {data_root, "/var/lib/riak/bitcask"}
        ]}
    ]},

app.config ファイルにインタフェース IP アドレスを設定します。プロダクション環境では、複数の NIC を使っていることでしょうが、今はテストクラスタだけなので、10.0.2.10 という IP アドレスの NIC が1台だけだと仮定します。

`/etc/riak/app.config` の次の行を変更します。

    {http, [ {"127.0.0.1", 8098 } ]}
    {pb_ip,   "127.0.0.1" }

このようにします。

	{http, [ {"10.0.2.10", 8098 } ]}
    {pb_ip,   "10.0.2.10" }


`/etc/riak-cs/app.config` の次の行を変更します。

    {cs_ip, "127.0.0.1"}
    {riak_ip, "127.0.0.1"}
    {stanchion_ip, "127.0.0.1"}

このようにします。

    {cs_ip, "10.0.2.10"}
    {riak_ip, "10.0.2.10"}
    {stanchion_ip, "10.0.2.10"}


Riak CS に全てのインタフェースをリッスンさせたいのであれば、cs_ip を 0.0.0.0 にしても構いません。


`/etc/stanchion/app.config` の次の行を変更します。

    {stanchion_ip, "127.0.0.1"}
    {riak_ip, "127.0.0.1"}

このようにします。

    {stanchion_ip, "10.0.2.10"}
    {riak_ip, "10.0.2.10"}


次は、サービス名を設定します。ローカル　IP アドレスを使ってもいいし、ホスト名を使っても構いません。ホスト名を使うのであれば、DNS あるいはすべてのノードの `/etc/hosts` でホスト名が解決するようにしておいてください。

<div class="note"><div class="title">ノート</div>サービス名には最低限 1 つのピリオドが必要です。</div>

`/etc/riak/vm.args` の次の行を変更します。

    -name riak@127.0.0.1

このようにします。

    -name riak@10.0.2.10


`/etc/riak-cs/vm.args` の次の行を変更します。

    -name riak-cs@127.0.0.1

このようにします。

    -name riak-cs@10.0.2.10


`/etc/stanchion/vm.args` の次の行を変更します。

    -name stanchion@127.0.0.1

このようにします。

    -name stanchion@10.0.2.10


以上がノードを起動するために必要な最低限のサービス設定です。サービスを起動するためには、次のようにタイプします。

    sudo riak start
    sudo stanchion start
    sudo riak-cs start

<div class="info"><div class="title">Basho Tip</div>それぞれが前のものに依存しているので、どの順でサービスを起動するかは重要です。</div>

最後に、`riak-cs ping` コマンドを使って、インストールした Riak CS が動いているかをチェックします。Riak CS が起動していて、正常に通信ができれば `pong` を返します。

```bash
riak-cs ping
```

<div class="note"><div class="title">ノート</div>Riak CS が Riak ノードと通信できないときに、<tt>riak-cs ping</tt> コマンドは失敗します。<tt>riak-cs ping</tt> を使って生きているかをチェックする前に、Riak CS システムのすべてのコンポーネントを確認して下さい。</div>

**ステップ ４: 管理ユーザを作る**

管理ユーザの作成は任意ですが、新しいサービスをテストするのに良い機会です。Riak CS のユーザを作成するには、2つの入力が要ります。

1. Name - URL エンコードされた文字列。例えば "admin%20user"

2. Email - ユニークな email アドレス。例えば "admin@admin.com"

管理ユーザは次のようにして `curl` コマンドで作成できます:

管理ユーザを作成するために、新しいユーザを "anonymous" ユーザとして作成し、パーミッションを与えます。
この設定には Riak CS のノードが 1 つだけ必要です。

`/etc/riak-cs/app.config` の `{cs_ip, ...}` のすぐ前に次のエントリを追加します。

    {anonymous_user_creation, true},

それから、新しい設定を有効にするために `riak-cs restart` を実行します。

`anonymous_user_creation` オプションが有効になっているのと同じ Riak CS マシン上に、
管理ユーザを、次のような `curl` コマンドを使って作成します。


```bash
curl -H 'Content-Type: application/json' \
  -X POST http://localhost:8080/riak-cs/user \
  --data '{"email":"admin@admin.com", "name":"admin"}'
```

このコマンドの出力結果は、次のような JSON オブジェクトとなります。

```bash
{"email":"admin@admin.com","display_name":"admin","name":"admin user","key_id":"5N2STDSXNV-US8BWF1TH","key_secret":"RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw==","id":"4b823566a2db0b7f50f59ad5e43119054fecf3ea47a5052d3c575ac8f990eda7"}
```

ユーザのアクセスキーおよび秘密キーは、それぞれ `key_id` と `key_secret` に返ります。

ここではキーはこのようになっています。

    Access key: 5N2STDSXNV-US8BWF1TH
    Secret key: RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw==

同じ操作を使って、Riak CS のユーザを追加することができます。このユーザを管理ユーザにするために、Riak CS および `app.config` ファイルに次のキーを送ります。

<div class="note"><div class="title">ノート</div>クラスタ内のすべてのノードに、同じ管理キーをセットする必要があります。</div>

すべての Riak CS マシンの `/etc/riak-cs/app.config` の、以下の行を変更します:

    {admin_key, "admin-key"}
    {admin_secret, "admin-secret"}

このようにします。

    {admin_key, "5N2STDSXNV-US8BWF1TH"}
    {admin_secret, "RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw=="}

<div class="note"><div class="title">ノート</div>`anonymous_user_creation` 設定を削除することを忘れてはいけません。</div>

`/etc/stanchion/app.config` の以下の行を変更します。

    {admin_key, "admin-key"}
    {admin_secret, "admin-secret"}

このようにします。

    {admin_key, "5N2STDSXNV-US8BWF1TH"}
    {admin_secret, "RF7WD0b3RjfMK2cTaPfLkpZGbPDaeALDtqHeMw=="}


変更結果を有効にするために、サービスをリスタートする必要があります:

    sudo stanchion restart
    sudo riak-cs restart

**ステップ ５: 確認テスト**

インストール結果をテストする最も簡単な方法は、`s3cmd` スクリプトを使うことです。次のようにしてスクリプトをインストールします。

    sudo apt-get -y install s3cmd

`s3cmd` が S3 にではなく、Riak CS サーバを使うように設定する必要があります。対話的にそれを行うには、次のようにタイプします。

    s3cmd --configure

次の4つのデフォルト値を変更してください:

* Access Key - Riak CS のユーザアクセスキーを使うようにする
* Secret Key - Riak CS のユーザ秘密キーを使うようにする
* Proxy Server - Riak CS の IP アドレスを使うようにする。例: 10.0.2.10
* Proxy Port - デフォルトの Riak CS ポートは 8080

`s3cmd` が設定されたら、テスト用のバケットを作成できるようになります:

    s3cmd mb s3://test-bucket

作成できたかどうかは、次のようにタイプします:

    s3cmd ls

これでテストファイルをバケットにアップロードできます:

    dd if=/dev/zero of=test_file bs=1M count=2 # Create a test file
    s3cmd put test_file s3://test-bucket

アップロードできたかどうかは、次のようにタイプします:

    s3cmd ls s3://test-bucket

テストファイルをダウンロードします:

    rm test_file # remove the local test file
    s3cmd get s3://test-bucket/test_file


## 追加のノードをインストールする
2 つの相違点を除いて、追加ノードのインストールは、最初のノードのときと同じです。

1. 最初のノードには Stanchion がインストールされている必要がありますが、他のノードにはこれをインストールし直す必要はありません。
Riak CS の `app.config` ファイルの `stanchion_ip` には、最初のノードの `stanchion_ip` をセットしておきます。
2. Riak クラスタにノードを追加するには、以下のコマンドを使います:

        sudo riak-admin cluster join riak@10.0.2.10

    ここで `riak@10.0.2.10` は Riak の最初のノードの `/etc/riak/vm.args` ファイルで設定したノードの名前です。

この後は、`riak-admin cluster plan` コマンドとでクラスタのプランを見直し、`riak-admin cluster commit` でクラスタの変更をコミットし、ジョイン プロセスを完了してください。詳しいことは [[コマンドライン ツール|Command-Line-Tools---riak-admin#cluster]] のドキュメントにあります。

<div class="note"><div class="title">ノート</div><strong>Riak CS は TCP ポート 80 を直接扱うようには設計されていません。また、直接それを公共のインターネットに公開するような運用をしてはいけません。</strong>その代わりに、Riak CS と外の世界とを、専用のデバイスや、<a href="http://haproxy.1wt.eu">HAProxy</a> や <a href="http://wiki.nginx.org/Main">Nginx</a> といったロードバランサつなぐといった解決を考慮してください。</div>
