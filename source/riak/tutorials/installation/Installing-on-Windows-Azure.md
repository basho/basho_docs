---
title: Windows Azure にインストールする
project: riak
version: 1.1.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, windows, azure]
prev: "[[SUSE にインストールする|Installing on SUSE]]"
up:   "[[インストールとアップグレード]]"
next: "[[AWS Marketplace にインストールする|Installing on AWS Marketplace]]"
---

Windows Azure プラットフォーム上の CentOS VM に Riak をインストールするステップです。

## CentOS VM を作る

仮想マシンを作るに当たって、まずは Windows Azure Virtual Machines プレビュー機能にサインアップ擦る必要があります。Windows Azure アカウントを持っていなくても、フリートライアルアカウントでサインアップすることができます。

1. [https://account.windowsazure.com](https://account.windowsazure.com/) へ行き、Windows Azure アカウントでサインインします。

2. "preview features" で、有効なプレビューを表示します。

	![](/images/antares-iaas-preview-01.png)

3. Virtual Machines & Virtual Networks までスクロールし、"try it now" をクリックします。

	![](/images/antares-iaas-preview-02.png)

4. 項目を選択し、チェックマークをクリックします。

	![](/images/antares-iaas-preview-04.png)

### 仮想マシンに CentOS Linux を走らせる

1. Windows Azure (プレビュー) 管理ポータルに、Windows Azure アカウントでログインします。

2. 管理ポータルのページの左下で、""+New"" をクリックし、"Virtual Machine" をクリック、さらに "From Gallery" をクリックします。

	![](/images/createvm_small.png)

3. CentOS 仮想マシンイメージをプラットフォームイメージから選択し、ページの右下の、右矢印をクリックします。 

	![](/images/vmconfiguration0.png)

4. VM Configuration ページで、以下の情報を設定してください:
    - VIRTUAL MACHINE NAME: 仮想マシン名、たとえば "testlinuxvm"
	- NEW USERNAME: 新しいユーザ名、"newuser" など。これは Sudoers のリストファイルに追加されます。
	- NEW PASSWORD: 新しいパスワード。強力なパスワードを入力してください。
	- CONFIRM PASSWORD: パスワードの確認。パスワードを入力しなおします。
	- SIZE: ドロップダウンリストから必要な構成を選択してください。
	- 右矢印をクリックして、続けます。

	![](/images/vmconfiguration1.png)

5. VM Mode ページで、以下の情報を設定してください:
    - **これが最初のノードであれば**、"STANDALONE VIRTUAL MACINE" のラジオボタンをチェックします。**そうでなければ**、"CONNECT TO EXISTING VIRTUAL MACHINE" をチェックし、ドロップダウンリストから最初のノードを選択ぢます。*
	ｰ DNS NAME ボックスには、有効な DNS アドレスを入力します。例: "testlinuxvm"
	- STRAGE ACCOUNT ボックスは、"Use Automatically Generated Storage Account" を選択します。
	- REGION/AFFINITY GROUP/VIRTUAL NETWORK ボックスでは、この仮想イメージがホストされている地域を選びます。
	- 右矢印をクリックして、続けます。

	![](/images/vmconfiguration2.png)

6. VM Options ページでは、AVAILABILITY SET ボックスは "(none)" を選択してください。チェックマークをクリックして、続けます。

	![](/images/vmconfiguration3.png)

7. Windows Azure が仮想マシンを準備するのを待ちます。

### エンドポイントの設定

仮想マシンができたら、リモートからアクセスできるようにエンドポイントの設定をしなければなりません。

1. 管理ポータルで、"Virtual Machines" をクリックし、新しい VM の名前をクリックします。次に "Endpoints" をクリックしてください。

2. **コレが最初のノードであれば**、"Add Endpoint" をチェックし、右矢印を押して、次のフォームを以下のように記入します:
	- Name: riak_web
	- Protocol: leave set to 'TCP'
	- Public Port: 8098
	- private Port: 8098

## PuTTY または SSH で CentOS VM に接続します。

仮想マシンが用意され、エンドポイントが設定されたら、SSH または PuTTY で接続できます。

### SSH で接続

**Linux & Mac ユーザ:**

	$ ssh newuser@testlinuxvm.cloudapp.net -o ServerAliveInterval=180
ユーザのパスワードを入力してください。

**Windows ユーザは、PuTTY を使用:**

Windows マシンを使用しているときは、PuTTY で VM に接続します。PuTTY は、[PuTTY Download Page](http://www.chiark.greenend.org.uk/~sgtatham/putty/download.html) からダウンロードすることができます。

1. putty.exe をパソコンにダウンロードし、保存します。コマンドプロンプトを開き、フォルダを開き、putty.exe を実行します。

2. ノードのダッシュボードで SSH DETAILS を入力します。例: ホスト名として "testlinuxvm.cloudapp.net"、ポート番号として "22"

	![](/images/putty.png)

## シェルスクリプトで CentOS と Riak を設定する

1. 各ノードごとに、上記のステップで接続します:

実行します。

	sudo su -

	curl -s https://raw.github.com/glickbot/riak_on_azure/master/azure_install_riak.sh | sh

**最初のノード** では、ノードダッシュボードの右に表示される "INTERNAL IP ADDRESS" を控えておいてください。  

**その他のノード" では、最初のノードの "INTERNAL IP ADDRESS" を使ってください。

実行します。

	riak-admin cluster join riak@<ip.of.first.node>

## Riak をクラスタ化し、テストデータをロードする

すべてのノードがインストールされ、上記のステップでジョインしたら、SSH または PuTTY でいずれかのノードに接続し、以下を実行します:

	riak-admin cluster plan

問題がなければ、

	riak-admin cluster commit

クラスタのステータスをチェックします:

	riak-admin member_status

これで Azure 上に Riak クラスタができました。

### テストデータをロードする

いずれかのノード上で実行します:

	curl -s http://rekon.basho.com | sh
	
ダッシュボードに表示されている DNS アドレスのポートにアクセスし、エンドポイントを開きます。

	http://testlinuxvm.cloudapp.net:8098/riak/rekon/go

もっと読む:

- [[Basic Riak API Operations]]
