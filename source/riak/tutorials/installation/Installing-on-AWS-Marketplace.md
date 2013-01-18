---
title: AWS Marketplace にインストールする
project: riak
version: 1.2.1+
document: tutorial
audience: beginner
keywords: [tutorial, installing, AWS, marketplace, amazon]
prev: "[[Windows Azure にインストールする|Installing on Windows Azure]]"
up:   "[[インストールとアップグレード]]"
next: "[[Riak をソースからインストールする|Installing Riak from Source]]"
---

## AWS Marketplace 経由で Riak VM を起動する

AWS Marketplace 経由で Riak 仮想マシンを起動するためには、まず、[Amazon Web Services] (http://aws.amazon.com) アカウントにサインアップする必要があります。

1. [https://aws.amazon.com/marketplace/] (https://aws.amazon.com/marketplace/) に行き、あなたの Amazon Web Services アカウントでサインインします。

2. "Databases &Caching" カテゴリ内の Riak へ進むか、任意のページで Riak を検索します。

3. お望みの AWS 地域、EC2 インスタンスタイプ、ファイアウォールの設定、キーペアを選択します。

	![AWS Marketplace Instance Settings](/images/aws-marketplace-settings.png)

4. "Accept Terms and Launch with 1-Click" ボタンをクリックします。

### セキュリティグループの設定

仮想マシンが作成されたら、選んだ EC2 セキュリティグループが Riak 用に正しく設定されているかを確認します。  

1. AWS EC2 Management Console で、"Security Groups" をクリックし、Riak VM 用のセキュリティグループの名前をクリックします。

2. ペーンの下部にある "Inbound" タブをクリックします。セキュリティグループが、以下のようにオープンポート内にあるはずです。
	- 22 (SSH)
	- 8087 (Riak Protocol Buffers Interface)
	- 8098 (Riak HTTP Interface)

3. Riak インスタンスがコミュニケートできるように、セキュリティグループに追加のルールを設定します。下記の各ポート範囲に、"Custom TCP rule" を新規作成し、ソースセットに現在のグループID ("Details" タブにあります) をセットします。  
	- Port range: 4369
	- Port range: 6000-7999
	- Port range: 8099 

4. 終了したら、下図のようにセキュリティグループにすべてのルールが現れるはずです。ルールが足りなかったら、パネルの下部でそれを追加し、"Apply Rule Changes" ボタンをクリックします。 

	![EC2 Security Group Settings](/images/aws-marketplace-security-group.png)

Riak の [[ネットワークセキュリティとファイアウォールの設定|Network Security and Firewall Configurations]] に詳細があります。

## AWS 上の Riak をクラスタ化

Riak をクラスタ化するために、最低でも3つのインスタンスを起動する必要があります。インスタンスが用意され、セキュリティグループが設定されたら、SSH または PuTTY で、ec2-user としてそれらにアクセスできます。

インスタンスへの接続についての詳しい情報は、オフィシャルな [Amazon EC2 instance guide](http://docs.amazonwebservices.com/AWSEC2/latest/UserGuide/AccessingInstances.html) にあります。

1. 最初のノードにはインターナル IP アドレスを与えます。

	```text
	curl http://169.254.169.254/latest/meta-data/local-ipv4 
	```

2. その他のノードには、最初のノードのインターナル IP アドレスを使います。

	```text
	sudo riak-admin cluster join riak@<ip.of.first.node>
	```

3. 全てのノードがジョインしたら、以下を実行します:

	```text
	sudo riak-admin cluster plan
	```

	うまくいったら、

	```text
	sudo riak-admin cluster commit
	```

	クラスタ化のステータスをチェックするには:

	```text
	sudo riak-admin member_status
	```

これで AWS 上に  Riak クラスタができました。

参照:

- [[Basic Riak API Operations]]
