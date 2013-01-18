---
title: RHEL および CentOS にインストールする
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, rhel, centos, linux]
prev: "[[Debian および Ubuntu にインストールする|Installing on Debian and Ubuntu]]"
up:   "[[インストールとアップグレード]]"
next: "[[Mac OS X にインストールする|Installing on Mac OS X]]"
download: 
  key: rhel
  name: "Red Hat or CentOS"
---

CentOS または Redhat に Riak をインストールするときは、ソースまたは私たちが作ったカスタム .rpm パッケージを利用いただけます。

## ノート

* CentOS はデフォルトで SE Linux を有効にしますが、問題が起きたときには SE Linux を無効にする必要があります。
* CentOS 5.2 上では Erlang OTP R15B01 と Riak Enterprise 1.2 は動きませんが、CentOS 5.3 以降では動作します。

## カスタム .rpm パッケージからインストール

### Centos 5 / RHEL 5

yum *(お勧め)* を使ってインストールできます。

```
package=basho-release-5-1.noarch.rpm && \
wget http://yum.basho.com/gpg/$package -O /tmp/$package && \
sudo rpm -ivh /tmp/$package
sudo yum install riak
```

...または、rpm パッケージから手動でインストールすることもできます。

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/rhel/5/riak-1.2.1-1.el5.x86_64.rpm
sudo rpm -Uvh riak-1.2.1-1.el5.x86_64.rpm
```

### Centos 6 / RHEL 6

yum *(お勧め)* を使ってインストールできます。

```
package=basho-release-6-1.noarch.rpm && \
wget http://yum.basho.com/gpg/$package -O /tmp/$package && \
sudo rpm -ivh /tmp/$package
sudo yum install riak
```

...または、rpm パッケージから手動でインストールすることもできます。

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/rhel/6/riak-1.2.1-1.el6.x86_64.rpm
sudo rpm -Uvh riak-1.2.1-1.el6.x86_64.rpm
```

## ソースからインストールする

Riak には [[Erlang|http://www.erlang.org/]] R15B01 が必要です。*ノート: 今のところ、Erlang version R15B02 は使わないでください。[riak-admin status](https://github.com/basho/riak/issues/227) コマンドでエラーを起こします。*

まだ Erlang がインストールされていなければ、[[Erlang のインストール|Installing Erlang]] を参照してください。ご心配なく、とても簡単です！

ソースのビルドには以下のパッケージが必要です:

* gcc
* gcc-c++
* glibc-devel
* make

これらは yum でインストールできます:

```bash
sudo yum install gcc gcc-c++ glibc-devel make git
```

Riak をダウンロードして、インストールします。

```bash
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/riak-1.2.1.tar.gz
tar zxvf riak-1.2.1.tar.gz
cd riak-1.2.1
make rel
```

これで `rel/riak` ディレクトリに新しい Riak ビルドが出来ているはずです。

## 次のステップは？

次の項目をチェックしてください。

* [[インストール後|Post Installation Notes]]: インストール後に Riak の状態をチェックする
* [[The Riak Fast Track]]: ノード3個のクラスタを1つセットアップし、Riak の主要な機能を知るためのガイド
* [[Basic Cluster Setup]]: ノード1つから、Google よりも巨大なノードにするまでのガイド
