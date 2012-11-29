---
title: Chef を使う
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, installing, chef]
---

Chef を使えば、システム内のすべてのノードに Riak CS を自動的にインストールさせることができます。このレシピにはシステムを構成するリソースが集められ、順番通りに処理されます。

Chef サーバおよびクライアントについての詳細は、以下のリンクの OpsCode Wiki を参照してください。OpsCode には、Chef が利用可能なホストプラットフォームについての情報も掲載されています。

* [[http://wiki.opscode.com]]
* [[http://www.opscode.com/hosted-chef]]

<div class="note">このセクションでは、すでに Chef についてご存知であることを仮定しています。</div>

## Riak CS での使用法Cookbooks for Riak CS

<div class="info">
このセクションでは、Chef cookbooks へアクセスのできることが必要です。まだ cookbooks をお持ち出なければ、[Enterprise downloads](https://help.basho.com/forums/20749257-riak-enterprise-downloads) を参照するか、<a href="http://help.basho.com">サポート</a> にご連絡いただき、ご入手ください。
</div>

このセクションでは Riak CS システムを正しくインストールするために、2 つの Chef Cookbooks の高度な説明を行います。

### Riak EDS Cookbook

**riak::default recipe**

The Riak EDS Cookbook has been available since Riak release 0.14 and has been used by Basho and many Basho customers to install the software. This cookbook automatically downloads and installs the version of Riak EDS. The Cookbook also generates the Erlang configuration file for Riak EDS.

**riak::autoconf recipe**

The Riak EDS Cookbook has an extra recipe for automatic configuration.

<div class="note"><div class="title">Note</div> This automatic configuration recipe works only with a full Chef Stack, not chef-solo.</div>

For more information, see the Riak EDS Cookbook's README file.

### Riak CS Cookbook
The Riak CS Cookbook installs both Riak CS and Stanchion. The Riak CS Cookbook contains two recipes.

**RiakCS::default recipe**

The default recipe downloads and installs the version of Riak CS specified. The version can be specified or overridden in package.rb or in a node attribute. This Cookbook also generates the Riak CS Erlang configuration file.

**RiakCS::stanchion recipe**

Stanchion should be installed on only one node in a Riak CS system, so the stanchion recipe should be applied only to one Riak CS node.

<div class="note"><div class="title">Note</div>Running Stanchion on more than one node can lead to problems if Riak CS nodes are configured to communicate using multiple Stanchion nodes. In this situation, the uniqueness of bucket names and user email addresses might not be enforced, which, in turn, could lead to unexpected behavior.</div>

For more information, see the Riak CS Cookbook's README file.
