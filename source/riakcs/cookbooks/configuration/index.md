---
title: Configuring Riak CS Overview
project: riakcs
version: 1.2.0+
document: cookbook
toc: false
index: true
audience: intermediate
keywords: [operator, configuration]
---

Riak CS ストレージシステムでは、3 つのコンポーネントが連携して動作しているので、他のものと協調できるように各コンポーネントの設定を行わなければなりません。

* Riak - バックエンド ストレージとして動作する、データベースシステム
* Riak CS - Riak の、クラウド ストレージ レイヤで、ストレージおよび課金 API を提供し、Riak へのファイルおよびメタデータの格納を行い、ユーザへのデータストリーミングを行う
* Stanchion - Riak のインスタンスに送られたバケットやユーザなどの、システム全体でユニークなエンティティを含むリクエストを管理します。たとえばユーザの作成や、バケットの作成 / 削除などです。

さらに、Riak CS システムとコミュニケートするためには、S3 クライアントの設定を行わなければなりません。

システム内の各 Riak CS ノードごとに 1 つの Riak ノードを用意するべきです。Riak および Riak CS ノードを、別々の物理マシン上で走らせることができますが、多くの場合、Riak と Riak CS のノードは同一の物理マシンで動かすことが望まれます。1 台の物理マシンに、Riak と Riak CS のノードが必要とする十分な能力があるとすると、ネットワークのレイテンシを低くすることができ、パフォーマンスが向上します。

システムが複数のノードから構成されるなら、コンポーネント間のコミュニケーションを第一に考えて設定します。ログファイルなどのその他の設定は、必要な場合にのみデフォルトから変更します。

## システムコンポーネントの設定

* [[Configuring Riak]]
* [[Configuring Riak CS]]
* [[Configuring Stanchion]]
* [[Configuring an S3 client]]
