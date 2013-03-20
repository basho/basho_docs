---
title: Riak CS
project: riakcs
version: 0.10.0+
document: tutorial
toc: true
index: true
audience: beginner
keywords: []
simple: true
---

![Riak CS Logo](/images/riak-cs-logo.png)
<br>
<br>
Riak CS はパブリックおよびプライベートクラウド向けのマルチテナント クラウド ストレージ ソフトウェアです。Riak CS は Basho の分散型データベースである Riak 上に構築された、シンプルで、アベイラブルな、どんなスケールにも対応した分散型クラウドストレージです。Riak CS は S3-API と互換性があり、テナントごとの課金情報、利用状況を確認することができます。Riak CS をご試用になるためには、[開発者トライアル](http://info.basho.com/RiakCS1.1_DeveloperTrialRequest.html) に登録してください。

## 特筆すべき Riak CS の機能

<table style="width: 100%; border-spacing: 0px;">
<tbody>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>Amazon S3-API 互換</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p>Riak CS には S3 ACL をサポートする S3 インタフェースが組み込まれているので、既存の S3 ツールやフレームワーク、さらに Amazon のデータをインポート / エクスポートすることができます。HTTP REST API は、サービス、バケット、オブジェクトレベルの操作をサポートし、データを簡単に格納し、取得することができます。</p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>マルチデータセンター・レプリケーション</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p>Riak CS はアクティブバックアップ、ディザスタリカバリ、データ局所性のためにマルチデータセンター・レプリケーションを提供しています。これはユーザーの場所に関わらず、低レイテンシのストレージを提供し、サイト障害が起きても可用性を維持します。&nbsp;</p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;"><strong>テナント単位の可視性</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;">
<p>Riak CS のレポート API を使うと、テナントごとのデータ利用量やネットワーク I/O の統計情報などを取得することができます。このレポート機能を使うことで、経理、売買、キャンセル、課金システムプラグイン、効率的な多部門利用などが実現できます。</p>
</td>
</tr>
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;">
<p><strong>最大 5GB の任意の形式のオブジェクト、およびメタデータをサポート</strong></p>
</td>
<td style="padding: 15px; margin: 15px; border-width: 0 0 1px 0; border-style: solid;">
<p>画像、テキスト、ビデオ、ドキュメント、データベースのバックアップ、ソフトウェアのバイナリ、その他を、最大 5GB の、アクセスしやすい単一オブジェクトとして格納できます。さらに、Riak CS は標準的な Amazon メタデータヘッダもサポートしています。</p>
</td>
</tr>
{{#1.3.0+}}
<tr align="left" valign="top">
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;"><strong>マルチデータセンター・レプリケーション</strong></td>
<td style="padding: 15px; margin: 15px; border-width: 1px 0 1px 0; border-style: solid;">
<p>Riak CS はアクティブバックアップ、ディザスタリカバリ、データ局所性のためにマルチデータセンター・レプリケーションを提供しています。これはユーザーの場所に関わらず、低レイテンシのストレージを提供し、サイト障害が起きても可用性を維持します。&nbsp;</p>
</td>
</tr>
{{/1.3.0+}}
</tbody>
</table>

