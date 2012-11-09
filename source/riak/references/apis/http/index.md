---
title: HTTP API
project: riak
version: 1.2+
document: api
toc: true
audience: advanced
keywords: [api, http]
index: true
---

Riak には完全な機能を豊富に持つ HTTP 1.1 API があります。
これは HTTP を使ってできる操作の概要を紹介し、また、クライアントを開発するときのガイドとすることができます。
すべての URL は、デフォルトで適切に設定された値を期待しています。
すべてのサンプルは、Riak と対話するために `curl` を使っています。

<div class="note"><div class="title">Client ID</div>
<p>Riak 1.0 より前、あるいは `vnode_vclock` を有効にしていない Riak 1.0 へのすべてのリクエストは、`X-Riak-ClientId` というヘッダを付加しなければいけません。これはクライアントをユニークに識別できればどんな文字列でもかまいません。オブジェクトの変更を追跡する [[ベクタークロック|Vector Clocks]] に使用します。</p>
</div>

<div class="note"><div class="title">URL エスケープ</div>
<p>バケット、キー、リンクの仕様ではエスケープしていないスラッシュを含めません。URL エスケープライブラリを使用するか、スラッシュを `%2F` に置き換えてください。</p>
</div>

## バケットの操作

Riak のバケットは仮想の概念です。
これらは主にネームスペース メカニズムであり、デフォルトのバケット設定から逸脱した設定変更から分離するためのメカニズムとして働きます。
たとえば、あなたが [[レプリカの数|Replication#Selecting-an-N-value-(n_val)]] を増やしてしまうかもしれませんし、特定のストレージバックエンドや [[コミットフック|Pre- and Post-Commit Hooks]] をバケット レベルで行うかもしれません。

<div class="info"><div class="title">何個のバケットを使えますか？</div>
<p>今のところ、デフォルトのバケット プロパティを変更しない限り、バケットにはいかなるコストも発生しません。
バケット プロパティを変更すると、それが周囲のクラスタに伝播し、結果としてネットワークを流れるデータの送料が増えます。
言い換えると、デフォルトのバケット プロパティにはコストがかかりません。</p>
</div>

<div class="note"><div class="title">バケットを削除する</div>
<p>全てのバケットを一気に削除する方法はありません。バケット内の全てのキーを削除するには、それぞれをひとつづつ削除する必要があります。</P>
</div>

## バケットの操作

* [[HTTP バケットのリスト|HTTP List Buckets]]
* [[HTTP キーのリスト|HTTP List Keys]]
* [[HTTP バケットのプロパティを得る|HTTP Get Bucket Properties]]
* [[HTTP バケットのプロパティを設定する|HTTP Set Bucket Properties]]

## オブジェクト / キーの操作

バケット、キー、バリュー、メタデータを組み合わせたものは、"Riak オブジェクト" と呼ばれます。
以下の操作は、それぞれのオブジェクトを操作します。

* [[HTTP オブジェクトのフェッチ|HTTP Fetch Object]]
* [[HTTP オブジェクトを格納する|HTTP Store Object]]
* [[HTTP オブジェクトを削除する|HTTP Delete Object]]

## クエリ操作

* [[HTTP リンクウォーキング|HTTP Link Walking]]
* [[HTTP MapReduce]]
* [[HTTP セカンダリインデックス|HTTP Secondary Indexes]]

<!-- ## Luwak Operations (Large Objects)

Luwak is an optional interface that automatically segments large files across
multiple Riak Objects.

* [[HTTP Get Luwak Properties]]
* [[HTTP List Luwak Keys]]
* [[HTTP Fetch Luwak Object]]
* [[HTTP Store Luwak Object]]
* [[HTTP Delete Luwak Object]]
 -->

## サーバ操作

* [[HTTP Ping]]
* [[HTTP ステータス|HTTP Status]]
* [[HTTP リソースのリスト|HTTP List Resources]]
