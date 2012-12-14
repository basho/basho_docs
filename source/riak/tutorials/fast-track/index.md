---
title: The Riak Fast Track
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, fast-track]
next: ["What is Riak?", "What-is-Riak.html"]
---

Riak の分散型データベースのアーキテクチャ:

* アベイラビリティ: Riak はデータをインテリジェントに複製および取得できます。つまり故障が発生しても、読み出し、書き込みの操作が可能です。
* フォールトトレランス: ネットワークのパーティショニングやハードウェアエラーなどによりノードの喪失が起きても、決してデータは失われません。
* シンプルなオペレーション: Riak クラスタに新しいマシンを導入するときも、ややこしい操作は不要です。小さなクラスタの時にも、大きなクラスタの時にも操作は同じです。
* スケーラビリティ: Riak は自動的にデータを周囲のクラスタに分散させます。さらに容量を追加すればほぼリニアにパフォーマンスが向上します。

## Riak の Fast Track とは？

Riak の Fast Track の目的は Riak をできるだけ素早く立ち上げ、動作させることにあります。ぜひこれを動かし、学んでください。これには Riak をインストールするための一連のモジュールがあり、4 つのノード クラスタを動かし、基本的な操作と、Riak のコアコンセプトを明らかにします。

Fast Track は Riak についての経験が無いか、あるいは少しだけといった人を対象にしています。もちろん経験のある人にも役に立ちます。最初から最後まで、およそ 45 分間かかります。

## Fast Track のカバー範囲は？

Fast Track には以下のセクションがあります。

* [[Riak とは？|What is Riak]]: Riak の概要とアーキテクチャについて
* [[開発環境を構築する|Building a Development Environment]]: マシンに開発用クラスタをインストールする
* [[Riak の基本 API の操作|Basic Riak API Operations]]:  標準 API の操作について
* [[データのロードと MapReduce クエリ|Loading Data and Running MapReduce Queries]]: データをインポートし、簡単な MapReduce クエリを実行する
* [[リンクとリンクウォーキング|Links and Link Walking]]:  Riak でのリンク操作方法
* [[CAP コントロールを調整する|Tunable CAP Controls in Riak]]:  整合性とアベイラビリティの調整方法
