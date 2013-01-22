---
title: Riak の起動と停止
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, installing]
---

Riak CS をバックグラウンドで実行するには、次のようにタイプしてください:

```bash
sudo riak-cs start
```

Erlang のインタラクティブ コンソールで実行するには次のようにします:

```bash
sudo riak-cs console
```

Riak CS が起動しているときは、プロセスリストに Riak CS が現れます。Riak CS のプロセスを確認するには、次のようにします:

```bash
ps -ef|grep riak-cs
```

Riak CS を停止するとき:

```bash
sudo riak-cs stop
```

次のコマンドが使用できます:

```bash
sudo riak-cs attach
```

コンソール上で Riak CS が起動しているインスタンスにアタッチしたり取得したりできます。

Riak CS の生死確認には、`riak-cs ping` コマンドを使用します。Riak CS が生きていて、正常に Riak と通信ができれば `pong` が返ります。

```bash
riak-cs ping
```

<div class="note"><div class="title">ノート</div>Riak CS のノードが対応する Riak ノードと通信できない場合、<tt>riak-cs ping</tt> コマンドは失敗します。<tt>riak-cs ping</tt> で Riak CS システムの全てのコンポーネントが動作していることを確認して下さい。</div>