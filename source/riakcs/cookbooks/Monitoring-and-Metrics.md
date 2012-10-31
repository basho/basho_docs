---
title: モニタリングとメトリクスMonitoring and Metrics
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, troubleshooting]
---

Riak CS は、Folsom Statistics ライブラリを使った
統計の取得が可能です。
Riak CS provides operational statistics which can be useful for monitoring through the Folsom statistics library, and initial probes for analysis of the running system with [[DTrace|http://dtrace.org/blogs/about/]].

## オペレーションの統計
Riak と同様に、Riak CS は
一般的なモニタリング、警告、trend analysis といったクリティカルな統計を取ることができます。これらの統計は、次のリソースへの HTTP リクエストとしてアクセスすることができます。
Much like Riak, Riak CS exposes statistics on critical operations which are commonly used for monitoring, alerting, and trend analysis. These statistics can be accessed through HTTP requests to the following resource:

```
/riak-cs/stats
```

レイテンシ・ヒストグラムではその平均、95%、99% を確認でき、さらに各統計における合計および割合を確認することができます。

結果は、次の操作によって、ヒストグラムおよびカウンタとして取得することができます。

* **block_get**: 実行した BLOCK GET 操作の合計
* **block_put**: 実行した BLOCK PUT 操作の合計Total BLOCK GET operations performed  <<== mistake???? <<==
* **block_delete**: 実行した BLOCK DELETE 操作の合計
* **service_get_buckets**: 実行した GET BUCKETS 操作の合計
* **bucket_list_keys**: 実行した BUCKETLISTKEYS 操作の合計
* **bucket_create**: 実行した BUCKET CREATE 操作の合計
* **bucket_delete**: 実行した BUCKET DELETE 操作の合計
* **bucket_get_acl**: 実行した BUCKET GET ACL 操作の合計
* **bucket_put_acl**: 実行した BUCKET PUT ACL 操作の合計
* **object_get**: 実行した GET 操作の合計
* **object_put**: 実行した PUT 操作の合計
* **object_head**: 実行した OBJECT HEAD 操作の合計
* **object_delete**: 実行した OBJECT DELETE 操作の合計
* **object_get_acl**: 実行した OBJECT GET ACL 操作の合計
* **object_put_acl**: 実行した OBJECT PUT ACL 操作の合計

## DTrace プローブ
Riak CS は、稼働中のシステムにおける特定の操作を調査するために [[DTrace|http://dtrace.org/blogs/about/]] を利用することができます。これによってさまざまな統計情報を得ることができます。

### 使い方の例
以下は、DTrace によって、稼働中の Riak CS のさまざまなコンポーネントの状況を取得する際の例です。

**リクエストしたユーザオブジェクトをトレース**:

    dtrace -qn 'erlang*:::user_trace* /arg2 == 703/ {printf("pid %s: mod %s op %s: user %s bucket/file %s\n", copyinstr(arg0), copyinstr(arg6), copyinstr(arg7), copyinstr(arg8), copyinstr(arg9));}'

**実行中の webmachine リソースをトレース**:

    dtrace -qn 'erlang*:::user_trace* /arg2 == 705/ {printf("pid %s: %s:%s\n", copyinstr(arg0), copyinstr(arg6), copyinstr(arg7));}'

<div class="info"><div class="title">DTrace のサポート</div> SmartOS および他のオペレーティングシステム上で稼働する Riak CS パッケージ用の DTrace サポートは、現在進行中で、最終的にはこれらのオペレーティングシステム上で Riak CS インスタンスを、低いレベルから解析する能力が追加されます。</div>
