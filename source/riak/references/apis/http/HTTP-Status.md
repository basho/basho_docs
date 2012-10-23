---
title: HTTP ステータス
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Server Operations"
---

リクエストされたRiakノードのパフォーマンスおよび設定情報のレポートを作成する。
これを使うためには app.config で `{riak_kv_stat,true}` を設定する必要がある。
この機能は [[riak-admin status|Command-Line Tools#status]] コマンドと同等である。

## リクエスト

```bash
GET /stats
```

重要なヘッダ:

* `Accept` - レスポンスのフォーマットを `application/json` にするか、`text/plain` にするかを決める

## レスポンス

正常ステータスコード:
* `200 OK`

主なエラーコード:
* `404 Not Found` - `riak_kv_stat` が有効でない

重要なヘッダ:
* `Content-Type` - `application/json` または `text/plain` (JSON は改行で区切られる)

## サンプル

```bash
$ curl -v http://127.0.0.1:8098/stats -H "Accept: text/plain"
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /stats HTTP/1.1
> User-Agent: curl/7.19.7 (universal-apple-darwin10.0) libcurl/7.19.7 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: text/plain
>
< HTTP/1.1 200 OK
< Vary: Accept, Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: text/plain
< Content-Length: 2102
<
{
    "vnode_gets": 0,
    "vnode_puts": 0,
    "read_repairs": 0,
    "vnode_gets_total": 0,
    "vnode_puts_total": 0,
    "node_gets": 0,
    "node_gets_total": 0,
    "node_get_fsm_time_mean": "undefined",
    "node_get_fsm_time_median": "undefined",
    "node_get_fsm_time_95": "undefined",
    "node_get_fsm_time_99": "undefined",
    "node_get_fsm_time_100": "undefined",
    "node_puts": 0,
    "node_puts_total": 0,
    "node_put_fsm_time_mean": "undefined",
    "node_put_fsm_time_median": "undefined",
    "node_put_fsm_time_95": "undefined",
    "node_put_fsm_time_99": "undefined",
    "node_put_fsm_time_100": "undefined",
    "read_repairs_total": 0,
    "cpu_nprocs": 84,
    "cpu_avg1": 251,
    "cpu_avg5": 174,
    "cpu_avg15": 110,
    "mem_total": 7946684000.0,
    "mem_allocated": 4340880000.0,
    "nodename": "riak@127.0.0.1",
    "connected_nodes": [

    ],
    "sys_driver_version": "1.5",
    "sys_global_heaps_size": 0,
    "sys_heap_type": "private",
    "sys_logical_processors": 2,
    "sys_otp_release": "R13B04",
    "sys_process_count": 189,
    "sys_smp_support": true,
    "sys_system_version": "Erlang R13B04 (erts-5.7.5) [[source]] [[64-bit]] [[smp:2:2]] [[rq:2]] [[async-threads:5]] [[hipe]] [[kernel-poll:true]]",
    "sys_system_architecture": "i386-apple-darwin10.3.0",
    "sys_threads_enabled": true,
    "sys_thread_pool_size": 5,
    "sys_wordsize": 8,
    "ring_members": [
        "riak@127.0.0.1"
    ],
    "ring_num_partitions": 64,
    "ring_ownership": "[{'riak@127.0.0.1',64}]",
    "ring_creation_size": 64,
    "storage_backend": "riak_kv_bitcask_backend",
    "pbc_connects_total": 0,
    "pbc_connects": 0,
    "pbc_active": 0,
    "riak_kv_version": "0.11.0",
    "riak_core_version": "0.11.0",
    "bitcask_version": "1.0.1",
    "luke_version": "0.1",
    "webmachine_version": "1.7.1",
    "mochiweb_version": "1.7.1",
    "erlang_js_version": "0.4",
    "runtime_tools_version": "1.8.3",
    "crypto_version": "1.6.4",
    "os_mon_version": "2.2.5",
    "sasl_version": "2.1.9",
    "stdlib_version": "1.16.5",
    "kernel_version": "2.13.5"
}
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
```

## 出力の説明

出力結果の `/stats` には、設定とパフォーマンスについての詳細が含まれます。詳細は以下のとおりです。


## CPU とメモリ

CPUの統計情報はErlangの cpu\_sup モジュールから直接得ています。これに関するドキュメントは [[ErlDocs: cpu_sup|http://erldocs.com/R14B04/os_mon/cpu_sup.html]] にあります。

* `cpu_nprocs`: オペレーティングシステムのプロセス数
* `cpu_avg1`: 過去1分間における有効なプロセスの平均数(top(1)コマンドの平均ロード数を256で割ったものと同等)
* `cpu_avg5`: 過去5分間における有効なプロセスの平均数(top(1)コマンドの平均ロード数を256で割ったものと同等)
* `cpu_avg15`: 過去15分における有効なプロセスの平均数(top(1)コマンドの平均ロード数を256で割ったものと同等)

メモリの統計情報はErlangバーチャルマシンから直接得ています。これに関するドキュメントは [[ErlDocs: Memory|http://erldocs.com/R14B04/erts/erlang.html?i=0&search=erlang:memory#memory/0]] にあります。

* `memory_total`: アロケート済みメモリの合計(プロセスおよびシステムの合計)
* `memory_processes`: Erlangのプロセスとしてアロケートされているメモリの総計
* `memory_processes_used`: Erlangのプロセスが使用しているメモリの総計
* `memory_system`: Erlangのプロセスとして直接使われていないメモリの合計
* `memory_atom`: 現在アトムストレージとしてアロケートされているメモリの合計
* `memory_atom_used`: 現在アトムストレージとして使用しているメモリの合計
* `memory_binary`: バイナリが使用しているメモリの合計
* `memory_code`: Erlangのコードにアロケートされているメモリの合計
* `memory_ets`: ErlangのTermストレージにアロケートされているメモリの合計
* `mem_total`: 使用可能なシステムメモリの合計
* `mem_allocated`: このノードにアロケートされているメモリの合計


## ノード、クラスタ、システム

* `nodename`: ステータス出力に使われているノードの名前
* `connected_nodes`: このノードに接続されているノードのリスト
* `read_repairs`: このノードが最後に扱ったリードリペア オペレーションの数
* `read_repairs_total`: ノードが作られてからの、このノードが扱っているリードリペア オペレーションの数
* `coord_redirs_total`: ノードが作られてからの、他のノードを扱っているノードのリクエストの数
* `ring_members`: リングのメンバーであるノードのリスト
* `ring_num_partitions`: リング内のパーティションの数
* `ring_ownership`: リング内の、パーティションのオーナである全ノードのリスト
* `ring_creation_size`: ノードに所有権のあるパーティションの数
* `ignored_gossip_total`: ノードが作られた時点からの、無視したゴシップメッセージの合計数
* `handoff_timeouts`: このノードで起きたハンドオフ タイムアウトの数
* `coord_redirs_total`: 起動時店からの、このノードから他のノードへリダイレクトしたリクエストの数
* `precommit_fail`: プレコミットフックのエラー数
* `postcommit_fail`: ポストコミットフックのエラー数
* `sys_driver_version`: ランタイムシステムによって使われているErlangドライバのバージョンを表す文字列
* `sys_global_heaps_size`: 共有グローバルヒープの現在のサイズ
* `sys_heap_type`: 使用中のヒープ型(private、shared、hybridのいずれか)を表す文字列
* `sys_logical_processors`: システムで有効な論理プロセサの数
* `sys_otp_release`: ノードで使用しているErlang OTPのリリースバージョン
* `sys_process_count`: このノードに存在するプロセスの数
* `sys_smp_support`: 対称マルチプロセッサ(SMP)が有効かどうかを表すブール値
* `sys_system_version`: 詳細なErlangのバージョン情報
* `sys_system_architecture`: ノードのオペレーティングシステムとハードウェアのアーキテクチャ
* `sys_threads_enabled`: スレッドが有効であることを表すブール値
* `sys_thread_pool_size`: 非同期スレッドプールにあるスレッドの数
* `sys_wordsize`: Erlangにおけるワードのバイト長、たとえば32ビットアーキテクチャでは4、64ビットアーキテクチャでは8となる
* `storage_backend`: 有効なストレージバックエンドの名前
* `pbc_connects_total`: ノードが作られてからのプロトコルバッファのコネクション数
* `pbc_connects`: 最近のプロトコルバッファのコネクション数
* `pbc_active`: 有効なプロトコルバッファのコネクション数
* `ssl_version`: 使用中のセキュア ソケット レイヤ(SSL)アプリケーションのバージョン
* `public_key_version`: パブリック キー アプリケーションのバージョン
* `runtime_tools_version`: 使用中のランタイム ツール アプリケーションのバージョン
* `basho_stats_version`: 使用中のBasho統計アプリケーションのバージョン
* `riak_search_version`: 使用中のRiak Searchアプリケーションのバージョン
* `riak_kv_version`: 使用中のRiak KVアプリケーションのバージョン
* `bitcask_version`: 使用中のBitcaskアプリケーションのバージョン
* `luke_version`: 使用中のLukeアプリケーションのバージョン
* `erlang_js_version`: 使用中のErlang JSアプリケーションのバージョン
* `mochiweb_version`: 使用中のMochiWebアプリケーションのバージョン
* `inets_version`: 使用中のInetsアプリケーションのバージョン
* `riak_pipe_version`: 使用中のRiak Pipeアプリケーションのバージョン
* `merge_index_version`: 使用中のMerge Indexアプリケーションのバージョン
* `cluster_info_version`: 使用中のクラスタ情報アプリケーションのバージョン
* `basho_metrics_version`: 使用中のBasho Metricsアプリケーションのバージョン
* `riak_control_version`: 使用中のRiak Controlアプリケーションのバージョン
* `riak_core_version`: 使用中のRiak Coreアプリケーションのバージョン
* `lager_version`: 使用中のLagerアプリケーションのバージョン
* `riak_sysmon_version`: 使用中のRiakシステムモニタ アプリケーションのバージョン
* `webmachine_version`: 使用中のWebmachineアプリケーションのバージョン
* `crypto_version`: 庄祐のCryptographyアプリケーションのバージョン
* `os_mon_version`: 使用中のオペレーティングシステム モニタ アプリケーションのバージョン
* `sasl_version`: 使用中のSASLアプリケーションのバージョン
* `stdlib_version`: 使用中の標準ライブラリ アプリケーションのバージョン
* `kernel_version`: 使用中のカーネル アプリケーションのバージョン

### ノードおよびＶノード カウンタ

* `vnode_gets`: このノード上のＶノードが、最近扱ったGETオペレーションの数
* `vnode_puts`: このノード上のＶノードが、最近扱ったPUTオペレーションの数
* `vnode_gets_total`: ノードが作られてからの、このノード上のＶノードが扱ったGETオペレーションの数
* `vnode_puts_total`: ノードが作られてからの、このノード上のＶノードが扱ったPUTオペレーションの数
* `node_gets`: このノードが最近扱った、ローカルおよび非ローカルGETオペレーションを合わせた数
* `node_gets_total`: ノードが作られてからの、このノードが扱った、ローカルおよび非ローカルGETオペレーションを合わせた数

### マイクロ秒タイマ

* `node_get_fsm_time_mean`: クライアントのGETリクエストからレスポンスが返るまでの平均時間
* `node_get_fsm_time_median`: クライアントのGETリクエストからレスポンスが返るまでの時間の中央値
* `node_get_fsm_time_95`: クライアントのGETリクエストからレスポンスが返るまでの時間の100分の95番目
* `node_get_fsm_time_99` クライアントのGETリクエストからレスポンスが返るまでの時間の100分の99番目
* `node_get_fsm_time_100` クライアントのGETリクエストからレスポンスが返るまでの時間の100分の100番目
* `node_puts`: このノードが最近扱った、ローカルおよび非ローカルPUTオペレーションを合わせた数
* `node_puts_total`: ノードが作られてからの、このノードが扱った、ローカルおよび非ローカルPUTオペレーションの数
* `node_put_fsm_time_mean`: クライアントのPUTリクエストからレスポンスが返るまでの平均時間
* `node_put_fsm_time_median`: クライアントのPUTリクエストからレスポンスが返るまでの時間の中央値
* `node_put_fsm_time_95`: クライアントのPUTリクエストからレスポンスが返るまでの時間の100分の95番目
* `node_put_fsm_time_99`: クライアントのPUTリクエストからレスポンスが返るまでの時間の100分の99番目
* `node_put_fsm_time_100`: クライアントのPUTリクエストからレスポンスが返るまでの時間の100分の100番目

### オブジェクト、インデクス、兄弟メトリクス

* `node_get_fsm_objsize_mean`: 最近の、このノードに到達したオブジェクトサイズの平均
* `node_get_fsm_objsize_median`: 最近の、このノードに到達したオブジェクトサイズの中央値
* `node_get_fsm_objsize_95`: 最近の、このノードに到達したオブジェクトサイズの100分の95番目
* `node_get_fsm_objsize_99`: 最近の、このノードに到達したオブジェクトサイズの100分の99番目
* `node_get_fsm_objsize_100` 最近の、このノードに到達したオブジェクトサイズの100分の100番目
* `vnode_index_reads`: 最新の、Ｖノードによるインデクス読み出しオペレーションの数
* `vnode_index_writes`: 最新の、Ｖノードによるインデクス書き込みオペレーションの数
* `vnode_index_deletes`: 最新の、Ｖノードによるインデクス削除オペレーションの数
* `vnode_index_reads_total`: ノードが作られてからの、Ｖノードによるインデクス読み出しオペレーションの数
* `vnode_index_writes_total`: ノードが作られてからの、Ｖノードによるインデクス書き込みオペレーションの数
* `vnode_index_deletes_total`: ノードが作られてからの、Ｖノードによるインデクス削除オペレーションの数
* `node_get_fsm_siblings_mean`: 最新の、このノードによる兄弟への全てのGETオペレーションの平均数
* `node_get_fsm_siblings_median`: 最新の、このノードによる兄弟への全てのGETオペレーション数の中央値
* `node_get_fsm_siblings_95`: 最新の、このノードによる兄弟への全てのGETオペレーション数の100分の95番目
* `node_get_fsm_siblings_99`: 最新の、このノードによる兄弟への全てのGETオペレーション数の100分の99番目
* `node_get_fsm_siblings_100`: 最新の、このノードによる兄弟への全てのGETオペレーション数の100分の100番目
