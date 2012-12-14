---
title: データのロードと MapReduce クエリ
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, fast-track]
prev: ["Basic HTTP Operations", "Basic-Riak-API-Operations.html"]
up:   ["The Riak Fast Track", "index.html"]
next: ["Links and Link Walking", "Links-and-Link-Walking.html"]
---

Riak は基本的なキー/バリュー操作の他に、さまざまな方法でデータをクエリする手段を提供しています: [[フルテキストサーチ|Riak Search]]、[[MapReduce]]、[[セカンダリインデックス|Secondary Indexes]]、[[リンクウォーキング|Links]]

このセクションではサンプルデータ(Googleから借用した物)を Riak にロードし、Curl で HTTP インタフェース経由で JSON を使って、データに対して MapReduce クエリをかけます。

## サンプルデータ

この Erlang スクリプトは Google の過去の株価データ(株式銘柄コード "GOOG")を既存の Riak クラスタにロードして、使用可能にします。以下のコードを `load_data` という名前のファイルにして、`dev` ディレクトリに配置してください。下記からダウンロードしても構いません。

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = tl(re:split(Data, "\r?\n", [{return, binary},trim])),
    lists:foreach(fun(L) -> LS = re:split(L, ","), format_and_insert(LS) end, Lines).

format_and_insert(Line) ->
    JSON = io_lib:format("{\"Date\":\"~s\",\"Open\":~s,\"High\":~s,\"Low\":~s,\"Close\":~s,\"Volume\":~s,\"Adj. Close\":~s}", Line),
    Command = io_lib:format("curl -XPUT http://127.0.0.1:8091/riak/goog/~s -d '~s' -H 'content-type: application/json'", [hd(Line),JSON]),
    io:format("Inserting: ~s~n", [hd(Line)]),
    os:cmd(Command).
```

スクリプトを実行可能にします:


```bash
$ chmod +x load_data
```

下記のリンクから株式データの CSV ファイルをダウンロードし、"dev" ディレクトリに置き、使えるようにします。

* [goog.csv](https://github.com/basho/basho_docs/raw/master/source/data/goog.csv) - Google の株式の過去データ
* [load_stocks.rb](https://github.com/basho/basho_docs/raw/master/source/data/load_stocks.rb) - Ruby によるデータをロードするスクリプト
* [load_data](https://github.com/basho/basho_docs/raw/master/source/data/load_data.erl) - Erlang によるデータをロードするスクリプト(一部は既出)

データを Riak にロードします。

```bash
$ ./load_data goog.csv
```

これで Riak クラスタにデータが格納されました。ちょっと脇道にそれて、MapReduce について、さらに Riak がそれをどのように使うかについて軽くおさらいしましょう。

## MapReduce

MapReduce は Google によって一般に広まったプログラミングパラダイムです。Riak では、プライマリキー以外へのクエリを行うためには、主に MapReduce を使用します。

MapReduce ジョブは Erlang API または HTTP API のいずれを使っても操作できます。このチュートリアルでは HTTP API を使います。

### どうして Riak のクエリに MapReduce を使うのですか？

Riak の標準であるキー・バリュー ストアの機能は単純にデータの格納と取得だけで、とても限定されています。MapReduce は Riak に、もっと強力なクエリ機能を付与します。さらに、関数型プログラミング志向の Riak コア・コードと、分散型のデータストレージに適合しています。

MapReduce の最終目標はさまざまなシステムで、クエリを分散化して並列処理することです。これはほぼ完成しています。クエリをいくつかのステップに分割して、データセットもいくつかのチャンクに分割して、これらのステップ/チャンクのペアを別々の物理ホストで走らせます。Riak の MapReduce には別な目的もあります。データの局所性を高めることです。巨大なデータセットを処理する際に、データを取ってきて演算するよりも、データ側で演算させたほうが効率的だという場合がよくあります。

"Map" および "Reduce" はいずれもクエリ処理のフェーズのことです。Map 機能はデータのピースを入力とし、処理を行い 0 個以上の結果を出力します。"リストのマッピング" を行う関数についてのプログラミング経験がおありでしたら、map/reduce クエリの "map" ステップについてよくお分かりのはずです。

## HTTP クエリの書式

ご自分のサンプルデータで MapReduce クエリを試してみる前に、クエリの書き込みについてと、それがどのように実行されるのかについてもう一度おさらいをしましょう。

MapReduce クエリは、HTTP で "/mapred" リソースに *POST* されます。ボディは次のような形式で、"application/json" にします。

```javascript
{"inputs":[...inputs...],"query":[...query...]}
```

Map/Reduce クエリのデフォルト タイムアウト時間は 60000 ミリ秒(60秒)です。デフォルトタイムアウトは JSON ドキュメントを使って、別の値をミリ秒で与えることでオーバライドすることができます。

```javascript
{"inputs":[...inputs...],"query":[...query...],"timeout": 90000}
```

### 入力

入力オブジェクトのリストは、[Bucket,Key] という形の 2 要素リスト、あるいは [Bucket,Key,KeyData] という 3 要素リストとして与えられます。

バケット名だけを渡す ({"inputs":"mybucket",...}) というのは、そのバケット内のすべてのキーを入力として渡すのと等価です(例: "a map/reduce across the whole bucket")。この操作が "list keys" という負荷の大きなオペレーションを誘発することにお気づきのはずです。ご使用は控えめにしてください。

### クエリ

クエリはフェーズのリストとして与えられます。各フェーズは {PhaseType:{...spec...}} という形をしています。有効な PhaseType は "map"、"reduce"、"link" です。

フェースには "keep" フィールドを持たすことができます。ブール値で "true" は、このフェーズのリザルトが map/reduce の最終リザルトであることを示します。"false" は、このフェーズのリザルトが、次のフェーズで使用されるためのものだということを示します。"keep" フィールドを省略すると、デフォルト値である "false" が使われ、最終フェーズ (あなたが欲しいのは map/reduce クエリの最終フェーズの結果であると仮定しています) 以外のフェーズであることを示します。

#### マップ

Map フェーズには、検索を行う関数の場所と、それがどの言語で書かれているのかの情報を与えなければなりません。

関数の場所は "source" フィールドで指定できます。関数の場所は "bucket" と "key" フィールドで、あらかじめ格納されている Riak オブジェクトからロードすることもできます。

サンプル:

```javascript
{"map":{"language":"javascript","source":"function(v) { return [v]; }","keep":true}}
```

これは Javascript 関数を実行し、map/reduce クエリの最終結果が出力されます。

```javascript
{"map":{"language":"javascript","bucket":"myjs","key":"mymap","keep":false}}
```

これは "myjs" バケット内の "maymap" オブジェクトにある Riak オブジェクトの中で定義されている Javascript 関数を実行し、その実行結果は map/reduce クエリの最終結果ではないということを表します。

```javascript
{"map":{"language":"erlang","module":"riak_kv_mapreduce","function":"map_object_value"}}
```

これは Erlang による "riak_kv_mapreduce:map_object_value/3" という関数を実行します。

Map フェーズには "arg" フィールドで引数を渡すこともできます。

#### Reduce

ひとことで言うと Reduce フェーズと map フェーズは同じですが、"reduce" というラベルが付いています。

<div class="info">map と reduce 機能についてもっとお知りになりたいときは docs の [[MapReduce|MapReduce#Phasefunctions]] セクションを参照してください。ここには関数へ渡す引数の説明もあります。</div>

#### Link

Link フェーズはリンククエリで一致したリンクを "bucket" と "tag" フィールドで受けとります。各フィールドの "_" (アンダースコア) は、"すべてと一致" という意味で、それ以外の文字列は "この文字列と一致するもの" を表します。両フィールドが空だったときは、いずれも "_" (すべてと一致)が指定されたとみなされます。

例:

```javascript
{"link":{"bucket":"foo","keep":false}}
```

これは "foo" バケット内で、タグに関わらず、オブジェクトを差しているすべてのリンクを追跡します

## MapReduce スクリーンキャスト

構文とクエリデザインで疲れた頭をリフレッシュするために、数分間、スクリーンキャストで Riak の MapReduce が動く様子を御覧ください。

<div style="display:none" class="iframe-video" id="http://player.vimeo.com/video/11328947"></div>

<p><a href="http://vimeo.com/11328947">JavaScript MapReduce in Riak</a> from <a href="http://vimeo.com/bashotech">Basho Technologies</a> on <a href="http://vimeo.com">Vimeo</a>.</p>

私たちが投稿したスクリーンキャストの一覧です:

<dl>
<dt>[[simple-map.json|https://github.com/basho/basho_docs/raw/master/source/data/simple-map.json]]</dt>
<dd>単純な map のみのジョブで、データセットを返します。</dd>
<dt>[[map-high.json|https://github.com/basho/basho_docs/raw/master/source/data/map-high.json]]</dt>
<dd>1月第1週の最高売上高を返す map-reduce ジョブです。</dd>
<dt>[[map-highs-by-month.json|https://github.com/basho/basho_docs/raw/master/source/data/map-highs-by-month.json]]</dt>
<dd>月毎の最高値を集計する、もっと複雑な map-reduce ジョブです。</dd>
<dt>[[first-week.json|https://github.com/basho/basho_docs/raw/master/source/data/first-week.json]]</dt>
<dd>2010年1月第1週の値を返す、単純な map のみのジョブです。</dd>
</dl>

## 機能のサンプル

MapReduce ジョブをご覧いただきました。次はあなたが自分でやる番です。

前のセクションでロードしたサンプルデータを元にして、動作するサンプルを用意しました。これらを動かしてみください。興味があればこれを改造して、MapReduce がどのように結果を返すかなどをお確かめください。

<div class="info"><div class="title">シェル上で [[MapReduce]] クエリを投げる</div>シェル上でクエリを実行するために、ここでは curl コマンドを使います:

```bash
curl -X POST http://127.0.0.1:8091/mapred -H "Content-Type: application/json" -d @-
```

RETURN を押してから、ジョブをペーストしてください。たとえば下記の "完了ジョブ" セクションの中身です。再度 RETURN を押します。それから Ctrl-D でサブミットします。この方法による MapReduce クエリは、このチュートリアルのためだけでなく、とても簡単なので、コマンドラインから使う使い捨てのクエリとして利用できます。クライアントライブラリを使えば、Riak に投げるために JSON を組み立てるなどの泥臭い仕事は代わりにやってもらえます。</div>

### Map: 最高値が $600.00 以上の日を探す

*フェーズ関数*

```javascript
function(value, keyData, arg) {
  var data = Riak.mapValuesJson(value)[0];
  if(data.High && data.High > 600.00)
    return [value.key];
  else
    return [];
}
```

*完了ジョブ*

```json
{"inputs":"goog",
 "query":[{"map":{"language":"javascript",
                  "source":"function(value, keyData, arg) { var data = Riak.mapValuesJson(value)[0]; if(data.High && parseFloat(data.High) > 600.00) return [value.key]; else return [];}",
                  "keep":true}}]
}
```

[sample-highs-over-600.json](https://github.com/basho/basho_docs/raw/master/source/data/sample-highs-over-600.json)

### Map: クローズ時間がオープン時間よりも早い日を検索する

*フェーズ関数*

```javascript
function(value, keyData, arg) {
  var data = Riak.mapValuesJson(value)[0];
  if(data.Close < data.Open)
    return [value.key];
  else
    return [];
}
```

*完了ジョブ*

```json
{"inputs":"goog",
 "query":[{"map":{"language":"javascript",
                  "source":"function(value, keyData, arg) { var data = Riak.mapValuesJson(value)[0]; if(data.Close < data.Open) return [value.key]; else return [];}",
                  "keep":true}}]
}
```

[sample-close-lt-open.json](https://github.com/basho/basho_docs/raw/master/source/data/sample-close-lt-open.json)

### Map と Reduce: 価格の日差変動が最も大きい月を探す

*フェーズ関数*


```javascript
/* Map function to compute the daily variance and key it by the month */
function(value, keyData, arg){
  var data = Riak.mapValuesJson(value)[0];
  var month = value.key.split('-').slice(0,2).join('-');
  var obj = {};
  obj[month] = data.High - data.Low;
  return [ obj ];
}

/* 月毎の最大変動を探す Reduce 関数 */
function(values, arg){
  return [ values.reduce(function(acc, item){
             for(var month in item){
                 if(acc[month]) { acc[month] = (acc[month] < item[month]) ? item[month] : acc[month]; }
                 else { acc[month] = item[month]; }
             }
             return acc;
            })
         ];
}
```

*完了ジョブ*

```json
{"inputs":"goog",
 "query":[{"map":{"language":"javascript",
                  "source":"function(value, keyData, arg){ var data = Riak.mapValuesJson(value)[0]; var month = value.key.split('-').slice(0,2).join('-'); var obj = {}; obj[month] = data.High - data.Low; return [ obj ];}"}},
         {"reduce":{"language":"javascript",
                    "source":"function(values, arg){ return [ values.reduce(function(acc, item){ for(var month in item){ if(acc[month]) { acc[month] = (acc[month] < item[month]) ? item[month] : acc[month]; } else { acc[month] = item[month]; } } return acc;  }) ];}",
                    "keep":true}}
         ]
}
```

<!-- TODO: replace with a gist link -->

[sample-max-variance-by-month.json](https://github.com/basho/basho_docs/raw/master/source/data/sample-max-variance-by-month.json)

## MapReduce チャレンジ

ロードしたデータを使う例題を出題します。お時間があれば、MapReduce で学んだことを応用してみてください。


<div class="note"><div class="title">MapReduce チャレンジ</div>毎月の取引額がもっとも大きい日を検索しなさい。さらに全体でもっとも大きい日はいつでしょうか。

*ヒント*: それぞれについて、1つ以上の map と reduce フェーズが必要です。</div>

#### 参考

* [[MapReduce In Depth|MapReduce]]
* [Erlang MapReduce](MapReduce#MapReduce via the Erlang API)
* [A Longer MapReduce Screencast](http://blog.basho.com/2010/02/03/the-release-riak-0.8-and-javascript-mapreduce/)
* [A list of prebuilt JavaScript MapReduce Functions that ship with Riak](https://github.com/basho/riak_kv/blob/master/priv/mapred_builtins.js)
* [Google's Original MapReduce Paper](http://research.google.com/archive/mapreduce.html)
