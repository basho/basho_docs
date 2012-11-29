---
title: セカンダリインデックスのサンプル
project: riak
version: 1.2.0+
document: tutorials
toc: true
audience: advanced
keywords: [operator, 2i]
---

以下のサンプルを実行するためには、localhost 上で Riak が動作しており、HTTP エンドポイントがポート 8098 をリッスンしていること、インデックス能力のあるストレージバックエンドが使えるように設定されていることを確実にしてください。さらに `curl` が必要です。

## オブジェクトをインデックス

以下のサンプルは 4 つの異なるオブジェクトをインデックス化します。ここではバイナリと整数フィールドの両方をソートし、フィールド名は自動的に小文字化され、フィールドによっては複数の値があり、重複したフィールドは自動的に de-duplicate されます。

```bash
curl -v -XPUT \
-d 'data1' \
-H "x-riak-index-field1_bin: val1" \
-H "x-riak-index-field2_int: 1001" \
http://127.0.0.1:8098/riak/mybucket/mykey1

curl -v -XPUT \
-d 'data2' \
-H "x-riak-index-Field1_bin: val2" \
-H "x-riak-index-Field2_int: 1002" \
http://127.0.0.1:8098/riak/mybucket/mykey2

curl -v -XPUT \
-d 'data3' \
-H "X-RIAK-INDEX-FIELD1_BIN: val3" \
-H "X-RIAK-INDEX-FIELD2_INT: 1003" \
http://127.0.0.1:8098/riak/mybucket/mykey3

curl -v -XPUT \
-d 'data4' \
-H "x-riak-index-field1_bin: val4, val4, val4a, val4b" \
-H "x-riak-index-field2_int: 1004, 1004, 1005, 1006" \
-H "x-riak-index-field2_int: 1004" \
-H "x-riak-index-field2_int: 1004" \
-H "x-riak-index-field2_int: 1004" \
-H "x-riak-index-field2_int: 1007" \
http://127.0.0.1:8098/riak/mybucket/mykey4
```

以下のサンプルはインデックスフィールドが不正な名前、またはタイプで指定されたときに何が起きるかを表したものです。システムは `400 Bad Request` と答え、エラーを表します。


不正なフィールド名:

```
curl -XPUT \
-d 'data1' \
-H "x-riak-index-field2_foo: 1001" \
http://127.0.0.1:8098/riak/mybucket/mykey

# レスポンス
Unknown field type for field: 'field2_foo'.
```

データタイプ不正:

```
curl -XPUT \
-d 'data1' \
-H "x-riak-index-field2_int: bar" \
http://127.0.0.1:8098/riak/mybucket/mykey

# レスポンス
Could not parse field 'field2_int', value 'bar'.
```

## 正常なクエリ

以下のサンプルは、HTTP インタフェースを用いてインデックスクエリに一致したときの様子です。

```bash
# Query a binary index...
curl http://localhost:8098/buckets/mybucket/index/field1_bin/val1

# Query an integer index...
curl http://localhost:8098/buckets/mybucket/index/field2_int/1001
```

以下のサンプルは、クエリに一致した結果を MapReduce ジョブにパイプしたときの様子です。

```bash
curl -X POST \
-H "content-type: application/json" \
-d @- \
http://localhost:8098/mapred \
<<EOF
{
   "inputs":{
       "bucket":"mybucket",
       "index":"field1_bin",
       "key":"val3"
   },
   "query":[
      {
         "reduce":{
            "language":"erlang",
            "module":"riak_kv_mapreduce",
            "function":"reduce_identity",
            "keep":true
         }
      }
   ]
}
EOF
```

## 範囲クエリ

以下のサンプルは、HTTP インタフェースを用いて範囲クエリを行ったときの様子です。

```bash
# Query a binary index...
curl http://localhost:8098/buckets/mybucket/index/field1_bin/val2/val4

# Query an integer index...
curl http://localhost:8098/buckets/mybucket/index/field2_int/1002/1004
```

以下のサンプルは、範囲クエリの結果を MapReduce ジョブへパイプしたときの様子です。

```bash
curl -X POST \
-H "content-type: application/json" \
-d @- \
http://localhost:8098/mapred \
<<EOF
{
   "inputs":{
       "bucket":"mybucket",
       "index":"field1_bin",
       "start":"val2",
       "end":"val4"
   },
   "query":[
      {
         "reduce":{
            "language":"erlang",
            "module":"riak_kv_mapreduce",
            "function":"reduce_identity",
            "keep":true
         }
      }
   ]
}
EOF
```
