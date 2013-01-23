---
title: HTTP MapReduce
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Query Operations"
---

[[MapReduce]] は入力を指定し、map を作成し、reduce し、データ集めるという、Riak における標準的なクエリ方法です。

## リクエスト

```bash
POST /mapred
```

重要なヘッダ:
* `Content-Type` - 常に `application/json` でなければならない。リクエストボディのフォーマットは [[MapReduce]] のページで説明する。

オプション クエリ パラメータ:
* `chunked` - `true` のときは、リザルトは `multipart/mixed` として、チャンクに分割されて返される。

_+このリクエストは、MapReduceクエリとしてJSONのフォーマットでリクエストを内部(ボディ)に含まねばならない。+_

## レスポンス

正常ステータスコード:
* `200 OK`

主なエラーコード:
* `400 Bad Request` - 無理なジョブが与えられた
* `500 Internal Server Error` - map または reduce 処理中にエラーが発生した
* `503 Service Unavailable` - ジョブ完了前にタイムアウトが発生した

重要なヘッダ:
* `Content-Type` - `chunked` が true でないときは `application/json` とし、そうでないときは `application/json` セクションは `multipart/mixed` となる

## サンプル

```bash
$ curl -v -d '{"inputs":"test", "query":[{"link":{"bucket":"test"}},{"map":{"language":"javascript","name":"Riak.mapValuesJson"}}]}' -H "Content-Type: application/json" http://127.0.0.1:8098/mapred
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> POST /mapred HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
> Content-Type: application/json
> Content-Length: 117
>
< HTTP/1.1 200 OK
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/json
< Content-Length: 30
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
[{"foo":"bar"},{"riak":"CAP"}]
```
