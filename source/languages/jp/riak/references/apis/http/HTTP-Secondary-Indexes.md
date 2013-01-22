---
title: HTTP セカンダリインデックス
project: riak
version: 1.0.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Query Operations"
---

[[セカンダリインデックス|Secondary Indexes]] はRiakオブジェクトのフィールド/バリュー ペアをタグとしてアプリケーションから利用することができます。
オブジェクトはこれらのフィールド/バリュー ペアを元にしてインデックスが作られ、アプリケーションからの問い合わせによって一致するキーを検索します。

## リクエスト

完全一致:

```bash
GET /buckets/mybucket/index/myindex_bin/value
```

範囲検索:
```
GET /buckets/mybucket/index/myindex_bin/start/end
```

## レスポンス

正常ステータスコード:

+ `200 OK`

主なエラーコード:

+ `400 Bad Request` - インデックス名または値が不正
+ `500 Internal Server Error` - map または reduce 処理中にエラーが発生した、あるいはシステムがインデックスをサポートしていない
+ `503 Service Unavailable` - ジョブが完了前にタイムアウトした

## サンプル

```bash
$ curl -v http://localhost:8098/buckets/mybucket/index/field1_bin/val1
* About to connect() to localhost port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to localhost (127.0.0.1) port 8098 (#0)
> GET /buckets/mybucket/index/field1_bin/val1 HTTP/1.1
> User-Agent: curl/7.19.7 (universal-apple-darwin10.0) libcurl/7.19.7 OpenSSL/0.9.8r zlib/1.2.3
> Host: localhost:8098
> Accept: */*
>
< HTTP/1.1 200 OK
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/json
< Content-Length: 19
<
* Connection #0 to host localhost left intact
* Closing connection #0
{"keys":["mykey1"]}%
```
