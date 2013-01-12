---
title: HTTP バケットのプロパティを得る
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Bucket Operations"
---

バケットのプロパティを読む

## リクエスト

```bash
GET /riak/bucket                # 旧フォーマット
GET /buckets/bucket/props       # 新フォーマット
```

オプション クエリ パラメータ (旧フォーマットでのみ有効):

* `props` - バケットプロパティを返す (デフォルトは `true`)
* `keys` - バケットに格納されているキーを返す (デフォルトは `false`)　[[HTTP List Keys|HTTP List Keys]] 参照

## レスポンス

正常ステータスコード:

* `200 OK`

重要なヘッダ:

* `Content-Type` - `application/json`

レスポンス内の JSON オブジェクトは、`"props"` と `"keys"` というエントリを2つまで含むことができます。オプション クエリ パラメータで、付けるか付けないかのいずれかです。デフォルトは `"props"` だけが付けられています。

有効なバケットプロパティの詳細は [[HTTP バケットプロパティのセット|HTTP Set Bucket Properties]] を参照してください。

## サンプル

```bash
$ curl -v http://127.0.0.1:8098/riak/test
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /riak/test HTTP/1.1
> User-Agent: curl/7.19.7 (universal-apple-darwin10.0) libcurl/7.19.7
OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
>
< HTTP/1.1 200 OK
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/json
< Content-Length: 368
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
{"props":{"name":"test","n_val":3,"allow_mult":false,"last_write_wins":false,"
precommit":[],"postcommit":[],"chash_keyfun":{"mod":"riak_core_util","fun":"
chash_std_keyfun"},"linkfun":{"mod":"riak_kv_wm_link_walker","fun":"
mapreduce_linkfun"},"old_vclock":86400,"young_vclock":20,"big_vclock":50,"
small_vclock":10,"r":"quorum","w":"quorum","dw":"quorum","rw":"quorum"}}
```
