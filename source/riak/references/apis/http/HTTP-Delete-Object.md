---
title: HTTP オブジェクトを削除する
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Object/Key Operations"
---

指定したバケット / キーからオブジェクトを削除する

## リクエスト

```
DELETE /riak/bucket/key           # 旧フォーマット
DELETE /buckets/bucket/keys/key   # 新フォーマット
```

オプション クエリ パラメータ:

* `rw` - get および put の両方で、削除するオブジェクト(デフォルトはバケットレベル)に関する指定を行う
* `r` - オブジェクトの取得が成功するために必要なレプリカの数(read quorum)
* `pr` - 'r'と同様だが、読み出すノードはフォールバックノードであってはならない(primary read quorum)
* `w` - 書き込みに成功するために必要なレプリカの数(write quorum)
* `dw` - 書き込みが成功するために、確実に書き込めたレプリカの数(durable write quorum)
* `pw` - 書き込みが成功するために、プライマリノードに書き込めたレプリカの数(primary write quorum)

## レスポンス

<div class="note"><div class="title">クライアントID</div>
<p>Riak &lt;1.0 または、'vnode_vclocks' が有効でない Riak 1.0 には 'X-Riak-ClientId' ヘッダをインクルードするべきです。
これはクライアントをユニークに識別できればどんな文字列でも構いません。
これは [[ベクトルクロック|Vector Clocks]] がオブジェクトの変化を追跡するために用いられます。</p>
</div>

正常時のレスポンス:

* `204 No Content`
* `404 Not Found`

主なエラーコード:

* `400 Bad Request` - rw パラメータが不正 (> N) なときなど

`404` は「正常」な戻り値です。というのも、DELETEオペレーションの結果オブジェクトは削除されるので、リソースが見つからなかったのと等価だからです。

## サンプル

```bash
$ curl -v -X DELETE http://127.0.0.1:8098/riak/test/test2
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> DELETE /riak/test/test2 HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
>
< HTTP/1.1 204 No Content
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/json
< Content-Length: 0
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
```
