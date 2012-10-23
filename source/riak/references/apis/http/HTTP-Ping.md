---
title: HTTP Ping
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Server Operations"
---

サーバが生きているかをチェックする。モニタリングツール、ロードバランサ、自動スクリプトなどで使用できる。

## リクエスト

```bash
GET /ping
```

## レスポンス

正常ステータスコード:

* `200 OK`

## サンプル

```bash
curl -v http://127.0.0.1:8098/ping
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /ping HTTP/1.1
> User-Agent: curl/7.19.7 (universal-apple-darwin10.0) libcurl/7.19.7 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
>
< HTTP/1.1 200 OK
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: text/html
< Content-Length: 2
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
OK
```
