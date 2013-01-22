---
title: HTTP バケットのリスト
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Bucket Operations"
---

既知の全バケット (キーが格納されている物) をリストする

<div class="note"><div class="title">プロダクションに使用してはならない</div>
<p>キーのリスト操作と同様で、クラスタに格納されている全てのキーを走査する。プロダクションに使用してはならない。</p>
</div>

## リクエスト

```bash
GET /riak?buckets=true       # 旧フォーマット
GET /buckets?buckets=true    # 新フォーマット
```

クエリパラメータが必要:

* **buckets=true** - バケットのリストを取るために必要

## レスポンス

正常ステータスコード:
* 200 OK

重要なヘッダ:
* Content-Type - application/json

レスポンス内の JSON オブジェクトには、"buckets" という1つのエントリがあり、この中にバケット名が配列となっています。

## サンプル

```bash
$ curl -i http://localhost:8098/riak?buckets=true
HTTP/1.1 200 OK
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
Link: </riak/files>; rel="contained"
Date: Fri, 30 Sep 2011 15:24:35 GMT
Content-Type: application/json
Content-Length: 21

{"buckets":["files"]}
```
