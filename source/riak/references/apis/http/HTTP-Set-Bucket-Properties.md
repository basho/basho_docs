---
title: HTTP バケットのプロパティ
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Bucket Operations"
---

"n_val" や "allow_mult" といったバケットプロパティをセットする

## リクエスト

```bash
PUT /riak/bucket                # 旧フォーマット
PUT /buckets/bucket/props       # 新フォーマット
```

重要なヘッダ:

* `Content-Type` - `application/json`

リクエストボディは"props"というエントリを1つ含むJSONオブジェクトにすべきである。
変更されていないバケットのプロパティは省略される場合がある。

有効なプロパティ:

* `n_val` (整数 > 0) - このバケット内のオブジェクトのレプリカ数
* `allow_mult` (true または false) - 作成時に兄弟オブジェクトを許すかどうか
(同時アップデート)
* `last_write_wins` (true または false) - 書き込み時にオブジェクト履歴(ベクトルクロック)を無視するかどうか
* `precommit` - [[プレコミットフック|Pre- and Post-Commit Hooks]]
* `postcommit` - [[ポストコミットフック|Pre- and Post-Commit Hooks]]
* `r, w, dw, rw` - バケット内のキーオペレーションでのデフォルト値
有効な値:
  * `"all"` - すべてのノードは応答できなければならない
  * `"quorum"` - (n_val/2) + 1 個のノードが応答できなければならない *デフォルト*
  * `"one"` - 1 と同じ
  * *整数* - n_val 未満でなければならない
* `backend` - `riak_kv_multi_backend` のときに、バケットが使用するバックエンドの名前

他にもプロパティがありますが、通常は変更しません。

<div class="note">
<div class="title">プロパティのタイプ</div>
<p><strong>n_val</strong> や <strong>allow_mult</strong> のように適切なタイプの属性を使用してください。
整数値やブール値ではなく文字列を使用すると、ログの中に次のような奇妙なエラーが残っているかもしれません。
<code>"{badarith,[{riak_kv_util,normalize_rw_value,2},]}"</code>.</p>
</div>

## レスポンス

正常ステータスコード:

* `204 No Content`

主なエラーコード:

* `400 Bad Request` - JSON が不正
* `415 Unsupported Media Type` - リクエストの Content-Type が application/json になっていない

正常終了すれば、空のレスポンスボディが返ります。

## サンプル

```bash
$ curl -v -XPUT -H "Content-Type: application/json" -d '{"props":{"n_val":5}}'
http://127.0.0.1:8098/riak/test
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> PUT /riak/test HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4
OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
> Content-Type: application/json
> Content-Length: 21
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
