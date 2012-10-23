---
title: HTTP オブジェクトを取得する
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Object/Key Operations"
---

指定されたバケット / キーからオブジェクトを読み出す

## リクエスト

```bash
GET /riak/bucket/key            # 旧フォーマット
GET /buckets/bucket/keys/key    # 新フォーマット
```

重要なヘッダ:

* `Accept` - content-type が `multipart/mixed` の時、オブジェクトに兄弟があれば1度のリクエストですべての兄弟を返します。[[1つのリクエストで全部を取得する|HTTPFetch Object#Get all siblings in one request]] のサンプルを参照してください。RFC 2616 - [[Accept header definition|http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1]] も参照してください。

オプションヘッダ:

* `If-None-Match` と `If-Modified-Since` は、オブジェクトの `ETag` と `Last-Modified` のそれぞれにマッチする条件付きリクエストです。オブジェクトがテストに通らないとき(つまり、ETag が equal またはオブジェクトが与えられたタイムスタンプの時点から変更されていないとき)、Riakは `304 Not Modified` というレスポンスを返します。RFC 2616 - [[304 Not Modified|http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.5]] も参照してください。

オプション クエリ パラメータ:

* `r` - オブジェクトの取得が成功するために必要なレプリカの数(read quorum)。([[デフォルトはバケットで指定される|HTTPSet Bucket Properties]])
* `pr` - 読み出しの際にオンラインでなければならないプライマリレプリカの数([[デフォルトはバケットで指定される|HTTP Set Bucket Properties]])
* `basic_quorum` - フェイルがいくつか起きたらリターンする(r=1 のときに2つのエラーがあっても、`basic_quorum=true` であれば、エラーを返す)。([[デフォルトはバケットで指定される|HTTP Set Bucket Properties]])
* `notfound_ok` - notfound を読み出し成功とみなす([[デフォルトはバケットで指定される|HTTPSet Bucket Properties]])
* `vtag` - 兄弟のあるオブジェクトにアクセスしたときに、どの兄弟を返すかを指定する。
詳しくは [[Manually requesting siblings|HTTP Fetch Object#Manually requesting siblings]] のサンプルを参照

## レスポンス

正常時のレスポンスコード:

* `200 OK`
* `300 Multiple Choices`
* `304 Not Modified` (条件付きリクエスト使用時)

主なエラーコード:

* `400 Bad Request` - r パラメータが不正 (> N) など
* `404 Not Found` - 十分なパーティション中にオブジェクトが見つからない
* `503 Service Unavailable` - リクエストが内部でタイムエラー

重要なヘッダ:

* `Content-Type` - メディアのタイプ/フォーマット
* `X-Riak-Vclock` - オブジェクトのベクトルクロック
* `X-Riak-Meta-*` - オブジェクトを格納するための、ユーザ定義のメタデータ定義
* `ETag` - オブジェクトの独立タグ、条件付き GET オペレーションとvalidation-basedキャッシュで使用する
* `Last-Modified` - オブジェクトが最後に書きこまれたときの、HTTP datetime フォーマットのタイムスタンプ
* `Link` - ユーザおよびシステム定義の、他のリソースへのリンク [[リンクについての詳細|Links]]

オブジェクトの兄弟があるとき以外は、レスポンスのボディはオブジェクトの内容となります。

<div class="note"><div class="title">兄弟</div>
<p>バケットのプロパティで `allow_mult` が true のとき、"兄弟"オブジェクトの作成を同時更新することができます。つまり、オブジェクトはベクトルクロックに関連した別々の値を持ちます。これによってアプリケーションは、自分で競合回避を行うことができるのです。</p>

<p>複数の兄弟を持つオブジェクトは `300 Multiple Choices` というレスポンスになります。`Accept` ヘッダが `multipart/mixed` を許せば、1つのリクエストのレスポンスボディに `multipart/mixed` セクションとしてすべての兄弟が返されます。あるいは、単純なテキスト形式で "vtags" のリストが返されます。クエリパラメータに `vtag` を追加して、個別の兄弟をリクエストすることができます。以下のサンプルの '兄弟をマニュアルでリクエスト' を参照してください。</p>

<p>競合を解決するためには、レスポンスに `X-Riak-Vclock` を追加してください。</p>
</div>

## 簡単なサンプル

```bash
$ curl -v http://127.0.0.1:8098/riak/test/doc2
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /riak/test/doc2 HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
>
< HTTP/1.1 200 OK
< X-Riak-Vclock: a85hYGBgzGDKBVIsbLvm1WYwJTLmsTLcjeE5ypcFAA==
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Link: </riak/test>; rel="up"
< Last-Modified: Wed, 10 Mar 2010 18:11:41 GMT
< ETag: 6dQBm9oYA1mxRSH0e96l5W
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/json
< Content-Length: 13
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
{"foo":"bar"}
```


## 兄弟のサンプル

### 兄弟をマニュアルでリクエスト

```bash
$ curl -v http://127.0.0.1:8098/riak/test/doc
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /riak/test/doc HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
>
< HTTP/1.1 300 Multiple Choices
< X-Riak-Vclock: a85hYGDgyGDKBVIszMk55zKYEhnzWBlKIniO8kGF2TyvHYIKf0cIszUnMTBzHYVKbIhEUl+VK4spDFTPxhHzFyqhEoVQz7wkSAGLMGuz6FSocFIUijE3pt5HlsgCAA==
< Vary: Accept, Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: text/plain
< Content-Length: 102
<
Siblings:
16vic4eU9ny46o4KPiDz1f
4v5xOg4bVwUYZdMkqf0d6I
6nr5tDTmhxnwuAFJDd2s6G
6zRSZFUJlHXZ15o9CG0BYl
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0

$ curl -v http://127.0.0.1:8098/riak/test/doc?vtag=16vic4eU9ny46o4KPiDz1f
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /riak/test/doc?vtag=16vic4eU9ny46o4KPiDz1f HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
>
< HTTP/1.1 200 OK
< X-Riak-Vclock: a85hYGDgyGDKBVIszMk55zKYEhnzWBlKIniO8kGF2TyvHYIKf0cIszUnMTBzHYVKbIhEUl+VK4spDFTPxhHzFyqhEoVQz7wkSAGLMGuz6FSocFIUijE3pt5HlsgCAA==
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Link: </riak/test>; rel="up"
< Last-Modified: Wed, 10 Mar 2010 18:01:06 GMT
< ETag: 16vic4eU9ny46o4KPiDz1f
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: application/x-www-form-urlencoded
< Content-Length: 13
<
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
{"bar":"baz"}
```

### 1つのリクエストですべての兄弟を得る

```bash
$ curl -v http://127.0.0.1:8098/riak/test/doc -H "Accept: multipart/mixed"
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /riak/test/doc HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: multipart/mixed
>
< HTTP/1.1 300 Multiple Choices
< X-Riak-Vclock: a85hYGDgyGDKBVIszMk55zKYEhnzWBlKIniO8kGF2TyvHYIKf0cIszUnMTBzHYVKbIhEUl+VK4spDFTPxhHzFyqhEoVQz7wkSAGLMGuz6FSocFIUijE3pt5HlsgCAA==
< Vary: Accept, Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: multipart/mixed; boundary=YinLMzyUR9feB17okMytgKsylvh
< Content-Length: 766
<

--YinLMzyUR9feB17okMytgKsylvh
Content-Type: application/x-www-form-urlencoded
Link: </riak/test>; rel="up"
Etag: 16vic4eU9ny46o4KPiDz1f
Last-Modified: Wed, 10 Mar 2010 18:01:06 GMT

{"bar":"baz"}
--YinLMzyUR9feB17okMytgKsylvh
Content-Type: application/json
Link: </riak/test>; rel="up"
Etag: 4v5xOg4bVwUYZdMkqf0d6I
Last-Modified: Wed, 10 Mar 2010 18:00:04 GMT

{"bar":"baz"}
--YinLMzyUR9feB17okMytgKsylvh
Content-Type: application/json
Link: </riak/test>; rel="up"
Etag: 6nr5tDTmhxnwuAFJDd2s6G
Last-Modified: Wed, 10 Mar 2010 17:58:08 GMT

{"bar":"baz"}
--YinLMzyUR9feB17okMytgKsylvh
Content-Type: application/json
Link: </riak/test>; rel="up"
Etag: 6zRSZFUJlHXZ15o9CG0BYl
Last-Modified: Wed, 10 Mar 2010 17:55:03 GMT

{"foo":"bar"}
--YinLMzyUR9feB17okMytgKsylvh--
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
```
