---
title: HTTP リンクウォーキング
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Query Operations"
---

リンクウォーキングは、バケットとキーの部分で指定されたオブジェクトからリンクを辿っていき、オブジェクトを返します。
これは [[MapReduce]] の特殊なケースで、より細かく処理を行うことができます。[[リンクに付いての詳細|Links]]

## リクエスト

```bash
GET /riak/bucket/key/[bucket],[tag],[keep]            # 旧フォーマット
GET /buckets/bucket/keys/key/[bucket],[tag],[keep]    # 新フォーマット
```

<div class="info"><div class="title">リンクフィルタ</div>
<p>リクエストURL内のリンクフィルタはカンマで3つの部分に分けられます。</p>

<ul>
<li>Bucket - リンクの範囲を制限するためのバケット名</li>
<li>Tag - リンクを制限するための "rinktag"</li>
<li>Keep - 0 または 1、このフェーズからリザルトを返すかどうか</li>
</ul>

<p>これら3つの部分は <code>_</code> (アンダースコア) に置き換えることができ、これはどんな値でも良いことを意味します。
URLの後ろにパスを続けてリンクの複数のフェーズを指定することができます。リンクフィルタはスラッシュで区切ります。
リンクウォーキング クエリの最後のフェーズで、暗黙のうちにリザルトが返されます。</p>
</div>

## レスポンス

正常ステータスコード:

* `200 OK`

主なエラーコード:

* `400 Bad Request` - URL内のクエリのフォーマットが不正
* `404 Not Found` - リンクウォークを開始するオブジェクトがない

重要なヘッダ:

* `Content-Type` - 常に `multipart/mixed` とし、境界を指定する

<div class="note"><div class="title">レスポンスボディについて</div>
<p>レスポンスボディは常に <code>multipart/mixed</code> で、各チャンクは1回のリンクウォーキング クエリの結果となります。
各フェーズは <code>multipart/mixed</code> としてエンコードされ、各チャンクが1つのオブジェクトとなります。
オブジェクトが見つからない、あるいは "keep" がセットされていなければ、そのフェーズにチャンクは現れません。
フェーズのリザルト内には <code>Location</code> というヘッダでバケットとキーが決定できます。
実際に各オブジェクトのチャンクを、ステータスコードなしで [[オブジェクトのフェッチ|HTTP FETCH OBJECT]] するのと同様に完全なレスポンスを得ることができます。</P>
</div>

## サンプル

```bash
$ curl -v http://127.0.0.1:8098/riak/test/doc3/test,_,1/_,next,1
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /riak/test/doc3/test,_,1/_,next,1 HTTP/1.1
> User-Agent: curl/7.19.4 (universal-apple-darwin10.0) libcurl/7.19.4 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: */*
>
< HTTP/1.1 200 OK
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Expires: Wed, 10 Mar 2010 20:24:49 GMT
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: multipart/mixed; boundary=JZi8W8pB0Z3nO3odw11GUB4LQCN
< Content-Length: 970
<

--JZi8W8pB0Z3nO3odw11GUB4LQCN
Content-Type: multipart/mixed; boundary=OjZ8Km9J5vbsmxtcn1p48J91cJP

--OjZ8Km9J5vbsmxtcn1p48J91cJP
X-Riak-Vclock: a85hYGDgymDKBVIszMk55zKYEhnzWBlKIniO8kGF2TyvHYIKf0cIszUnMTBzHYVKbIhEUl+VK4spDFTPxhHzFyqhEoVQz7wkSAGLMGuz6FSocFIUijE3pt7HlGBhnqejARXmq0QyZnnxE6jwVJBwFgA=
Location: /riak/test/doc
Content-Type: application/json
Link: </riak/test>; rel="up", </riak/test/doc2>; riaktag="next"
Etag: 3pvmY35coyWPxh8mh4uBQC
Last-Modified: Wed, 10 Mar 2010 20:14:13 GMT

{"riak":"CAP"}
--OjZ8Km9J5vbsmxtcn1p48J91cJP--

--JZi8W8pB0Z3nO3odw11GUB4LQCN
Content-Type: multipart/mixed; boundary=RJKFlAs9PrdBNfd74HANycvbA8C

--RJKFlAs9PrdBNfd74HANycvbA8C
X-Riak-Vclock: a85hYGBgzGDKBVIsbLvm1WYwJTLmsTLcjeE5ypcFAA==
Location: /riak/test/doc2
Content-Type: application/json
Link: </riak/test>; rel="up"
Etag: 6dQBm9oYA1mxRSH0e96l5W
Last-Modified: Wed, 10 Mar 2010 18:11:41 GMT

{"foo":"bar"}
--RJKFlAs9PrdBNfd74HANycvbA8C--

--JZi8W8pB0Z3nO3odw11GUB4LQCN--
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
```
