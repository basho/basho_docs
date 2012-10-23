---
title: HTTP キーのリスト
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Bucket Operations"
---

バケット内のキーをリストする

<div class="note">
<div class="title">プロダクションで使用してはならない</div>

クラスタに格納されている全てのキーを走査する。プロダクションに使用してはならない。

</div>

## リクエスト

```bash
GET /riak/bucket?keys=true            # 全てのキーをリストする　旧フォーマット
GET /buckets/bucket/keys?keys=true    # 全てのキーをリストする　新フォーマット
GET /riak/bucket?keys=stream          # キーをクライアントにストリームする　旧フォーマット
GET /buckets/bucket/keys?keys=stream  # キーをクライアントにストリームする　新フォーマット
```

必要なクエリパラメータ:

* `keys` - デフォルトは 'false'。'true' の場合は単一のペイロードに全てのキーが返される。'stream' の場合はキーはチャンクに分割されて返される。

オプション クエリ パラメータ:

* `props` - デフォルトは 'true' で、[[バケットのプロパティ|HTTP-Get-Bucket-Properties]] をレスポンスとして返す。'false' の時にはプロパティを返さない。

## レスポンス

正常なレスポンスコード:

* `200 OK`

重要なヘッダ:

* `Content-Type` - `application/json`
* `Transfer-Encoding` - `keys` クエリパラメータが `stream` の時には、`chunked` で返される。

レスポンスの JSON オブジェクトには、クエリパラメータに応じて `"props"` と `"keys"` という2つのエントリを持つことができます。
クエリパラメータが `keys=stream` のときは、`"keys"` というエントリを含む複数のJOSONオブジェクトがチャンクとして返ります。

## サンプル

```bash
$ curl -i http://localhost:8098/riak/jsconf?keys=true\&props=false
HTTP/1.1 200 OK
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
Link: </riak/jsconf/challenge.jpg>; riaktag="contained",
</riak/jsconf/puddi.png>; riaktag="contained", </riak/jsconf/basho.gif>;
riaktag="contained", </riak/jsconf/puddikid.jpg>; riaktag="contained",
</riak/jsconf/yay.png>; riaktag="contained", </riak/jsconf/thinking.png>;
riaktag="contained", </riak/jsconf/victory.gif>; riaktag="contained",
</riak/jsconf/slides>; riaktag="contained", </riak/jsconf/joyent.png>;
riaktag="contained", </riak/jsconf/seancribbs-small.jpg>; riaktag="contained",
</riak/jsconf/trollface.jpg>; riaktag="contained",
</riak/jsconf/riak_logo_animated1.gif>; riaktag="contained",
</riak/jsconf/victory.jpg>; riaktag="contained", </riak/jsconf/challenge.png>;
riaktag="contained", </riak/jsconf/team_cribbs.png>; riaktag="contained"
Date: Fri, 30 Sep 2011 15:24:35 GMT
Content-Type: application/json
Content-Length: 239

{"keys":["challenge.jpg","puddi.png","basho.gif","puddikid.jpg","yay.png","
thinking.png","victory.gif","slides","joyent.png","seancribbs-small.jpg","
trollface.jpg","riak_logo_animated1.gif","victory.jpg","challenge.png","
team_cribbs.png"]}
```
