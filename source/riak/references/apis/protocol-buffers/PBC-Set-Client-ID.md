---
title: PBC クライアント ID のセット
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Server Operations"
---

このコネクションのためのクライアント ID をセットします。
再コネクトのときにユニークに要素を識別できるうまい方法があれば、ライブラリはクライアント ID をセットしたほうが良いでしょう。
これによってベクタークロックの肥大化を抑えることができます。

<div class="note"><div class="title">1.0 でのクライアント ID</div>
<p>Riak &lt; 1.0 または <code>vnode_clocks</code> を使わない Riak 1.0 への全てのリクエストは
クライアント ID をセットすべきです。これは [[ベクタークロック|Vector Clocks]] としてオブジェクトの変化を追跡するために、
クライアントをユニークに識別できるどんな文字列でも構いません。</p>
</div>

## リクエスト


```bash
message RpbSetClientIdReq {
    required bytes client_id = 1; // Client id to use for this connection
}
```


## レスポンス

RpbSetClientIdResp メッセージコードのみです。

## サンプル

リクエスト

```bash
Hex      00 00 00 07 05 0A 04 01 65 01 B6
Erlang <<0,0,0,7,5,10,4,1,101,1,182>>

RpbSetClientIdReq protoc decode:
client_id: "001e001266"

```


レスポンス

```bash
Hex      00 00 00 01 06
Erlang <<0,0,0,1,6>>

RpbSetClientIdResp - メッセージコードのみが示されます。
```