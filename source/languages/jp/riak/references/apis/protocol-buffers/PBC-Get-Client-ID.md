---
title: PBC クライアントID取得
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Server Operations"
---

このコネクションで使用しているクライアント ID を取得します。
クライアント ID は競合を解決するために使われ、システムによって割り当てられたユニークな値です。
クライアント ID は、ソケットがコネクトしたときにランダムに割り当てられ、[[クライアント ID のセット|PBC Set Client ID]] で変更することができます。

<div class="note"><div class="title">Riak 1.0 におけるクライアントID</div>
<p>Riak 1.0 より前、および Riak 1.0 への全てのリクエストは、<code>vnode_vclocks</code> が有効なときを除いて、クライアント ID をセットすべきで、これはクライアントがユニークに識別できればどんな文字列でも構いません。これは [[ベクタークロック|Vector Clocks]] でオブジェクトの変更を追跡するためのものです。</p>
</div>

## リクエスト

これは RpbGetClientIdReq メッセージであり、リクエストメッセージは定義されていません。

## レスポンス


```bash
// Get ClientId Request - no message defined, just send RpbGetClientIdReq
message code
message RpbGetClientIdResp {
    required bytes client_id = 1; // Client id in use for this connection
}
```


## サンプル

リクエスト

```bash
Hex     00 00 00 01 03
Erlang  <<0,0,0,1,3>>
```


レスポンス

```bash
Hex     00 00 00 07 04 0A 04 01 65 01 B5
Erlang <<0,0,0,7,4,10,4,1,101,1,181>>

RpbGetClientIdResp protoc decode:
client_id: "001e001265"

```
