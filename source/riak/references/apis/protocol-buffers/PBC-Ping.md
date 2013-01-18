---
title: PBC Ping
project: riak
version: 0.14.0+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
group_by: "Server Operations"
---

サーバが生きているかをチェックする

## リクエスト

RpbPingReq メッセージだけです。リクエストメッセージは定義されていません。

## レスポンス

RpbPingResp メッセージだけです。レスポンスメッセージは定義されていません。

## サンプル

リクエスト

```bash
Hex    00 00 00 01 01
Erlang <<0,0,0,1,1>>
```

レスポンス

```bash
Hex    00 00 00 01 02
Erlang <<0,0,0,1,2>>
```
