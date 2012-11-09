---
title: PBC API
project: riak
version: 1.2+
document: api
toc: true
audience: advanced
keywords: [api, protocol-buffer]
index: true
---

これは Riak のプロトコル バッファ クライアント(PBC)インタフェースを使ってできる操作の概要を紹介し、また、クライアントを開発するときのガイドとすることができます。

## プロトコル

Riak はインカミング コネクションとして TCP ポート(デフォルトは 8087)をリッスンしています。
コネクションが成立すると、クライアントは同じポートを使ってリクエストのストリームを送信できます。

それぞれのオペレーションは、1 つのリクエストメッセージと、1 つ以上の応答メッセージから成ります。
すべてのメッセージは同じ方法でエンコードされます。
* ネットワークバイトオーダの、32 ビット長のメッセージコード ＋ プロトコルバッファ メッセージ
* プロトコルバッファ メッセージを表す 8 ビット メッセージ コード
* プロトコルバッファのエンコード済みメッセージ N バイト

### サンプル


```bash
00 00 00 07 09 0A 01 62 12 01 6B
|----Len---|MC|----Message-----|

Len = 0x07
Message Code (MC) = 0x09 = RpbGetReq
RpbGetReq Message = 0x0A 0x01 0x62 0x12 0x01 0x6B

Decoded Message:
bucket: "b"
key: "k"
```


### メッセージ コード

<table>
<tr><td>0</td><td>RpbErrorResp</td></tr>
<tr><td>1</td><td>RpbPingReq</td></tr>
<tr><td>2</td><td>RpbPingResp</td></tr>
<tr><td>3</td><td>RpbGetClientIdReq</td></tr>
<tr><td>4</td><td>RpbGetClientIdResp</td></tr>
<tr><td>5</td><td>RpbSetClientIdReq</td></tr>
<tr><td>6</td><td>RpbSetClientIdResp</td></tr>
<tr><td>7</td><td>RpbGetServerInfoReq</td></tr>
<tr><td>8</td><td>RpbGetServerInfoResp</td></tr>
<tr><td>9</td><td>RpbGetReq</td></tr>
<tr><td>10</td><td>RpbGetResp</td></tr>
<tr><td>11</td><td>RpbPutReq</td></tr>
<tr><td>12</td><td>RpbPutResp</td></tr>
<tr><td>13</td><td>RpbDelReq</td></tr>
<tr><td>14</td><td>RpbDelResp</td></tr>
<tr><td>15</td><td>RpbListBucketsReq</td></tr>
<tr><td>16</td><td>RpbListBucketsResp</td></tr>
<tr><td>17</td><td>RpbListKeysReq</td></tr>
<tr><td>18</td><td>RpbListKeysResp</td></tr>
<tr><td>19</td><td>RpbGetBucketReq</td></tr>
<tr><td>20</td><td>RpbGetBucketResp</td></tr>
<tr><td>21</td><td>RpbSetBucketReq</td></tr>
<tr><td>22</td><td>RpbSetBucketResp</td></tr>
<tr><td>23</td><td>RpbMapRedReq</td></tr>
<tr><td>24</td><td>RpbMapRedResp</td></tr>
<tr><td>25</td><td>RpbIndexReq <i>(new in 1.2+)</i></td></tr>
<tr><td>26</td><td>RpbIndexResp <i>(new in 1.2+)</i></td></tr>
<tr><td>27</td><td>RpbSearchQueryReq <i>(new in 1.2+)</i></td></tr>
<tr><td>28</td><td>RbpSearchQueryResp <i>(new in 1.2+)</i></td></tr>
</table>


<div class="info"><div class="title">メッセージの定義</div>
<p>すべてのプロトコルバッファ メッセージは、[[riak.proto|https://github.com/basho/riak_pb/blob/master/src/riak.proto]] で定義されています。また、他の .proto ファイルは RiakPB プロジェクトにあります。</p>
</div>


### エラー レスポンス

サーバがリクエストの処理中にエラーが発生すると、リクエストに対するレスポンスではなく、RpbErrorResp というメッセージが返ります(たとえば、RpbGetReq の本来のレスポンスは RpbGetResp です)。エラーメッセージはエラーストリングとエラーコードを含みます。

```bash
message RpbErrorResp {
    required bytes errmsg = 1;
    required uint32 errcode = 2;
}
```

値:

* **errmsg** - 何が悪いかを表す文字列
* **errcode** - 数値によるコード。現在は RIAKC_ERR_GENERAL=1 のみが定義されている

## バケット操作

* [[PBC バケットのリスト|PBC List Buckets]]
* [[PBC キーのリスト|PBC List Keys]]
* [[PBC バケット プロパティを得る|PBC Get Bucket Properties]]
* [[PBC バケット プロパティを設定する|PBC Set Bucket Properties]]

## オブジェクト / キー 操作

* [[PBC オブジェクトのフェッチ|PBC Fetch Object]]
* [[PBC オブジェクトを格納する|PBC Store Object]]
* [[PBC オブジェクトを削除する|PBC Delete Object]]

## クエリ 操作

* [[PBC MapReduce]]
* [[PBC インデックス|PBC Index]]
* [[PBC サーチ|PBC Search]]

## サーバ操作

* [[PBC Ping]]
* [[PBC クライアント ID を得る|PBC Get Client ID]]
* [[PBC クライアント ID を設定する|PBC Set Client ID]]
* [[PBC サーバ情報|PBC Server Info]]
