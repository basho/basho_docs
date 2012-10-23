---
title: アクセスコントロールリスト
project: riakcs
version: 1.2.0+
document: tutorial
toc: true
index: true
audience: beginner
keywords: [installing]
---

アクセスコントロールリスト (ACL) は、バケットやオブジェクトにアクセス権を与えたり、取り上げたりするためのものです。各バケットやオブジェクトは ACL を持っています。バケットやオブジェクトが作成されたときには、それを作成したグループへフルコントロールを、他のグループへはアクセス禁止がデフォルト ACL として割り当てられています。Riak CS の ACL は S3 の ACL を元にしています。より詳しいことは、Amazon の [[Access Control List Overview|http://docs.amazonwebservices.com/AmazonS3/latest/dev/ACLOverview.html]] ドキュメントを参照してください。

## 記述法
ACL とのやり取りできるフォーマットは XML のみです。将来的には [[JSON|http://www.json.org]] もサポートする予定です。

XML による ACL の記述例:

```xml
<xml version="1.0" encoding="UTF-8">
<AccessControlPolicy xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
  <Owner>
    <ID>abcd123</ID>
    <DisplayName>joebob</DisplayName>
  </Owner>
  <AccessControlList>
    <Grant>
      <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Canonical User">
        <ID>abcd123</ID>
        <DisplayName>joebob</DisplayName>
      </Grantee>
      <Permission>FULL_CONTROL</Permission>
    </Grant>
  </AccessControlList>
</AccessControlPolicy>
```

## パーミッション

### バケットのパーミッション
* **READ** - バケットのオブジェクトのリストを得られます
* **READ_ACP** - バケットの ACL を読むことができます
* **WRITE** - バケット内のすべてのオブジェクトを、作成し、書き換え、削除することができます
* **WRITE_ACP** - アプリケーションのバケットのために ACL を書きこむことができます
* **FULL_CONTROL** - バケットに対して **READ**、**WRITE**、**READ_ACP**、**WRITE_ACP** のパーミッションを持ちます

### オブジェクトのパーミッション
* **READ** - オブジェクトのデータおよびそのメタデータを読むことができます
* **READ_ACP** - オブジェクトの ACL を読むことができます  **ノート:** オブジェクトのオーナは、**READ_ACP** パーミッションを明示的に与えなくても、ACL を読むことができます
* **WRITE_ACP** - 適切なオブジェクトの ACL を書きこむ事ができます  **ノート:** オブジェクトのオーナは、**WRITE_ACP** パーミッションを明示的に与えなくても、ACL に書きこむことができます
* **FULL_CONTROL** - オブジェクトに対して **READ**、**READ_ACP**、**WRITE_ACP** のパーミッションを持ちます

## バケット
バケット名は全体的にユニーク **でなければいけません**。競合を避けるために、すべてのバケット作成リクエストは、**stanchion** と呼ばれるアプリケーションで行われます。このため、バケット ACL を変更するすべてのリクエストは **stanchion** 経由でシリアル化 **されるべき** です。これらのリクエストを望ましくないシリアル化を引き起こすかもしれませんが、Amazon による [[バケット上の規制についてのドキュメント|http://docs.amazonwebservices.com/AmazonS3/2006-03-01/dev/BucketRestrictions.html]] で述べられている、バケット操作の制限に該当すると考えています。

<blockquote>バケット操作は集中化、全体的なリソーススペースを妨害するので、アプリケーション上で高可用性を想定してバケットの作成や削除を呼び出すのは望ましいことではありません。</blockquote>

この条項は作成および削除呼び出しにのみ直接関わります。しかし私たちは、ACL 変更のリクエストも含むように、より幅広い解釈を行いました。

## オブジェクト
オブジェクトの ACL は各オブジェクトのメタデータフィールドに格納されます。オブジェクト作成リクエストに ACL 情報がなければ、デフォルトの ACL として、作成者は所有権とフルアクセスコントロールを持ち、他のグループはアクセス禁止が与えられます。
