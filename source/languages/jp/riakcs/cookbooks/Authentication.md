---
title: 認証
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator]
---

現在、Riak CS で使用できる唯一の認証スキーマは、S3 認証スキーマです。署名は、各リクエスト内のいくつかの要素、およびユーザの `key_id` と `key_secret` から計算されます。この署名はリクエストの Authorization ヘッダに含まれます。サーバはリクエストを受け取ると、署名を再計算し、Authorization ヘッダで受け取った署名と比較します。両者が一致すれば、リクエストは認証されます。そうでなければ、認証は失敗です。

完全な詳細は [S3 の認証スキーマのドキュメント](http://docs.amazonwebservices.com/AmazonS3/latest/dev/RESTAuthentication.html) にあります。

Riak CS はクエリのパラメータによる認証もサポートしています。これも上記の URL の、Amazon の認証ドキュメントで説明されています。

これは、事前に署名済みのリクエストであれば、個人的な Riak CS のデータへ、パブリックにアクセス許可を与える基本的な方法です。タイムスタンプの有効期間もサポートしているので、事前署名の URL は一定の時間が経過後、無効とすることができます。
