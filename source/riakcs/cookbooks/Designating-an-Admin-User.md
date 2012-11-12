---
title: 管理ユーザを任命する
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator]
---

ユーザを作成したら、`app.config` 内のユーザの資格証明である `admin_key` と `admin_secret` を書き換えて、ユーザを管理者にします。これが完了したら、Stanchion の `app.config` 内の資格証明を同様に更新することを忘れないでください。

<div class="note">
<div class="title">ノート</div>
これは強力な権限で、指定された者にシステムの管理能力を託すものです。管理ユーザの資格証明に不用意にアクセスされないよう、十分注意を行ってください。
</div>
