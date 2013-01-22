---
title: RiakCS オブジェクトを削除
project: riakcs
version: 1.2.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
---

`DELETE Object` は、オブジェクトがあれば、それを削除します。

## リクエスト

### リクエストの書式

```
DELETE /ObjectName HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Content-Length: length
Authorization: signature_value
```

## サンプル

### リクエストのサンプル

`projects-schedule.jpg` というオブジェクトを削除します。

```
DELETE /projects-schedule.jpg HTTP/1.1
Host: bucketname.data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 GMT
Authorization: AWS QMUG3D7KP5OQZRDSQWB6:4Pb+A0YT4FhZYeqMdDhYls9f9AM=
```

### レスポンスのサンプル

```
HTTP/1.1 204 No Content
Date: Wed, 06 Jun 2012 20:47:15 GMT
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```
