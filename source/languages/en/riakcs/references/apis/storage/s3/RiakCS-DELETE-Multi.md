---
title: RiakCS DELETE Multiple Objects
project: riakcs
version: 1.5.0+
document: api
index: false
audience: advanced
keywords: [api, http]
---

Multi-object `DELETE` enables you to delete multiple objects from a
bucket at the same time if those objects exist. Multi-object `DELETE`s
require you to `POST` an XML object to Riak CS specifying object key
and version information, as in the example below.

## Requests

### Request Syntax

```
POST /?delete HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Content-Length: length
Authorization: signature_value

<?xml version="1.0" encoding="UTF-8"?>
<Delete>
    <Quiet>true</Quiet>
    <Object>
         <Key>Key</Key>
         <VersionId>VersionId</VersionId>
    </Object>
    <Object>
         <Key>Key</Key>
    </Object>
    ...
</Delete>
```