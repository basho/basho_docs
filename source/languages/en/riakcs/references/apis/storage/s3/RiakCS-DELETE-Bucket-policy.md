---
title: RiakCS DELETE Bucket policy
project: riakcs
version: 1.3.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
---

The `DELETE Bucket policy` operation deletes the `policy` subresource of an existing bucket. To perform this operation, you must be the bucket owner.

## Requests

### Request Syntax

```
DELETE /?policy HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signatureValue

```

### Request Parameters

This operation does not use request parameters.

### Request Headers

This operation uses only request headers that are common to all operations. For more information, see [[Common RiakCS Request Headers]].

### Request Elements

No body should be appended.

## Response

### Response Headers

This implementation of the operation uses only response headers that are common to most responses. For more information, see [[Common RiakCS Response Headers]].

### Response Elements

`DELETE` response elements return whether the operation succeeded or not.

## Examples

### Sample Request

The following request shows the DELETE individual policy request for the bucket.

```
DELETE /?policy HTTP/1.1
Host: bucketname.data.basho.com
Date: Tue, 04 Apr 2010 20:34:56 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=

```

### Sample Response

```
HTTP/1.1 204 No Content
Date: Tue, 04 Apr 2010 12:00:01 GMT
Connection: keep-alive
Server: Riak CS
```
