---
title: RiakCS Abort Multipart Upload
project: riakcs
version: 1.3.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
---

Aborts a multipart upload. After a multipart upload is aborted, the storage
consumed by any previously uploaded parts will be freed.

## Requests

### Request Syntax

This example shows the syntax for aborting a multipart upload.

```
DELETE /ObjectName?uploadId=UploadId HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signatureValue
```

### Request Headers

This implementation of the operation uses only response headers that are common to most responses. For more information, see [[Common RiakCS Response Headers]].

### Request Elements

This operation does not use request elements.

## Response

### Response Headers

This implementation of the operation uses only response headers that are common to most responses. For more information, see [[Common RiakCS Response Headers]].

### Response Elements

This operation does not use response elements.

### Special Errors

**NoSuchUpload** - The specified multipart upload does not exist.

## Examples

### Sample Request

The following request aborts a multipart upload identified by its upload ID.

```
DELETE /large.iso?uploadId=VXBsb2FkIElEIGZvciA2aWWpbmcncyBteS1tb3ZpZS5tMnRzIHVwbG9hZA HTTP/1.1
Host: os.data.basho.com
Date:  Mon, 1 Nov 2010 20:34:56 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:0RQf3/cRonhpaBX5sCYVf1bNRuU=
```

### Sample Response

```
HTTP/1.1 204 OK
Date:  Mon, 1 Nov 2010 20:34:56 GMT
Content-Length: 0
Connection: keep-alive
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```
