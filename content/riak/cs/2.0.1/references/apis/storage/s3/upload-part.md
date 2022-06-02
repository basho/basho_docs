---
title: "Riak CS Upload Part"
description: ""
project: "riak_cs"
project_version: "2.0.1"
toc: true
aliases:
  - /riakcs/2.0.1/references/apis/storage/s3/RiakCS-Upload-Part/
  - /riak/cs/2.0.1/references/apis/storage/s3/RiakCS-Upload-Part/
---

This operation uploads a part in a multipart upload. You must [initiate a multipart upload]({{<baseurl>}}riak/cs/2.0.1/references/apis/storage/s3/initiate-multipart-upload) before you can upload any part. In this operation you provide part data in your request.

## Requests

### Request Syntax

This example shows the syntax for uploading a part in a multipart upload.

```
PUT /ObjectName?partNumber=PartNumber&uploadId=UploadId HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Content-Length: size
Authorization: signatureValue
```

### Request Headers

**Content-Length** - The size of the object in bytes. This header is required.

* *Type*: String
* *Default*: None
* *Constraints*: None

**Content-MD5** - The base64-encoded 128-bit MD5 digest of the message without the headers according to RFC 1864. Although this header is optional, the Content-MD5 header can be used to confirm that the data is the same as what was originally sent.

* *Type*: String
* *Default*: None
* *Constraints*: None

**Expect** - When you use `100-continue` in your application, it doesn't send the request body until it receives an acknowledgment. That way, the body of the message isn't sent if the message is rejected based on the headers.

* *Type*: String
* *Default*: None
* *Valid Values*: 100-continue
* *Constraints*: None

### Request Elements

This operation does not use request elements.

## Response

### Response Headers

This implementation of the operation uses only response headers that are common to most responses. For more information, see [Common Riak CS Response Headers]({{<baseurl>}}riak/cs/2.0.1/references/apis/storage/s3/common-response-headers).

### Response Elements

This operation does not use response elements.

## Examples

### Sample Request

The following `PUT` request uploads part number 1 in a multipart upload. This request includes the upload ID from an [Initiate Multipart Upload]({{<baseurl>}}riak/cs/2.0.1/references/apis/storage/s3/initiate-multipart-upload) request.

```
PUT /large.iso?partNumber=1&uploadId=VXBsb2FkIElEIGZvciA2aWWpbmcncyBteS1tb3ZpZS5tMnRzIHVwbG9hZA HTTP/1.1
Host: os.data.basho.com
Date:  Mon, 1 Nov 2010 20:34:56 GMT
Content-Length: 10485760
Content-MD5: pUNXr/BjKK5G2UKvaRRrOA==
Authorization: AWS AKIAIOSFODNN7EXAMPLE:VGhpcyBtZXNzYWdlIHNpZ25lZGGieSRlbHZpbmc=

[10485760 bytes of object data]
```

### Sample Response

The response includes the `ETag` header. This value must be retained for when you send the [Complete Multipart Upload]({{<baseurl>}}riak/cs/2.0.1/references/apis/storage/s3/complete-multipart-upload) request.

```
HTTP/1.1 200 OK
Date:  Mon, 1 Nov 2010 20:34:56 GMT
ETag: "b54357faf0632cce46e942fa68356b38"
Content-Length: 0
Connection: keep-alive
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```
