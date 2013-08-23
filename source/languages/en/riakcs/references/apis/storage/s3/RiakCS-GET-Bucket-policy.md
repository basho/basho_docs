---
title: RiakCS GET Bucket policy
project: riakcs
version: 1.3.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
moved: {
  '1.4.0-': '/references/apis/storage/RiakCS-GET-Bucket-policy'
}
---

The `GET Bucket policy` operation uses the `policy` subresource to fetch the policy currently set to an existing bucket. If the bucket does not have a policy the call ends up in 404 Not Found. To perform this operation, you must be the bucket owner.

## Requests

### Request Syntax

This example shows the syntax for setting the policy in the request body.

```
GET /?policy HTTP/1.1
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

`GET` response has a JSON which was PUT as its body.

## Examples

### Sample Request

The following request shows the GET individual policy request for the bucket.

```
GET /?policy HTTP/1.1
Host: bucketname.data.basho.com
Date: Tue, 04 Apr 2010 20:34:56 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=
Content-size: 0

```

### Sample Response

```
HTTP/1.1 200 OK
Date: Tue, 04 Apr 2010 12:00:01 GMT
Connection: keep-alive
Server: Riak CS
Content-size: 256

{
  "Version": "2008-10-17",
  "Statement": [
    {
      "Sid": "Stmtaaa",
      "Effect": "Allow",
      "Principal": "*",
      "Action": ["s3:GetObjectAcl","s3:GetObject"],
      "Resource": "arn:aws:s3:::bucketname/*",
      "Condition": {
        "IpAddress": {
          "aws:SourceIp": "127.0.0.1/32"
        }
      }
    }
  ]
}
```
