---
title: RiakCS PUT Bucket policy
project: riakcs
version: 1.3.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
---

The `PUT Bucket policy` operation uses the `policy` subresource to add or replace the policy on an existing bucket. If the bucket already has a policy, the one in this request completely replaces it. To perform this operation, you must be the bucket owner.

<div class="note"><div class="title">Note</div>Currently only the `aws:SourceIp` and `aws:SecureTransport` policy conditions are supported.</div>

## Requests

### Request Syntax

This example shows the syntax for setting the policy in the request body.

```
PUT /?policy HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signatureValue

Policy written in JSON
```

### Request Parameters

This operation does not use request parameters.

### Request Headers

This operation uses only request headers that are common to all operations. For more information, see [[Common RiakCS Request Headers]].

### Request Elements

The body is a JSON string containing the policy contents containing the policy statements.

## Response

### Response Headers

This implementation of the operation uses only response headers that are common to most responses. For more information, see [[Common RiakCS Response Headers]].

### Response Elements

`PUT` response elements return whether the operation succeeded or not.

## Examples

### Sample Request

The following request shows the PUT individual policy request for the bucket.

```
PUT /?policy HTTP/1.1
Host: bucketname.data.basho.com
Date: Tue, 04 Apr 2010 20:34:56 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=

{
  "Version": "2008-10-17",
  "Statement": [
    {
      "Sid": "0xDEADBEEF",
      "Effect": "Allow",
      "Principal": "*",
      "Action": ["s3:GetObjectAcl","s3:GetObject"],
      "Resource": "arn:aws:s3:::bucketname/*",
      "Condition": {
        "IpAddress": {
          "aws:SourceIp": "192.0.72.1/24"
        }
      }
    }
  ]
}
```

### Sample Response

```
HTTP/1.1 204 No Content
Date: Tue, 04 Apr 2010 12:00:01 GMT
Connection: keep-alive
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```
