---
title: "Riak CS PUT Bucket Policy"
description: ""
project: "riak_cs"
project_version: "2.0.0"
toc: true
aliases:
  - /riakcs/2.0.0/references/apis/storage/s3/RiakCS-PUT-Bucket-policy/
  - /riak/cs/2.0.0/references/apis/storage/s3/RiakCS-PUT-Bucket-policy/
---

The `PUT Bucket policy` operation uses the `policy` subresource to add or replace the policy on an existing bucket. If the bucket already has a policy, the one in this request completely replaces it. To perform this operation, you must be the bucket owner.

{{% note title="Note" %}}
Currently only the `aws:SourceIp` and `aws:SecureTransport` policy conditions
are supported.
{{% /note %}}

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

This operation uses only request headers that are common to all operations. For more information, see [Common Riak CS Request Headers]({{<baseurl>}}riak/cs/2.0.0/references/apis/storage/s3/common-request-headers).

### Request Elements

The body is a JSON string containing the policy elements. The supported policy elements are detailed below.

#### Version

The Version element specifies the policy language version. If a version is not specified, this defaults to `2008-10-17`.

#### ID

The Id element specifies an optional identifier for the policy.

#### Statement

The Statement element is the main element for a policy. This element is required. It can include multiple elements. The Statement element contains an array of individual statements. Each individual statement is a JSON block enclosed in braces, i.e. `{ ... }`. Below is a list of currently supported statements.

**SID** : The Sid (statement ID) is an optional identifier that you provide for the policy statement. You can assign a Sid value to each statement in a statement array.

**Effect** : The Effect element is required and specifies whether the statement will result in an allow or an explicit deny. Valid values for Effect are `Allow` and `Deny`.

**Principal** : The Principal element specifies the user, account, service, or other entity that is allowed or denied access to a resource. Currently, Riak CS only supports the `*` principal type.

**Action** : The Action element describes the type of access that should be allowed or denied.

Supported actions are:

- `s3:GetObject`, `s3:PutObject`, `s3:DeleteObject`,
- `s3:GetObjectAcl`, `s3:PutObjectAcl`,
- `s3:ListMultipartUploadParts`, `s3:AbortMultipartUpload`,
- `s3:CreateBucket`, `s3:DeleteBucket`, `s3:ListBucket`, `s3:ListAllMyBuckets`,
- `s3:GetBucketAcl`, `s3:PutBucketAcl`,
- `s3:GetBucketPolicy`, `s3:DeleteBucketPolicy`, `s3:PutBucketPolicy`,
- `s3:ListBucketMultipartUploads` .

**Resource** : The Resource element specifies the object or objects that the statement covers. Currently, Riak only supports buckets as resources, specified as: `"arn:aws:s3:::<BUCKET_NAME>/*"`.

**Condition** : The Condition element (or Condition block) lets you specify conditions for when a policy is in effect. The Condition element is optional.Riak CS supports 3 Condition Types: `Bool`, `IpAddress`, and `NotIpAddress`.

Riak CS supports two keys to be used with these conditions: `aws:SecureTransport` and `aws:SourceIp`. `aws:SecureTransport` is used with the `Bool` condition to check whether the request was sent with SSL. Accepted values for this key are `true` and `false`. `aws:SourceIp` is used with the `IpAddress` and `NotIpAddress` conditions, and represents the requester's IP address. IPv4 IP addresses in CIDR notation are supported.

The IP address to be compared with `IpAddress` or `NotIpAddress` is taken from the source IP address of the TCP connection. If Riak CS is behind a load balancer that does not preserve source IP address and bucket policies related to IP addresses, Riak CS can be configured to adopt IP address described in the `X-Forwarded-For` request header, which is added by the load balancer. Set `trust_x_forwarded_for` to `true` in `app.config` when the header can be trusted and secure. The default is `false`, where Riak CS uses the source IP address of the TCP connection.

More information on S3 Policies can be found in Amazon's [Permissions And Policies](http://docs.aws.amazon.com/IAM/latest/UserGuide/PermissionsAndPolicies.html) documentation.


## Response

### Response Headers

This implementation of the operation uses only response headers that are common to most responses. For more information, see [Common Riak CS Response Headers]({{<baseurl>}}riak/cs/2.0.0/references/apis/storage/s3/common-response-headers).

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
