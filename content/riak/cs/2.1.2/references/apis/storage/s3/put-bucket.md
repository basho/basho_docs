---
title: "Riak CS PUT Bucket"
description: ""
project: "riak_cs"
project_version: "2.1.2"
toc: true
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/RiakCS-PUT-Bucket/
  - /riak/cs/2.1.2/references/apis/storage/s3/RiakCS-PUT-Bucket/
  - /riak/cs/latest/references/apis/storage/s3/put-bucket/
---

The `PUT Bucket` operation creates a new bucket. The user who sends the request to create the bucket becomes the bucket owner. Anonymous requests can't create buckets.

*Note:* To create a bucket, you must have a valid Key ID, which is used to authenticate requests.

## Bucket Naming

A best practice is to use DNS-compliant bucket names. DNS-compliant bucket names ensure that virtual hosted-style requests can locate buckets.

A bucket name must obey the following rules, which produces a DNS-compliant bucket name:

* Must be from 3 to 63 characters.
* Must be one or more labels, each separated by a period (.). Each label:
* Must start with a lowercase letter or a number. Must end with a lowercase letter or a number. Can contain lowercase letters, numbers and dashes.
* Must not be formatted as an IP address (e.g., 192.168.9.2).

## Access Permissions

PUT Bucket offers the option to specify the permissions you want to grant to specific accounts or groups for the bucket. You can grant permissions to accounts or groups with request headers, using one of the following two methods:

* Specify a predefined ACL using the x-amz-acl request header. More information about predefined ACLs is available [here](http://docs.amazonwebservices.com/AmazonS3/latest/dev/ACLOverview.html#CannedACL).
* Specify access permissions explicitly using the x-amz-grant-read, x-amz-grant-write, x-amz-grant-read-acp, x-amz-grant-write-acp, x-amz-grant-full-control headers, which map to the set of ACL permissions supported by Amazon S3.

*Note*: You can use either a predefined ACL or specify access permissions explicitly, not both.

## Requests

### Request Syntax

```
PUT / HTTP/1.1
Host: bucketname.data.basho.com
Content-Length: length
Date: date
Authorization: signature_value

  <CreateBucketConfiguration xmlns="http://data.basho.com/doc/2012-06-01/">
    <LocationConstraint>BucketRegion</LocationConstraint>
  </CreateBucketConfiguration>
```
{{% note title="Note" %}}
This example includes some request headers. The Request Headers section
contains the complete list of headers.
{{% /note %}}

### Request Parameters

This operation does not use request parameters.

### Request Headers

PUT Bucket offers the following request headers in addition to the request headers common to all operations.

**x-amz-acl** - This request header specifies a predefined ACL to apply to the bucket being created. A predefined ACL grants specific permissions to individual accounts or predefined groups.

* *Type*: String
* *Valid Values*: private | public-read | public-read-write | authenticated-read | bucket-owner-read | bucket-owner-full-control

### Response Elements

PUT Bucket does not return response elements.

## Examples

### Sample Request

A request that creates a bucket named `basho_docs`.

```
PUT / HTTP/1.1
Host: basho_docs.data.basho.com
Content-Length: 0
Date: Fri, 01 Jun  2012 12:00:00 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=
```

### Sample Response

```
HTTP/1.1 200 OK
Date: Fri, 01 Jun  2012 12:00:00 GMT
Content-Length: 0
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```

### Sample Request to Configure Access Permission Using Predefined ACL

This request creates a bucket named `basho_docs` and sets the ACL to private.

```
PUT / HTTP/1.1
Host: basho_docs.data.basho.com
Content-Length: 0
x-amz-acl: private
Date: Fri, 01 Jun  2012 12:00:00 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=
```

### Sample Response For Bucket with Predefined ACL

```
HTTP/1.1 200 OK
Date: Fri, 01 Jun  2012 12:00:00 GMT

Location: /basho_docs
Content-Length: 0
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```
