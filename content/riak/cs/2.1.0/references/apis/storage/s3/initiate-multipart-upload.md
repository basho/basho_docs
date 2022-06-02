---
title: "Riak CS Initiate Multipart Upload"
description: ""
project: "riak_cs"
project_version: "2.1.0"
toc: true
aliases:
  - /riakcs/2.1.0/references/apis/storage/s3/RiakCS-Initiate-Multipart-Upload/
  - /riak/cs/2.1.0/references/apis/storage/s3/RiakCS-Initiate-Multipart-Upload/
---

Initiates a multipart upload and returns an upload ID. The upload ID is used to associate all the parts in the specific multipart upload.

## Requests

### Request Syntax

This example shows the syntax for initiating a multipart upload.

```
POST /ObjectName?uploads HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signatureValue
```

### Request Headers

**Content-Type** - A standard MIME type that describes the content format.

* *Type*: String
* *Default*: binary/octet-stream
* *Valid Values*: 100-continue
* *Constraints*: None

**x-amz-meta-*** - User specified metadata fields which can be stored with the object.

* *Type*: String
* *Default*: None
* *Constraints*: None

#### Permission Request Headers

**x-amz-acl** - This request header specifies a predefined ACL to apply to the object being created. A predefined ACL grants specific permissions to individual accounts or predefined groups.

* *Type*: String
* *Valid Values*: private | public-read | public-read-write | authenticated-read | bucket-owner-read | bucket-owner-full-control
* *Constraints*: None

### Request Elements

This operation does not use request elements.

## Response

### Response Headers

This implementation of the operation uses only response headers that are common to most responses. For more information, see [Common RiakCS Response Headers]({{<baseurl>}}riak/cs/2.1.0/references/apis/storage/s3/common-response-headers).

### Response Elements

**InitiateMultipartUploadResult** - Container for response.

* *Type*: Container
* *Children*: `Bucket`, `Key`, `UploadId`
* *Ancestors*: None

**Bucket** - Name of the bucket to which the multipart upload was initiated.

* *Type*: String
* *Children*: `Bucket`, `Key`, `UploadId`
* *Ancestors*: `InitiateMultipartUploadResult`

**Key** - Object key for which the multipart upload was initiated.

* *Type*: String
* *Ancestors*: `InitiateMultipartUploadResult`

**UploadId** - ID for the initiated multipart upload.

* *Type*: String
* *Ancestors*: `InitiateMultipartUploadResult`

## Examples

### Sample Request

This operation initiates a multipart upload for the `large.iso` object.

```
POST /large.iso?uploads HTTP/1.1
Host: os.data.basho.com
Date: Mon, 1 Nov 2010 20:34:56 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:0RQf4/cRonhpaBX5sCYVf1bNRuU=
```

### Sample Response

```
HTTP/1.1 200 OK
Date:  Mon, 1 Nov 2010 20:34:56 GMT
Content-Length: 197
Connection: keep-alive
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)

<?xml version="1.0" encoding="UTF-8"?>
<InitiateMultipartUploadResult xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
  <Bucket>os</Bucket>
  <Key>large.iso</Key>
  <UploadId>VXBsb2FkIElEIGZvciA2aWWpbmcncyBteS1tb3ZpZS5tMnRzIHVwbG9hZA</UploadId>
</InitiateMultipartUploadResult>
```
