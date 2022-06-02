---
title: "Riak CS List Parts"
description: ""
project: "riak_cs"
project_version: "2.1.0"
toc: true
aliases:
  - /riakcs/2.1.0/references/apis/storage/s3/RiakCS-List-Parts/
  - /riak/cs/2.1.0/references/apis/storage/s3/RiakCS-List-Parts/
---

Lists the parts that have been uploaded for a specific multipart upload.

## Requests

### Request Syntax

This example shows the syntax for listing parts of a multipart upload.

```
GET /ObjectName?uploadId=UploadId HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signatureValue
```

### Request Parameters

**uploadId** - Upload ID identifying the multipart upload whose parts are being listed.

* *Type*: String
* *Default*: None

**max-parts** - Sets the maximum number of parts to return in the response body.

* *Type*: String
* *Default*: 1,000

**part-number​-marker** - Specifies the part after which listing should begin. Only parts with higher part numbers will be listed.

* *Type*: String
* *Default*: None

### Request Headers

This implementation of the operation uses only response headers that are common to most responses. For more information, see [Common Riak CS Response Headers]({{<baseurl>}}riak/cs/2.1.0/references/apis/storage/s3/common-response-headers).

### Request Elements

This operation does not use request elements.

## Response

### Response Headers

This implementation of the operation uses only response headers that are common to most responses. For more information, see [Common Riak CS Response Headers]({{<baseurl>}}riak/cs/2.1.0/references/apis/storage/s3/common-response-headers).

### Response Elements

**ListPartsResult** - Container for the response.

* *Type*: Container
* *Children*: `Bucket`, `Key`, `UploadId`, `Initiator`, `Owner`, `StorageClass`, `PartNumberMarker`, `NextPartNumberMarker`, `MaxParts`, `IsTruncated`, `Part`

**Bucket** - Name of the bucket to which the multipart upload was initiated.

* *Type*: String
* *Ancestors*: `ListPartsResult`

**Key** - Object key for which the multipart upload was initiated.

* *Type*: String
* *Ancestors*: `ListPartsResult`

**UploadId** - Upload ID identifying the multipart upload whose parts are being listed.

* *Type*: String
* *Ancestors*: `ListPartsResult`

**Initiator** - Container element that identifies who initiated the multipart upload.

* *Type*: Container
* *Children*: `ID`, `DisplayName`
* *Ancestors*: `ListPartsResult`

**ID** - Canonical User ID.

* *Type*: String
* *Ancestors*: `Initiator`

**DisplayName** - Principal's name.

* *Type*: String
* *Ancestors*: `Initiator`

**Owner** - Container element that identifies the object owner, after the object is created.

* *Type*: Container
* *Children*: `ID`, `DisplayName`
* *Ancestors*: `ListPartsResult`

**PartNumberMarker** - Part number after which listing begins.

* *Type*: Integer
* *Ancestors*: `ListPartsResult`

**NextPartNumberMarker** - When a list is truncated, this element specifies the last part in the list, as well as the value to use for the `part-number-marker` request parameter in a subsequent request.

* *Type*: Integer
* *Ancestors*: `ListPartsResult`

**MaxParts** - Maximum number of parts that were allowed in the response.

* *Type*: Integer
* *Ancestors*: `ListPartsResult`

**IsTruncated** - Indicates whether the returned list of parts is truncated.

* *Type*: Boolean
* *Ancestors*: `ListPartsResult`

**Part** - Container for elements related to a particular part.

* *Type*: String
* *Children*: `PartNumber`, `LastModified`, `ETag`, `Size`
* *Ancestors*: `ListPartsResult`

**PartNumber** - Part number identifying the part.

* *Type*: Integer
* *Ancestors*: `Part`

**LastModified** - Date and time at which the part was uploaded.

* *Type*: Date
* *Ancestors*: `Part`

**ETag** - Entity tag returned when the part was uploaded.

* *Type*: String
* *Ancestors*: `Part`

**Size** - Size of the uploaded part data.

* *Type*: Integer
* *Ancestors*: `Part`

## Examples

### Sample Request

The following request lists multipart upload parts.

```
GET /large.iso?uploadId=VXBsb2FkIElEIGZvciA2aWWpbmcncyBteS1tb3ZpZS5tMnRzIHVwbG9hZA HTTP/1.1
Host: os.data.basho.com
Date: Mon, 1 Nov 2010 20:34:56 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:0RQf4/cRonhpaBX5sCYVf1bNRuU=
```

### Sample Response

```
HTTP/1.1 200 OK
Date: Mon, 1 Nov 2010 20:34:56 GMT
Content-Length: 985
Connection: keep-alive
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)

<?xml version="1.0" encoding="UTF-8"?>
<ListPartsResult xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
  <Bucket>os</Bucket>
  <Key>large.iso</Key>
  <UploadId>VXBsb2FkIElEIGZvciA2aWWpbmcncyBteS1tb3ZpZS5tMnRzIHVwbG9hZA</UploadId>
  <Initiator>
      <ID>arn:aws:iam::111122223333:user/some-user-11116a31-17b5-4fb7-9df5-b288870f11xx</ID>
      <DisplayName>umat-user-11116a31-17b5-4fb7-9df5-b288870f11xx</DisplayName>
  </Initiator>
  <Owner>
    <ID>75aa57f09aa0c8caeab4f8c24e99d10f8e7faeebf76c078efc7c6caea54ba06a</ID>
    <DisplayName>someName</DisplayName>
  </Owner>
  <StorageClass>STANDARD</StorageClass>
  <Part>
    <PartNumber>1</PartNumber>
    <LastModified>2010-11-10T20:48:34.000Z</LastModified>
    <ETag>"7778aef83f66abc1fa1e8477f296d394"</ETag>
    <Size>10485760</Size>
  </Part>
  ...
</ListPartsResult>
```
