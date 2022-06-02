---
title: "Riak CS List Multipart Uploads"
description: ""
project: "riak_cs"
project_version: "2.1.2"
toc: true
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/RiakCS-List-Multipart-Uploads/
  - /riak/cs/2.1.2/references/apis/storage/s3/RiakCS-List-Multipart-Uploads/
  - /riak/cs/latest/references/apis/storage/s3/list-multipart-uploads/
---

Lists multipart uploads that have not yet been completed or aborted.

In the response, the uploads are sorted by key. If your application has
initiated more than one multipart upload using the same object key, then uploads
in the response are first sorted by key. Additionally, uploads are sorted in
ascending order within each key by the upload initiation time.

## Requests

### Request Syntax

This example shows the syntax for listing of multipart uploads.

```
GET /?uploads HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signatureValue
```

### Request Parameters

**delimiter** - Character you use to group keys.

* *Type*: String

**max-uploads** - Sets the maximum number of multipart uploads, from 1 to 1,000, to return in the response body.

* *Type*: Integer
* *Default*: 1,000

**key-marker** - Together with `upload-id-marker`, this parameter specifies the multipart upload after which listing should begin.

* *Type*: String

**prefix** - Lists in-progress uploads only for those keys that begin with the specified prefix.

* *Type*: String

**upload-id-​marker** - Together with `key-marker`, specifies the multipart upload after which listing should begin.

* *Type*: String

### Request Headers

This implementation of the operation uses only response headers that are common to most responses. For more information, see [Common RiakCS Response Headers]({{<baseurl>}}riak/cs/2.1.2/references/apis/storage/s3/common-response-headers).

### Request Elements

This operation does not use request elements.

## Response

### Response Headers

This implementation of the operation uses only response headers that are common to most responses. For more information, see [Common RiakCS Response Headers]({{<baseurl>}}riak/cs/2.1.2/references/apis/storage/s3/common-response-headers).

### Response Elements

**ListMultipartUploadsResult** - Container for the response.

* *Type*: Container
* *Children*: `Bucket`, `KeyMarker`, `UploadIdMarker`, `NextKeyMarker`, `NextUploadIdMarker`, `MaxUploads`, `Delimiter`, `Prefix`, `CommonPrefixes`, `IsTruncated`
* *Ancestors*: None

**Bucket** - Name of the bucket to which the multipart upload was initiated.

* *Type*: String
* *Ancestors*: `ListMultipartUploadsResult`

**KeyMarker** - The key at or after which the listing began.

* *Type*: String
* *Ancestors*: `ListMultipartUploadsResult`

**UploadIdMarker** - Upload ID after which listing began.

* *Type*: String
* *Ancestors*: `ListMultipartUploadsResult`

**NextKeyMarker** - When a list is truncated, this element specifies the value that should be used for the key-marker request parameter in a subsequent request.

* *Type*: Container
* *Ancestors*: `ListMultipartUploadsResult`

**NextUploadIdMarker** - When a list is truncated, this element specifies the value that should be used for the `upload-id-marker` request parameter in a subsequent request.

* *Type*: String
* *Ancestors*: `ListMultipartUploadsResult`

**MaxUploads** - Maximum number of multipart uploads that could have been included in the response.

* *Type*: Integer
* *Ancestors*: `ListMultipartUploadsResult`

**IsTruncated** - Indicates whether the returned list of parts is truncated.

* *Type*: Boolean
* *Ancestors*: `ListPartsResult`

**Upload** - Container for elements related to a particular multipart upload.

* *Type*: Container
* *Children*:  `Key`, `UploadId`, `InitiatorOwner`, `StorageClass`, `Initiated`
* *Ancestors*: `ListMultipartUploadsResult`

**Key** - Key of the object for which the multipart upload was initiated.

* *Type*: Integer
* *Ancestors*: `Upload`

**UploadId** - Upload ID that identifies the multipart upload.

* *Type*: Integer
* *Ancestors*: `Upload`

**Initiator** - Container element that identifies who initiated the multipart upload.

* *Type*: Container
* *Children*: `ID`, `DisplayName`
* *Ancestors*: `Upload`

**ID** - Canonical User ID.

* *Type*: String
* *Ancestors*: `Initiator`, `Owner`

**DisplayName** - Principal's name.

* *Type*: String
* *Ancestors*: `Initiator`, `Owner`

**Owner** - Container element that identifies the object owner, after the object is created.

* *Type*: Container
* *Children*:  `ID`, `DisplayName`
* *Ancestors*: `Upload`

**Initiated** - Date and time at which the multipart upload was initiated.

* *Type*: Date
* *Ancestors*: `Upload`

**ListMultipartUploadsResult.Prefix** - When a prefix is provided in the request, this field contains the specified prefix.

* *Type*: String
* *Ancestors*: `ListMultipartUploadsResult`

**Delimiter** - Contains the delimiter you specified in the request.

* *Type*: String
* *Ancestors*: `ListMultipartUploadsResult`

**CommonPrefixes** - If you specify a delimiter in the request, then the result returns each distinct key prefix containing the delimiter in a CommonPrefixes element.

* *Type*: Container
* *Ancestors*: `ListMultipartUploadsResult`

**CommonPrefixes.Prefix** - If the request does not include the Prefix parameter, then this element shows only the substring of the key that precedes the first occurrence of the delimiter character. These keys are not returned anywhere else in the response.

* *Type*: String
* *Ancestors*: `CommonPrefixes`

## Examples

### Sample Request

The following request lists three multipart uploads.

```
GET /?uploads HTTP/1.1
Host: os.data.basho.com
Date: Mon, 1 Nov 2010 20:34:56 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:0RQf4/cRonhpaBX5sCYVf1bNRuU=
```

### Sample Response

```
HTTP/1.1 200 OK
Date: Mon, 1 Nov 2010 20:34:56 GMT
Content-Length: 1330
Connection: keep-alive
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)

<?xml version="1.0" encoding="UTF-8"?>
<ListMultipartUploadsResult xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
  <Bucket>os</Bucket>
  <KeyMarker></KeyMarker>
  <UploadIdMarker></UploadIdMarker>
  <NextKeyMarker>large.iso</NextKeyMarker>
  <NextUploadIdMarker></NextUploadIdMarker>
  <MaxUploads></MaxUploads>
  <IsTruncated></IsTruncated>
  <Upload>
    <Key>my-divisor</Key>
    <UploadId>VXBsb2FkIElEIGZvciA2aWWpbmcncyBteS1tb3ZpZS5tMnRzIHVwbG9hZA</UploadId>
    <Initiator>
      <ID>arn:aws:iam::111122223333:user/user1-11111a31-17b5-4fb7-9df5-b111111f13de</ID>
      <DisplayName>user1-11111a31-17b5-4fb7-9df5-b111111f13de</DisplayName>
    </Initiator>
    <Owner>
      <ID>75aa57f09aa0c8caeab4f8c24e99d10f8e7faeebf76c078efc7c6caea54ba06a</ID>
      <DisplayName>OwnerDisplayName</DisplayName>
    </Owner>
    <StorageClass>STANDARD</StorageClass>
    <Initiated>2010-11-10T20:48:33.000Z</Initiated>
  </Upload>
  ...
</ListMultipartUploadsResult>
```
