---
title: "Riak CS Complete Multipart Upload"
description: ""
project: "riak_cs"
project_version: "2.0.0"
toc: true
aliases:
  - /riakcs/2.0.0/references/apis/storage/s3/RiakCS-Complete-Multipart-Upload
  - /riak/cs/2.0.0/references/apis/storage/s3/RiakCS-Complete-Multipart-Upload
---

Completes a multipart upload by assembling previously uploaded parts. Upon
receiving this request, Riak CS concatenates all the parts in ascending order by
part number to create a new object. The parts list (part number and ETag header
value)must be provided in the Complete Multipart Upload request.

Processing of a Complete Multipart Upload request could take several minutes to
complete. An HTTP response header that specifies a `200 OK` response is sent
while processing is in progress. After that, Riak CS periodically sends
whitespace characters to keep the connection from timing out. Because a request
could fail after the initial `200 OK` response has been sent, it is important
that you check the response body to determine whether the request succeeded.

## Requests

### Request Syntax

This example shows the syntax for completing a multipart upload.

```
POST /ObjectName?uploadId=UploadId HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Content-Length: size
Authorization: signatureValue

<CompleteMultipartUpload>
  <Part>
    <PartNumber>PartNumber</PartNumber>
    <ETag>ETag</ETag>
  </Part>
  ...
</CompleteMultipartUpload>
```

### Request Headers

This implementation of the operation uses only response headers that are common to most responses. For more information, see [Common Riak CS Response Headers]({{<baseurl>}}riak/cs/2.0.0/references/apis/storage/s3/common-response-headers).

### Request Elements

**CompleteMultipartUpload** - Container for the request.

* *Type*: Container
* *Children*: One or more `Part` elements
* *Ancestors*: None

**Part** - Container for elements related to a particular previously uploaded part.

* *Type*: Container
* *Children*: `PartNumber`, `ETag`
* *Ancestors*: `CompleteMultipartUpload`

**PartNumber** - Part number that identifies the part.

* *Type*: Integer
* *Ancestors*: `Part`

**ETag** - Entity tag returned when the part was uploaded.

* *Type*: String
* *Ancestors*: `Part`

## Response

### Response Headers

This implementation of the operation uses only response headers that are common to most responses. For more information, see [Common Riak CS Response Headers]({{<baseurl>}}riak/cs/2.0.0/references/apis/storage/s3/common-response-headers).

### Response Elements

**CompleteMultipartUploadResult** - Container for the response.

* *Type*: Container
* *Children*: `Location`, `Bucket`, `Key`, `ETag`
* *Ancestors*: None

**Location** - The URI that identifies the newly created object.

* *Type*: URI
* *Ancestors*: `CompleteMultipartUploadResult`

**Bucket** - The name of the bucket that contains the newly created object.

* *Type*: String
* *Ancestors*: `CompleteMultipartUploadResult`

**Key** - The object key of the newly created object.

* *Type*: String
* *Ancestors*: `CompleteMultipartUploadResult`

**ETag** - Entity tag that identifies the newly created object's data.

* *Type*: String
* *Ancestors*: `CompleteMultipartUploadResult`

### Special Errors

**EntityTooSmall** - Your proposed upload is smaller than the minimum allowed object size. Each part must be at least 5 MB in size, except the last part.

**InvalidPart** - One or more of the specified parts could not be found. The part might not have been uploaded, or the specified entity tag might not have matched the part's entity tag.

**InvalidPartOrder** - The list of parts was not in ascending order. Parts list must specified in order by part number.

**NoSuchUpload** - The specified multipart upload does not exist. The upload ID might be invalid, or the multipart upload might have been aborted or completed.

## Examples

### Sample Request

The following Complete Multipart Upload request specifies three parts in the `CompleteMultipartUpload` element.

```
POST /large.iso?uploadId=VXBsb2FkIElEIGZvciA2aWWpbmcncyBteS1tb3ZpZS5tMnRzIHVwbG9hZA HTTP/1.1
Host: os.data.basho.com
Date:  Mon, 1 Nov 2010 20:34:56 GMT
Content-Length: 391
Authorization: AWS AKIAIOSFODNN7EXAMPLE:0RQf4/cRonhpaBX5sCYVf1bNRuU=

<CompleteMultipartUpload>
  <Part>
    <PartNumber>1</PartNumber>
    <ETag>"a54357aff0632cce46d942af68356b38"</ETag>
  </Part>
  <Part>
    <PartNumber>2</PartNumber>
    <ETag>"0c78aef83f66abc1fa1e8477f296d394"</ETag>
  </Part>
  <Part>
    <PartNumber>3</PartNumber>
    <ETag>"acbd18db4cc2f85cedef654fccc4a4d8"</ETag>
  </Part>
</CompleteMultipartUpload>
```

### Sample Response

```
HTTP/1.1 200 OK
Date: Mon, 1 Nov 2010 20:34:56 GMT
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)

<?xml version="1.0" encoding="UTF-8"?>
<CompleteMultipartUploadResult xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
  <Location>http://os.data.basho.com/large.iso</Location>
  <Bucket>os</Bucket>
  <Key>large.iso</Key>
  <ETag>"3858f62230ac3c915f300c664312c11f-9"</ETag>
</CompleteMultipartUploadResult>
```
