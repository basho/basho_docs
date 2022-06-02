---
title: "Riak CS GET Object"
description: ""
project: "riak_cs"
project_version: "2.1.2"
toc: true
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/RiakCS-GET-Object
  - /riak/cs/2.1.2/references/apis/storage/s3/RiakCS-GET-Object
  - /riak/cs/latest/references/apis/storage/s3/get-object/
---

The `GET Object` operation retrieves objects from the Riak CS storage.

*Note:* You must have READ access to the object to use this operation. If the anonymous user has READ access, you can retrieve an object without using an authorization header.

GET Object retrieves an object.

## Requests

### Request Syntax

```
GET /objectName HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signature_value
```

## Examples

### Sample Request

The following request returns the object, `basho-process.jpg`.

```
GET /basho-process.jpg HTTP/1.1
Host: bucket.data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
Authorization: AWS AKIAIOSFODNN7EXAMPLE:0RQf4/cRonhpaBX5sCYVf1bNRuU=
```

### Sample Response

```
HTTP/1.1 200 OK
Date: Wed, 06 Jun 2012 20:48:15 GMT
Last-Modified: Wed, 06 Jun 2012 13:39:25 GMT
ETag: "3327731c971645a398fba9dede5f2768"
Content-Length: 611892
Content-Type: text/plain
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
[611892 bytes of object data]
```

### Sample Request Using the Range Header

This sample request asks for only the first `1000` bytes of a `1705` byte file

```
GET build.sh HTTP/1.1
Host: projects.data.basho.com
Date: Tue, 07 Jan 2014 19:49:11 +0000
Authorization: AWS QMUG3D7KP5OQZRDSQWB6:4Pb+A0YT4FhZYeqMdDhYls9f9AM=
Range: bytes=0-999
```

### Sample Response Using the Range Header

```
HTTP/1.1 206 Partial Content
Server: BashoData
Date: Tue, 07 Jan 2014 19:49:11 GMT
Content-Type: application/xml
Content-Range: bytes 0-1000/1705
Content-Length: 1000
Accept-Ranges: bytes
```
