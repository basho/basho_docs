---
title: RiakCS GET Object
project: riakcs
version: 1.2.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, http]
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

Note: While Range queries are planned for a future release, using `Range:bytes=byte_range`, this feature is not currently implemented.

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
