---
title: RiakCS Get Object
project: riakcs
version: 1.4.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, openstack, http]
---

Retrieves an object from a Riak CS container.

## Requests

### Request Syntax

```
GET /<api version>/<account>/<container>/<object> HTTP/1.1
Host: data.basho.com
X-Auth-Token: auth_token
```

## Examples

### Sample Request

The following request returns the object `basho-process.jpg` from the container `basho-docs`.

```
GET /v1.0/deadbeef/basho-docs/basho-process.jpg HTTP/1.1
Host: data.basho.com
Date: Fri, 01 Jun  2012 12:00:00 GMT
X-Auth-Token: aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa
```

### Sample Response

```
HTTP/1.1 200 OK
Date: Wed, 06 Jun 2012 20:48:15 GMT
Last-Modified: Wed, 06 Jun 2012 13:39:25 GMT
ETag: "32cf731c97645a398434535f271b2358"
Content-Length: 201445
Content-Type: text/plain
Connection: close
Server: RiakCS
[201445 bytes of object data]
```
