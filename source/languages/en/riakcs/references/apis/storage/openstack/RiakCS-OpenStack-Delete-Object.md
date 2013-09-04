---
title: RiakCS OpenStack Delete Object
project: riakcs
version: 1.4.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, openstack, http]
---

Removes the specified object, if it exists.

## Requests

### Request Syntax

```
DELETE /<api version>/<account>/<container>/<object> HTTP/1.1
Host: data.basho.com
X-Auth-Token: auth_token
```

## Responses

This operation does not return a response.

## Examples

### Sample Request

The following request deletes the object `basho-process.jpg` from the container `basho-docs`.

```
DELETE /v1.0/deadbeef/basho-docs/basho-process.jpg HTTP/1.1
Host: data.basho.com
Date: Fri, 01 Jun  2012 12:00:00 GMT
X-Auth-Token: aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa
```

### Sample Response

```
HTTP/1.1 204 No Content
Date: Wed, 06 Jun 2012 20:47:15 GMT
Connection: close
Server: RiakCS
```
