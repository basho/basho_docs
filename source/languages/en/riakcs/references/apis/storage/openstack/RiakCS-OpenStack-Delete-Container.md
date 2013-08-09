---
title: RiakCS Delete Container
project: riakcs
version: 1.4.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, openstack, http]
---

Deletes a container.

<div class="note"><div clas="title">Note</div>All objects in the container must be deleted before you can delete the container.</div>

## Requests

### Request Syntax

```
DELETE /<api version>/<account>/<container> HTTP/1.1
Host: data.basho.com
X-Auth-Token: auth_token
```

## Responses

This operation does not return a response.

## Examples

### Sample Request

A request that deletes a container named `basho-docs`.

```
DELETE /v1.0/deadbeef/basho-docs HTTP/1.1
Host: data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
X-Auth-Token: aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa
```

### Sample Response

```
HTTP/1.1 204 No Content
Date: Wed, 06 Jun 2012 20:47:15 +0000
Connection: close
Server: RiakCS
Content-Length: 0
Content-Type: text/plain; charset=UTF-8
```
