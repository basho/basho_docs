---
title: RiakCS List Containers
project: riakcs
version: 1.4.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, openstack, http]
---

Returns a list of all containers owned by an *authenticated* account.

*Note:* This operation does not list containers created by other accounts. It also does not list containers for anonymous requests.

## Requests

### Request Syntax

```
GET /<api version>/<account> HTTP/1.1
Host: data.basho.com
X-Auth-Token: auth_token
```

## Responses

A list of containers is returned in the response body, one container
per line. The HTTP response's status code will be 2xx (between 200 and
299, inclusive).

## Examples

### Sample Request

```
GET /v1.0/deadbeef HTTP/1.1
Host: data.basho.com
X-Auth-Token: aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa
```

### Sample Response

```
HTTP/1.1 200 Ok
Date: Thu, 07 Jun 2010 18:57:07 GMT
Server: RiakCS
Content-Type: text/plain; charset=UTF-8
Content-Length: 32

  images
  movies
  documents
  backups
```
