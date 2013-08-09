---
title: RiakCS List Objects
project: riakcs
version: 1.4.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, openstack, http]
---

Returns a list of objects (all or up to 1,000) in a container.

## Requests

### Request Syntax

```
GET /<api version>/<account>/<container> HTTP/1.1
Host: data.basho.com
X-Auth-Token: auth_token
```

## Responses

A list of objects is returned in the response body, one object name
per line. The response will be a `200 Ok` if the request succeeded. If
the container does not exist, or if an incorrect account is specified,
then a response with a 404 (Not Found) status code will be returned.

## Examples

### Sample Request

A request that returns the objects in the container named `basho-docs`.

```
GET /v1.0/deadbeef/basho-docs HTTP/1.1
Host: data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
X-Auth-Token: aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa
```

### Sample Response

```
HTTP/1.1 200 Ok
Date: Thu, 07 Jun 2010 18:50:19 GMT
Server: RiakCS
Content-Type: text/plain; charset=UTF-8
Content-Length: 28

    scheduleQ1.jpg
    scheduleQ2.jpg
```
