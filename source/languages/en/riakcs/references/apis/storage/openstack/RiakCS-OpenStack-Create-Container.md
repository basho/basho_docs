---
title: RiakCS OpenStack Create Container
project: riakcs
version: 1.4.0+
document: api
toc: true
index: false
audience: advanced
keywords: [api, openstack, http]
---

Creates a new container. The account of the user who makes the request
to create the container becomes the container owner. Anonymous
requests cannot create containers.

## Container Naming

A container name must obey the following rules, which produces a
DNS-compliant container name:

* Must be from 3 to 63 characters.
* Must be one or more labels, each separated by a period (.). Each label:
* Must start with a lowercase letter or a number. Must end with a
  lowercase letter or a number. Can contain lowercase letters, numbers
  and dashes.
* Must not be formatted as an IP address (e.g., 192.168.9.2).

## Requests

### Request Syntax

```
PUT /<api version>/<account>/<container> HTTP/1.1
Host: data.basho.com
X-Auth-Token: auth_token
```

### Request Parameters

This operation does not use request parameters.

## Responses

This operation does not return a response.

## Examples

### Sample Request

A request that creates a container named `basho-docs`.

```
PUT /v1.0/deadbeef/basho-docs HTTP/1.1
Host: data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
X-Auth-Token: aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa
```

### Sample Response

```
HTTP/1.1 201 Created
Date: Thu, 07 Jun 2007 18:50:19 GMT
Server: RiakCS
Content-Type: text/plain; charset=UTF-8
```
