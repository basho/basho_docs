---
title: "Riak CS OpenStack Delete Object"
description: ""
project: "riak_cs"
project_version: "2.0.0"
lastmod: 2015-03-28T00:00:00-00:00
sitemap:
  priority: 0.1
aliases:
  - /riakcs/2.0.0/references/apis/storage/openstack/RiakCS-OpenStack-Delete-Object
  - /riak/cs/2.0.0/references/apis/storage/openstack/RiakCS-OpenStack-Delete-Object
---

Removes the specified object, if it exists.

## Requests

### Request Syntax

```http
DELETE /<api version>/<account>/<container>/<object> HTTP/1.1
Host: data.basho.com
X-Auth-Token: auth_token
```

## Responses

This operation does not return a response.

## Examples

### Sample Request

The following request deletes the object `basho-process.jpg` from the container `basho-docs`.

```http
DELETE /v1.0/deadbeef/basho-docs/basho-process.jpg HTTP/1.1
Host: data.basho.com
Date: Fri, 01 Jun  2012 12:00:00 GMT
X-Auth-Token: aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa
```

### Sample Response

```http
HTTP/1.1 204 No Content
Date: Wed, 06 Jun 2012 20:47:15 GMT
Connection: close
Server: RiakCS
```
