---
title: "Riak CS OpenStack Get Object"
description: ""
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/references/apis/storage/openstack/RiakCS-OpenStack-Get-Object
  - /riak/cs/2.1.2/references/apis/storage/openstack/RiakCS-OpenStack-Get-Object
  - /riak/cs/latest/references/apis/storage/openstack/get-object/
---

Retrieves an object from a Riak CS container.

## Requests

### Request Syntax

```http
GET /<api version>/<account>/<container>/<object> HTTP/1.1
Host: data.basho.com
X-Auth-Token: auth_token
```

## Examples

### Sample Request

The following request returns the object `basho-process.jpg` from the container `basho-docs`.

```http
GET /v1.0/deadbeef/basho-docs/basho-process.jpg HTTP/1.1
Host: data.basho.com
Date: Fri, 01 Jun  2012 12:00:00 GMT
X-Auth-Token: aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa
```

### Sample Response

```http
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
