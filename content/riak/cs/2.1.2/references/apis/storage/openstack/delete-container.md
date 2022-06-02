---
title: "Riak CS OpenStack Delete Container"
description: ""
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/references/apis/storage/openstack/RiakCS-OpenStack-Delete-Container
  - /riak/cs/2.1.2/references/apis/storage/openstack/RiakCS-OpenStack-Delete-Container
  - /riak/cs/latest/references/apis/storage/openstack/delete-container/
---

Deletes a container.

{{% note title="Note" %}}
All objects in the container must be deleted before you can delete the
container.
{{% /note %}}

## Requests

### Request Syntax

```http
DELETE /<api version>/<account>/<container> HTTP/1.1
Host: data.basho.com
X-Auth-Token: auth_token
```

## Responses

This operation does not return a response.

## Examples

### Sample Request

A request that deletes a container named `basho-docs`.

```http
DELETE /v1.0/deadbeef/basho-docs HTTP/1.1
Host: data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
X-Auth-Token: aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa
```

### Sample Response

```http
HTTP/1.1 204 No Content
Date: Wed, 06 Jun 2012 20:47:15 +0000
Connection: close
Server: RiakCS
Content-Length: 0
Content-Type: text/plain; charset=UTF-8
```
