---
title: "Riak CS OpenStack List Containers"
description: ""
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/references/apis/storage/openstack/RiakCS-OpenStack-List-Containers
  - /riak/cs/2.1.2/references/apis/storage/openstack/RiakCS-OpenStack-List-Containers
  - /riak/cs/latest/references/apis/storage/openstack/list-containers/
---

Returns a list of all containers owned by an *authenticated* account.

**Note**: This operation does not list containers created by other accounts. It also does not list containers for anonymous requests.

## Requests

### Request Syntax

```http
GET /<api version>/<account> HTTP/1.1
Host: data.basho.com
X-Auth-Token: auth_token
```

## Responses

A list of containers is returned in the response body, one container per line. The HTTP response's status code will be `2xx` (between `200` and `299`, inclusive).

## Examples

### Sample Request

```http
GET /v1.0/deadbeef HTTP/1.1
Host: data.basho.com
X-Auth-Token: aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa
```

### Sample Response

```http
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
