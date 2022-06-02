---
title: "Riak CS OpenStack List Objects"
description: ""
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/references/apis/storage/openstack/RiakCS-OpenStack-List-Objects
  - /riak/cs/2.1.2/references/apis/storage/openstack/RiakCS-OpenStack-List-Objects
  - /riak/cs/latest/references/apis/storage/openstack/list-objects/
---

Returns a list of objects (all or up to 1,000) in a container.

## Requests

### Request Syntax

```http
GET /<api version>/<account>/<container> HTTP/1.1
Host: data.basho.com
X-Auth-Token: auth_token
```

## Responses

A list of objects is returned in the response body, one object name
per line. The response will be a `200 OK` if the request succeeded. If
the container does not exist, or if an incorrect account is specified,
then a response with a `404` (Not Found) status code will be returned.

## Examples

### Sample Request

A request that returns the objects in the container named `basho-docs`.

```http
GET /v1.0/deadbeef/basho-docs HTTP/1.1
Host: data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
X-Auth-Token: aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa
```

### Sample Response

```http
HTTP/1.1 200 Ok
Date: Thu, 07 Jun 2010 18:50:19 GMT
Server: RiakCS
Content-Type: text/plain; charset=UTF-8
Content-Length: 28

    scheduleQ1.jpg
    scheduleQ2.jpg
```
