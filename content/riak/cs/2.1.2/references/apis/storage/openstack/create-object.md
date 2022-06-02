---
title: "Riak CS OpenStack Create Object"
description: ""
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/references/apis/storage/openstack/RiakCS-OpenStack-Create-Object
  - /riak/cs/2.1.2/references/apis/storage/openstack/RiakCS-OpenStack-Create-Object
  - /riak/cs/latest/references/apis/storage/openstack/create-object/
---

Writes or overwrites an object's content and metadata.

Riak CS is a distributed system. If it receives multiple write requests for the same object at the same time, the system will overwrite all but the last object written. If necessary, you can build versioning or object locking into your application.

To prevent the storage of data corrupted during transmission over a network, the Content-MD5 header instructs Riak CS to compare the object to the MD5 value provided. If the values don't match, the operation returns an error. In addition, if the PUT Object operation calculates the MD5, you can compare the ETag that is returned to the calculated MD5 value.

**Note**: You can configure an application to use the `100-continue` HTTP status code, which sends the Request Headers prior to sending the request body. Doing so prevents sending the message body when the message is rejected based on the headers, for example, due to authentication failure or redirect.

## Requests

### Request Syntax

```http
PUT /<api version>/<account>/<container>/<object> HTTP/1.1
Host: data.basho.com
X-Auth-Token: auth_token
```

## Responses

This operation does not return a response.

## Examples

### Sample Request

A request that stores the object `basho-process.jpg` in the container `basho-docs`.

```http
PUT /v1.0/deadbeef/basho-docs/basho-process.jpg HTTP/1.1
Host: data.basho.com
Date: Fri, 01 Jun  2012 12:00:00 GMT
X-Auth-Token: aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa
Content-Type: text/plain
Content-Length: 201445
Expect: 100-continue
[201445 bytes of object data]
```

### Sample Response

```http
HTTP/1.1 201 Created
Date: Fri, 01 Jun  2012 12:00:00 GMT
ETag: "32cf731c97645a398434535f271b2358"
Content-Length: 0
Connection: close
Server: RiakCS
```
