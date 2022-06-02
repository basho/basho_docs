---
title: "Riak CS DELETE Object"
description: ""
project: "riak_cs"
project_version: "2.1.2"
toc: true
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/RiakCS-DELETE-Object
  - /riak/cs/2.1.2/references/apis/storage/s3/RiakCS-DELETE-Object
  - /riak/cs/latest/references/apis/storage/s3/delete-object/
---

The `DELETE Object` operation removes an object, if one exists.

## Requests

### Request Syntax

```
DELETE /ObjectName HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Content-Length: length
Authorization: signature_value
```

## Examples

### Sample Request

The DELETE Object operation deletes the object, `projects-schedule.jpg`.

```
DELETE /projects-schedule.jpg HTTP/1.1
Host: bucketname.data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 GMT
Authorization: AWS QMUG3D7KP5OQZRDSQWB6:4Pb+A0YT4FhZYeqMdDhYls9f9AM=
```

### Sample Response

```
HTTP/1.1 204 No Content
Date: Wed, 06 Jun 2012 20:47:15 GMT
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```
