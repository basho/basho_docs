---
title: "Common Riak CS Response Headers"
description: ""
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/Common-RiakCS-Response-Headers
  - /riak/cs/2.1.2/references/apis/storage/s3/Common-RiakCS-Response-Headers
  - /riak/cs/latest/references/apis/storage/s3/common-response-headers/
---

These are the headers that are common to all Riak CS REST responses.

Header | Description | Data type
:------|:------------|:---------
`Content-Length` | The length in bytes of the response body. | string |
`Connection` | Whether the connection to the server is open or closed. | enum (`open` or `close`) |
`Date` | The date and time that Riak CS responded, e.g. `Fri, 01 Jun 2012 12:00:00 GMT` | string |
`Etag` | The entity tag is an MD5 hash of the object and reflects only changes to the object contents, not the object's metadata. The ETag is set when an object is created. | string |
`Server` | The name of the server that created the response. | string |
