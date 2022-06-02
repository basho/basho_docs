---
title: "Riak CS DELETE Multiple Objects"
description: ""
project: "riak_cs"
project_version: "2.1.2"
toc: true
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/RiakCS-DELETE-Multi
  - /riak/cs/2.1.2/references/apis/storage/s3/RiakCS-DELETE-Multi
  - /riak/cs/latest/references/apis/storage/s3/delete-multi/
---

Multi-object `DELETE` enables you to delete multiple objects from a
bucket at the same time if those objects exist. Multi-object `DELETE`s
require you to `POST` an XML object to Riak CS specifying object key
and version information, as in the example below.

## Requests

### Request Syntax

```
POST /?delete HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Content-Length: length
Authorization: signature_value

<?xml version="1.0" encoding="UTF-8"?>
<Delete>
    <Quiet>true</Quiet>
    <Object>
         <Key>Key</Key>
    </Object>
    <Object>
         <Key>Key</Key>
    </Object>
    ...
</Delete>
```

## Example

### Sample Request

```
POST /?delete HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Content-Length: length
Authorization: signature_value

<Delete>
  <Object>
    <Key>sample1.txt</Key>
  </Object>
  <Object>
    <Key>sample2.txt</Key>
  </Object>
</Delete>
```

### Sample Response

```
HTTP/1.1 200 OK
Date: Wed, 06 Jun 2012 20:47:15 GMT
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)

<?xml version="1.0" encoding="UTF-8"?>
<DeleteResult>
  <Deleted>
    <Key>sample1.txt</Key>
  </Deleted>
  <Error>
    <Key>sample2.txt</Key>
    <Code>AccessDenied</Code>
    <Message>Access Denied</Message>
  </Error>
</DeleteResult>
```
