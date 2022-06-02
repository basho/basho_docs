---
title: "Riak CS GET Service"
description: ""
project: "riak_cs"
project_version: "2.1.2"
toc: true
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/RiakCS-GET-Service
  - /riak/cs/2.1.2/references/apis/storage/s3/RiakCS-GET-Service
  - /riak/cs/latest/references/apis/storage/s3/get-service/
---

The `GET Service` operation returns a list of all buckets owned by the *authenticated* user who sent sent the request.

*Note:* The GET Service operation doesn't list buckets created by other users. It also doesn't list buckets for anonymous requests.

## Requests

### Request Syntax

```
GET / HTTP/1.1
Host: data.basho.com
Date: date
Authorization: signature_value
```

## Response Elements

**Bucket** - Container for bucket information.

* *Type*: Container
* *Children*: Name,CreationDate
* *Ancestor*: ListAllMyBucketsResult.Buckets

**Buckets** - Container for one or more buckets.

* *Type*: Container
* *Children*: Bucket
* *Ancestor*: ListAllMyBucketsResult

**CreationDate** - Date the bucket was created.

* *Type*: date (format yyyy-mm-ddThh:mm:ss.timezone, e.g., 2012-06-03T15:4548:02.000Z)
* *Ancestor*: ListAllMyBucketsResult.Buckets.Bucket

**DisplayName** - Bucket owner's display name.

* *Type*: String
* *Ancestor*: ListAllMyBucketsResult.Owner

**ID** - Bucket owner's user ID.

* *Type*: String
* *Ancestor*: ListAllMyBucketsResult.Owner

**ListAllMyBucketsResult** - Container for response.

* *Type*: Container
* *Children*: Owner, Buckets
* *Ancestor*: None

**Name** - Bucket's name.

* *Type*: String
* *Ancestor*: ListAllMyBucketsResult.Buckets.Bucket

**Owner** - Container for bucket owner information.

* *Type*: Container
* *Ancestor*: ListAllMyBucketsResult

## Examples

### Sample Request

The GET operation on the Service endpoint (data.basho.com in this example) returns a list of all of the buckets owned by the authenticated sender of the request.

```
Host: data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
Authorization: AWS QMUG3D7KP5OQZRDSQWB6:4Pb+A0YT4FhZYeqMdDhYls9f9AM=
```

### Sample Response

```xml
  <?xml version="1.0" encoding="UTF-8"?>
  <ListAllMyBucketsResult xmlns="http://data.basho.com/2012-06-12">
    <Owner>
      <ID>324ABC0713CD0B420EFC086821BFAE7ED81442C</ID>
      <DisplayName>"foobar</DisplayName>
    </Owner>
    <Buckets>
      <Bucket>
        <Name>projects</Name>
        <CreationDate>2011-05-10T14:10:15.000Z</CreationDate>
      </Bucket>
      <Bucket>
        <Name>templates</Name>
        <CreationDate>2011-05-10T14:18:25.000Z</CreationDate>
      </Bucket>
    </Buckets>
  </ListAllMyBucketsResult>
```
