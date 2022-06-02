---
title: "Riak CS GET Object ACL"
description: ""
project: "riak_cs"
project_version: "2.1.2"
toc: true
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/RiakCS-GET-Object-ACL
  - /riak/cs/2.1.2/references/apis/storage/s3/RiakCS-GET-Object-ACL
  - /riak/cs/latest/references/apis/storage/s3/get-object-acl/
---

The `GET Object acl` operation uses the `acl` subresource to return the access control list (ACL) of an object.

*Note:* You must have READ_ACP access to the object to use this operation.

## Requests

### Request Syntax

```
GET /ObjectName?acl HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signature_value
```

### Request Parameters

The GET Object acl operation doesn't use request parameters.

## Response Elements

**AccessControlList** - Container for ACL information (Grant, Grantee, and Permission).

* *Type*: Container
* *Ancestors*: AccessControlPolicy

**AccessControlPolicy** - Contains the elements that set the ACL permissions for each grantee.

* *Type*: String
* *Ancestors*: None

**DisplayName** - Bucket owner's display name.

* *Type*: String
* *Ancestors*: AccessControlPolicy.Owner

**Grant** - Container for `Grantee` and `Permission`.

* *Type*: Container
* *Ancestors*: AccessControlPolicy.AccessControlList

**Grantee** - The `ID`, `Emailaddress`, or `uri` of the subject who is being granted permissions.

* *Type*: String
* *Ancestors*: AccessControlPolicy.AccessControlList.Grant

**ID** - Bucket owner's ID.

* *Type*: String
* *Ancestors*: AccessControlPolicy.Owner|AccessControlPolicy.AccessControlList.Grant

**Owner** - Container for bucket owner information.

* *Type*: Container
* *Ancestors*: AccessControlPolicy

**Permission** - Permission granted to the `Grantee` for bucket.

* *Type*: String
* *Valid Values*: FULL_CONTROL|WRITE|READ_ACP
* *Ancestors*: AccessControlPolicy.AccessControlList.Grant

## Examples

### Sample Request

This request returns the ACL of the object, `basho-process.jpg`.

```
GET /basho-process.jpg?acl HTTP/1.1
Host:bucket.data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
Authorization: AWS QMUG3D7KP5OQZRDSQWB6:4Pb+A0YT4FhZYeqMdDhYls9f9AM=
```

### Sample Response

```
HTTP/1.1 200 OK
Date: Wed, 06 Jun 2012 20:47:15 GMT
Last-Modified: Mon, 04 Jun 2012 12:00:00 GMT
Content-Length: 124
Content-Type: text/plain
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)

  <AccessControlPolicy>
    <Owner>
      <ID>24ef09aa099d10f75aa57c8caeab4f8c8e7faeebf76c078efc7c6caea54ba06a</ID>
      <DisplayName>UserName@basho.com</DisplayName>
    </Owner>
    <AccessControlList>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                          xsi:type="CanonicalUser">
          <ID>24ef09aa099d10f75aa57c8caeab4f8c8e7faeebf76c078efc7c6caea54ba06a</ID>
          <DisplayName>UserName@basho.com</DisplayName>
        </Grantee>
        <Permission>FULL_CONTROL</Permission>
      </Grant>
    </AccessControlList>
  </AccessControlPolicy>
```
