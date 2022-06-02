---
title: "Riak CS GET Bucket ACL"
description: ""
project: "riak_cs"
project_version: "2.1.2"
toc: true
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/RiakCS-GET-Bucket-ACL
  - /riak/cs/2.1.2/references/apis/storage/s3/RiakCS-GET-Bucket-ACL
  - /riak/cs/latest/references/apis/storage/s3/get-bucket-acl/
---

The `GET Bucket acl` operation uses the `acl` subresource to return the access control list (ACL) of a bucket.

*Note:* You must have READ_ACP access to the bucket to use this operation. If the anonymous user has READ_ACP permission, this operation will return the ACL of the bucket without an authorization header.

## Requests

### Request Syntax

```
GET /?acl HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signature_value
```

### Request Parameters

The GET Bucket acl operation doesn't use request parameters.

## Response Elements

**AccessControlList** - Container for ACL information.

* *Type*: Container
* *Ancestry*: AccessControlPolicy

**AccessControlPolicy** - Container for the response.

* *Type*: Container
* *Ancestry*: None

**DisplayName** - Bucket owner's display name.

*Note*: The operation returns the `DisplayName` only if the owner's e-mail address can be determined from the `ID`.

* *Type*: String
* *Ancestry*: AccessControlPolicy.Owner

**Grant** - Container for `Grantee` and `Permission`.

* *Type*: Container
* *Ancestry*: AccessControlPolicy.AccessControlList

**Grantee** - Container for `DisplayName` and `ID` of the person who is being granted permissions.

* *Type*: Container
* *Ancestry*: AccessControlPolicy.AccessControlList.Grant

**ID** - Bucket owner's ID.

* *Type*: String
* *Ancestry*: AccessControlPolicy.Owner

**Owner** - Container for bucket owner information.

* *Type*: Container
* *Ancestry*: AccessControlPolicy

**Permission** - Permission granted to the `Grantee` for bucket.

* *Type*: String
* *TypeValid Values*: FULL_CONTROL|WRITE|WRITE_ACP|READ|READ_ACP

*Ancestry*: AccessControlPolicy.AccessControlList.Grant**AccessControlList** - Container for ACL information.

* *Type*: Container
* *Ancestry*: AccessControlPolicy

**AccessControlPolicy** - Container for the response.

* *Type*: Container
* *Ancestry*: None

**DisplayName** - Bucket owner's display name.

*Note*: The operation returns the `DisplayName` only if the owner's e-mail address can be determined from the `ID`.

* *Type*: String
* *Ancestry*: AccessControlPolicy.Owner

**Grant** - Container for `Grantee` and `Permission`.

* *Type*: Container
* *Ancestry*: AccessControlPolicy.AccessControlList

**Grantee** - Container for `DisplayName` and `ID` of the person who is being granted permissions.


* *Type*: Container
* *Ancestry*: AccessControlPolicy.AccessControlList.Grant

**ID** - Bucket owner's ID.


* *Type*: String
* *Ancestry*: AccessControlPolicy.Owner

**Owner** - Container for bucket owner information.


* *Type*: Container
* *Ancestry*: AccessControlPolicy

**Permission** - Permission granted to the `Grantee` for bucket.


* *Type*: String
* *Valid Values*: FULL_CONTROL|WRITE|WRITE_ACP|READ|READ_ACP
* *Ancestry*: AccessControlPolicy.AccessControlList.Grant

## Examples

### Sample Request

This request returns the ACL of the specified bucket.

```
GET ?acl HTTP/1.1
Host:bucket.data.basho.com
Date: Wed, 06 Jun 2012 20:47:15 +0000
Authorization: AWS QMUG3D7KP5OQZRDSQWB6:4Pb+A0YT4FhZYeqMdDhYls9f9AM=
```

### Sample Response

```
HTTP/1.1 200 OK
Date: Wed, 06 Jun 2012 20:47:15 +0000
Last-Modified: Mon, 04 Jun 2012 12:00:00 GMT
Content-Length: 124198
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
