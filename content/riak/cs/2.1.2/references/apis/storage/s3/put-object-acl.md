---
title: "Riak CS PUT Object ACL"
description: ""
project: "riak_cs"
project_version: "2.1.2"
toc: true
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/RiakCS-PUT-Object-ACL/
  - /riak/cs/2.1.2/references/apis/storage/s3/RiakCS-PUT-Object-ACL/
  - /riak/cs/latest/references/apis/storage/s3/put-object-acl/
---

The `PUT Object acl` operation uses the `acl` subresource to set the access control list (ACL) permissions for an existing object in a bucket.

*Note:* You must have WRITE_ACP access to the object to use this operation.

`PUT Object acl` offers two methods for setting an object's permissions:

* Specify the ACL in the request body
* Specify permissions using request headers

*Note*: You can specify an ACL in the request body or with request headers, not both.

## Requests

### Request Syntax

This example shows the syntax for setting the ACL in the request body. The Request Headers section contain a list of headers you can use instead.

```
PUT /ObjectName?acl HTTP/1.1
Host: bucketname.data.basho.com
Date: date
Authorization: signatureValue

  <AccessControlPolicy>
    <Owner>
      <ID>ID</ID>
      <DisplayName>EmailAddress</DisplayName>
    </Owner>
    <AccessControlList>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser">
          <ID>ID</ID>
          <DisplayName>EmailAddress</DisplayName>
        </Grantee>
        <Permission>Permission</Permission>
      </Grant>
      ...
    </AccessControlList>
  </AccessControlPolicy>
```

### Request Parameters

This operation does not use request parameters.

### Request Headers

`PUT Object acl` offers the following request headers in addition to the request headers common to all operations.

**x-amz-acl** - This request header specifies a predefined ACL to apply to the object being created. A predefined ACL grants specific permissions to individual accounts or predefined groups.

* *Type*: String
* *Valid Values*: private | public-read | public-read-write | authenticated-read | bucket-owner-read | bucket-owner-full-control
* *Default*: private

### Request Elements

If you specify the ACL using the request body, you must use the following elements:

**AccessControlList** - Container for ACL information (Grant, Grantee, and Permission).

* *Type*: Container
* *Ancestors*: AccessControlPolicy

**AccessControlPolicy** - Contains the elements that set the ACL permissions for each grantee.

* *Type*: Container
* *Ancestors*: None

**DisplayName** - Object owner's display name.

* *Type*: String
* *Ancestors*: AccessControlPolicy.Owner

**Grant** - Container for `Grantee` and `Permission`.

* *Type*: Container
* *Ancestors*: AccessControlPolicy.AccessControlList

**Grantee** - The subject who is being granted permissions.

* *Type*: String
* *Valid Values*: DisplayName|EmailAddress|AuthenticatedUser
* *Ancestors*: AccessControlPolicy.AccessControlList.Grant

**ID** - Object owner's ID.

* *Type*: String
* *Ancestors*: AccessControlPolicy.Owner|AccessControlPolicy.AccessControlList.Grant

**Owner** - Container for object owner information.

* *Type*: Container
* *Ancestors*: AccessControlPolicy

**Permission** - Permission granted to the `Grantee`.

* *Type*: String
* *Valid Values*: FULL_CONTROL|WRITE_ACP|READ|READ_ACP
* *Ancestors*: AccessControlPolicy.AccessControlList.Grant

In request elements, you can specify the grantee to whom you are granting permissions in the following ways:

* *emailAddress*: The email address of an account

```
  <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CustomerByEmail">
    <EmailAddress>user1@basho.com</EmailAddress>
  </Grantee>
```

From the email address, the grantee is resolved to the CanonicalUser. The response to a `GET Object acl` request displays the grantee as the CanonicalUser.

* *id*: The user ID of an account

```
  <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser">
    <ID>ID</ID>
    <DisplayName>GranteesEmail</DisplayName>
  </Grantee>
```

For the id method, DisplayName is optional and ignored in the request.

* *uri*: The uri that defines a group

```
  <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Group">
    <URI>http://data.basho.com/groups/AuthenticatedUsers<URI>
  </Grantee>
```

### Response Elements

PUT Bucket acl does not return response elements.

## Examples

### Sample Request with Access Permission Specified in Request Body

This sample request grants access permission to an existing object, named `basho-process.jpg`, by specifying the ACL in the request body. In addition to granting full control to the bucket owner, grant full control to an account identified by its canonical user ID.

```
PUT /basho-process.jpg?acl HTTP/1.1
Host: basho_docs.data.basho.com
Date: Fri, 01 Jun  2012 12:00:00 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=
Content-Length: 124

  <AccessControlPolicy>
    <Owner>
      <ID>75aa57f09aa0c8caeab4f8c24e99d10f8e7faeebf76c078efc7c6caea54ba06a</ID>
      <DisplayName>user1@basho.com</DisplayName>
    </Owner>
    <AccessControlList>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser">
          <ID>75aa57f09aa0c8caeab4f8c24e99d10f8e7faeeExampleCanonicalUserID</ID>
          <DisplayName>user2@basho.com</DisplayName>
        </Grantee>
        <Permission>FULL_CONTROL</Permission>
      </Grant>
    </AccessControlList>
  </AccessControlPolicy>
```

### Sample Response

This is the sample response when versioning is enabled.

```
HTTP/1.1 200 OK
Date: Fri, 01 Jun  2012 12:00:00 GMT
Last-Modified:Fri, 01 Jun  2012 10:30:15 GMT
Content-Length: 0
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```

### Sample Request Setting Access Permissions with Headers

The following request uses ACL-specific request headers, x-amz-acl, and specifies a predefined ACL (public_read) to grant object read access to everyone.

```
PUT basho-process.jpg?acl HTTP/1.1
Host: examplebucket.data.basho.com
x-amz-acl: public-read
Accept: */*
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=
Host: data.basho.com
Connection: Keep-Alive
```

### Sample Response to Setting Permissions with Headers

```
HTTP/1.1 200 OK
x-amz-id-2: ZDsjJI9E3ke4WK56w5YegkbG6RWPxNQHIQ0CjrjyRVFZhEbabXnBO9w5G7Dmxsgk
x-amz-request-id: 827BD84C13B255B1
Date:  Fri, 01 Jun  2012 12:00:00 GMT
Content-Length: 0
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```
