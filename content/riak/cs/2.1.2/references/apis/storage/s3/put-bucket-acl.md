---
title: "Riak CS PUT Bucket ACL"
description: ""
project: "riak_cs"
project_version: "2.1.2"
toc: true
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/RiakCS-PUT-Bucket-ACL/
  - /riak/cs/2.1.2/references/apis/storage/s3/RiakCS-PUT-Bucket-ACL/
  - /riak/cs/latest/references/apis/storage/s3/put-bucket-acl/
---

The `PUT Bucket acl` operation uses the `acl` subresource to set the permissions on an existing bucket using an access control list (ACL).

*Note:* You must have WRITE_ACP access to the bucket to use this operation.

`PUT Bucket acl` offers two methods for setting a bucket's permissions:

* Specify the ACL in the request body
* Specify permissions using request headers

*Note*: You can specify an ACL in the request body or with request headers, not both.

## Requests

### Request Syntax

This example shows the syntax for setting the ACL in the request body. The Request Headers section contain a list of headers you can use instead.

```
PUT /?acl HTTP/1.1
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
`PUT Bucket acl` offers the following request headers in addition to the request headers common to all operations.

**x-amz-acl** - This request header specifies a predefined ACL to apply to the bucket being created. A predefined ACL grants specific permissions to individual accounts or predefined groups.


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
* *Valid Values*: FULL_CONTROL|WRITE|WRITE_ACP|READ|READ_ACP
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

### Sample Request with Access Permission Specified in the Request Body
This sample request grants access permission to an existing bucket, named basho_docs, by specifying the ACL in the request body. In addition to granting full control to the bucket owner, the request specifies the following grants:

* Grant AllUsers group READ permission on the bucket.
* Grant the Dev group WRITE permission on the bucket.
* Grant an account, which is identified by email address, WRITE_ACP permission.
* Grant an account, which is identified by canonical user ID, READ_ACP permission.

```
PUT /?acl HTTP/1.1
Host: basho_docs.data.basho.com
Content-Length: 1660202
x-amz-date: Fri, 01 Jun  2012 12:00:00 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=

  <AccessControlPolicy xmlns="http://data.basho.com/doc/2012-04-05/">
    <Owner>
      <ID>BucketOwnerCanonicalUserID</ID>
      <DisplayName>OwnerDisplayName</DisplayName>
    </Owner>
    <AccessControlList>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser">
          <ID>852b113e7a2f25102679df27bb0ae12b3f85be6BucketOwnerCanonicalUserID</ID>
          <DisplayName>OwnerDisplayName</DisplayName>
        </Grantee>
        <Permission>FULL_CONTROL</Permission>
      </Grant>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Group">
          <URI xmlns="">http://acs.data.basho.com/groups/global/AllUsers</URI>
        </Grantee>
        <Permission xmlns="">READ</Permission>
      </Grant>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Group">
          <URI xmlns="">http://acs.data.basho.com/groups/global/Dev</URI>
        </Grantee>
        <Permission xmlns="">WRITE</Permission>
      </Grant>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="AmazonCustomerByEmail">
          <EmailAddress xmlns="">user1@basho.com</EmailAddress>
        </Grantee>
        <Permission xmlns="">WRITE_ACP</Permission>
      </Grant>
      <Grant>
        <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser">
          <ID xmlns="">f30716ab7115dcb44a5ef76e9d74b8e20567f63TestAccountCanonicalUserID</ID>
        </Grantee>
        <Permission xmlns="">READ_ACP</Permission>
      </Grant>
    </AccessControlList>
  </AccessControlPolicy>
```

### Sample Response

```
HTTP/1.1 200 OK
Date: Fri, 01 Jun  2012 12:00:00 GMT
Content-Length: 0
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```
