---
title: "Access Control Lists"
description: ""
menu:
  riak_cs-2.1.2:
    name: "Access Control Lists"
    identifier: "admin_access_control"
    weight: 101
    parent: "http_admin"
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/cookbooks/Access-Control-Lists/
  - /riak/cs/latest/cookbooks/access-control-lists/
---

Access Control Lists (ACLs) are a means of granting and denying access
to buckets and objects. Each bucket and object in a Riak CS cluster will
have an ACL associated with it. When a bucket or object is created, a
default ACL will be created alongside it that grants full control to the
creating party and denies access to all other parties.

Riak CS ACLs are modeled after S3 ACLs. For more information, see the
Amazon [Access Control List Overview](http://docs.amazonwebservices.com/AmazonS3/latest/dev/ACLOverview.html)
documentation.

> **ACL Limit**
>
> An ACL can have up to 100 grants.

## Representations

XML is the only supported external format for ACLs. In the future, other
formats such as [JSON](http://www.json.org) may be supported.

Example XML representation of an ACL:

```xml
<xml version="1.0" encoding="UTF-8">
<AccessControlPolicy xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
  <Owner>
    <ID>abcd123</ID>
    <DisplayName>joebob</DisplayName>
  </Owner>
  <AccessControlList>
    <Grant>
      <Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Canonical User">
        <ID>abcd123</ID>
        <DisplayName>joebob</DisplayName>
      </Grantee>
      <Permission>FULL_CONTROL</Permission>
    </Grant>
  </AccessControlList>
</AccessControlPolicy>
```

## Permissions

Riak CS permissions are split into two types: **bucket permissions** and
**object permissions**.

### Bucket Permissions

* `READ` --- Grantee may list the objects in the bucket
* `READ_ACP` --- Grantee may read the bucket ACL
* `WRITE` --- Grantee may create, overwrite, and delete any object in
  the bucket
* `WRITE_ACP` --- Grantee may write the ACL for the applicable bucket
* `FULL_CONTROL` --- Grantee has `READ`, `WRITE`, `READ_ACP`, and
  `WRITE_ACP` permissions on the bucket

### Object Permissions

* `READ` --- Grantee may read the object data and its metadata
* `READ_ACP` --- Grantee may read the object ACL. **Note:** The object
  owner may read the object ACL even if not explicitly granted
  `READ_ACP` permission.
* `WRITE_ACP` --- Grantee may write the ACL for the applicable object.
  **Note:** The object owner may write the object ACL even if not
  explicitly granted `WRITE_ACP` permission.
* `FULL_CONTROL` --- Grantee has `READ`, `READ_ACP`, and `WRITE_ACP`
  permissions on the object.

## Buckets

Bucket names **must** be [globally unique]({{<baseurl>}}riak/cs/2.1.2/theory/stanchion/#globally-unique-entities). To avoid conflicts, all
bucket creation requests are made to an application called
[Stanchion]({{<baseurl>}}riak/cs/2.1.2/cookbooks/configuration/stanchion). This means that all requests for modification of a bucket ACL should be serialized through Stanchion. While this may cause undesirable serialization of these requests, we
believe it is appropriate based on the following statement from this
[documentation on bucket restrictions](http://docs.amazonwebservices.com/AmazonS3/latest/dev/BucketRestrictions.html) from Amazon regarding restrictions on bucket operations:

> Because bucket operations work against a centralized, global
resource space, it is not appropriate to make bucket create or delete
calls on the high availability code path of your
application.

This statement only directly references create or delete calls, but we
have taken a more broad interpretation to include requests that modify
the ACL.

## Objects

The object ACL is stored with each object as a metadata field. If no ACL
information is present in the object creation request, a default ACL is
created granting the creator both ownership and full access control and
denying access to all other parties.

For information on specifying an ACL when making a `PUT` request, see
[Riak CS PUT Object ACL]({{<baseurl>}}riak/cs/2.1.2/references/apis/storage/s3/put-object-acl).
