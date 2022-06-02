---
title: "Riak CS PUT Object (Copy)"
description: ""
project: "riak_cs"
project_version: "2.1.2"
toc: true
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/RiakCS-PUT-Object-Copy/
  - /riak/cs/2.1.2/references/apis/storage/s3/RiakCS-PUT-Object-Copy/
  - /riak/cs/latest/references/apis/storage/s3/put-object-copy/
---

The `PUT Object (Copy)` creates a copy of an object that is already stored in Riak CS. Adding the `x-amz-copy-source` HTTP header makes the PUT operation copy the source object into the destination bucket.

## Access Permissions

PUT Object (Copy) offers the option to specify the permissions you want to grant to specific accounts or groups for the copied object. You can grant permissions to accounts or groups with request headers, using one of the following two methods:

* Specify a predefined ACL using the `x-amz-acl` request header. More information about predefined ACLs is available [here](http://docs.amazonwebservices.com/AmazonS3/latest/dev/ACLOverview.html#CannedACL).
* Specify access permissions explicitly using the `x-amz-grant-read`, `x-amz-grant-write`, `x-amz-grant-read-acp`, `x-amz-grant-write-acp`, `x-amz-grant-full-control` headers, which map to the set of ACL permissions supported by Amazon S3.

{{% note title="Note" %}}
You can use either a predefined ACL or specify access permissions explicitly,
not both.
{{% /note %}}

*Note*: You can configure an application to use the `100-continue` HTTP status code, which sends the Request Headers prior to sending the request body. Doing so prevents sending the message body when the message is rejected based on the headers, for example, due to authentication failure or redirect).


## Requests

### Request Syntax

```
PUT /ObjectNameCopy HTTP/1.1
Host: bucketname.data.example.com
x-amz-copy-source: /ObjectName
Date: date
Authorization: signature_value
```

### Request Headers

PUT Object (Copy) offers the following request headers in addition to request headers common to all operations:

**Expect** - When you use `100-continue` in your application, it doesn't send the request body until it receives an acknowledgment. That way, the body of the message isn't sent if the message is rejected based on the headers.

* *Type*: String
* *Default*: None
* *Valid Values*: 100-continue
* *Constraints*: None

**x-amz-copy-source** - Path to source object (object to be copied).

* *Type*: String
* *Default*: None
* *Constraints*: None

#### Permission Request Headers

**x-amz-acl** - This request header specifies a predefined ACL to apply to the object being created. A predefined ACL grants specific permissions to individual accounts or predefined groups.

* *Type*: String
* *Valid Values*: private | public-read | public-read-write | authenticated-read | bucket-owner-read | bucket-owner-full-control
* *Constraints*: None

## Examples

### Sample Request

A request that copies the object, `basho-process.jpg` in the bucket, `basho_docs`.

```
PUT /basho-process.jpg HTTP/1.1
Host: basho_docs.data.basho.com
x-amz-copy-source: /basho-process.jpg
Date: Mon, 18 Feb 2013 16:38:49 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=
Content-Type: text/plain
Content-Length: 0
Expect: 100-continue
```

### Sample Response

```
HTTP/1.1 100 Continue
HTTP/1.1 200 OK
Server: Riak CS
ETag: "d41d8cd98f00b204e9800998ecf8427e"
Date: Mon, 18 Feb 2013 16:38:49 GMT
Content-Type: text/plain
Content-Length: 0
Connection: close
```
