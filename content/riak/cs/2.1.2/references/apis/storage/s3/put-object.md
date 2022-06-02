---
title: "Riak CS PUT Object"
description: ""
project: "riak_cs"
project_version: "2.1.2"
toc: true
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/RiakCS-PUT-Object/
  - /riak/cs/2.1.2/references/apis/storage/s3/RiakCS-PUT-Object/
  - /riak/cs/latest/references/apis/storage/s3/put-object/
---

The `PUT Object` operation adds an object to a bucket. The PUT Object operation does not add partial objects, so a success response indicates that the entire object was added to the bucket.

*Note:* You must have WRITE permission on a bucket to use this operation.

Riak CS is a distributed system. If it receives multiple write requests for the same object at the same time, the system will overwrite all but the last object written. If necessary, you can build versioning or object locking into your application.

To prevent the storage of data corrupted during transmission over a network, the Content-MD5 header instructs Riak CS to compare the object to the MD5 value provided. If the values don't match, the operation returns an error. In addition, if the PUT Object operation calculates the MD5, you can compare the ETag that is returned to the calculated MD5 value.

*Note*: You can configure an application to use the `100-continue` HTTP status code, which sends the Request Headers prior to sending the request body. Doing so prevents sending the message body when the message is rejected based on the headers, for example, due to authentication failure or redirect).

## Access Permissions
PUT Object offers the option to specify the permissions you want to grant to specific accounts or groups for the object. You can grant permissions to accounts or groups with request headers, using one of the following two methods:

* Specify a predefined ACL using the x-amz-acl request header. More information about predefined ACLs is available [here](http://docs.amazonwebservices.com/AmazonS3/latest/dev/ACLOverview.html#CannedACL).
* Specify access permissions explicitly using the x-amz-grant-read, x-amz-grant-write, x-amz-grant-read-acp, x-amz-grant-write-acp, x-amz-grant-full-control headers, which map to the set of ACL permissions supported by Amazon S3.

{{% note title="Note" %}}
You can use either a predefined ACL or specify access permissions explicitly,
not both.
{{% /note %}}

## Requests

### Request Syntax

```
PUT /ObjectName HTTP/1.1
Host: bucketname.data.example.com
Date: date
Authorization: signature_value
```

### Request Headers

PUT Object offers the following request headers in addition to request headers common to all operations:

**Content-Length** - The size of the object in bytes. This header is required.

* *Type*: String
* *Default*: None
* *Constraints*: None

**Content-MD5** - The base64-encoded 128-bit MD5 digest of the message without the headers according to RFC 1864. Although this header is optional, the Content-MD5 header can be used to confirm that the data is the same as what was originally sent.

* *Type*: String
* *Default*: None
* *Constraints*: None

**Content-Type** - A standard MIME type that describes the content format.

* *Type*: String
* *Default*: binary/octet-stream
* *Valid Values*: 100-continue
* *Constraints*: None

**Expect** - When you use `100-continue` in your application, it doesn't send the request body until it receives an acknowledgment. That way, the body of the message isn't sent if the message is rejected based on the headers.

* *Type*: String
* *Default*: None
* *Valid Values*: 100-continue
* *Constraints*: None

**x-amz-meta-*** - User specified metadata fields which can be stored with the object.

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

A request that stores the object, `basho-process.jpg` in the bucket, `basho_docs`.

```
PUT /basho-process.jpg HTTP/1.1
Host: basho_docs.data.basho.com
Date: Fri, 01 Jun  2012 12:00:00 GMT
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=
Content-Type: text/plain
Content-Length: 201445
Expect: 100-continue
[201445 bytes of object data]
```

### Sample Response

```
HTTP/1.1 200 OK
Date: Fri, 01 Jun  2012 12:00:00 GMT
ETag: "32cf731c97645a398434535f271b2358"
Content-Length: 0
Connection: close
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```

### Sample Request with Predefined Access Permissions

This request uses an `x-amz-acl` header to specify a predefined ACL to grant READ permission to the public.

```
...Object data in the body...
PUT draftschedule.jpg HTTP/1.1
Host: myBucket.data.basho.com
x-amz-date: b24cf9553547f8b395dd038b34a81474
x-amz-acl: public-read
Authorization: AWS AKIAIOSFODNN7EXAMPLE:xQE0diMbLRepdf3YB+FIEXAMPLE=
Content-Length: 300
Expect: 100-continue
Connection: Keep-Alive

...Object data in the body...
```

### Sample Response for Predefined Access Permissions

```
HTTP/1.1 200 OK
Date: b24cf9553547f8b395dd038b34a81474
ETag: "b24cf9553547f8b395dd038b34a81474"
Content-Length: 0
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
```
