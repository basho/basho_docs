---
title: "Common Riak CS Request Headers"
description: ""
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/Common-RiakCS-Request-Headers
  - /riak/cs/2.1.2/references/apis/storage/s3/Common-RiakCS-Request-Headers
  - /riak/cs/latest/references/apis/storage/s3/common-request-headers/
---

These are the headers that are common to all Riak CS REST requests.

Header | Description | Data type
:------|:------------|:---------
`Authorization` | Information required to requests authentication. This header is not required for anonymous requests.
`Cache-Control` | This header is for use by caches and intermediate proxies. It can be any string. {{1.5.0+}}
`Content-Length` | The length of the message without headers according to [RFC 2616](https://www.ietf.org/rfc/rfc2616.txt). This header is required for PUTs and for operations that load XML.
`Content-Type` | The content type of the resource, e.g. `application/json`.
`Content-MD5` | The base64-encoded 128-bit MD5 digest of the message without the headers according to [RFC 1864](https://www.ietf.org/rfc/rfc1864.txt). Although this header is optional, the `Content-MD5` header can be used to confirm that the data is the same as what was originally sent.
`Date` | The current data and time according to the requester, e.g. `Fri, 01 Jun 2012 12:00:00 GMT`. With the `Authorization` header, you must specify either the `x-amz-date` or `Date` header.
`Expect` | When you use `100-continue` in your application, it doesn't send the request body until it receives an acknowledgment. That way, the body of the message isn't sent if the message is rejected based on the headers.
`Host` | For path-style requests, the value is something like `data.basho.com`. For virtual-style requests, the value is something like `bucketname.data.basho.com`.
`x-amz-date` | This  header is optional for HTTP 1.0 requests but required for HTTP 1.1. Registers the current date and time according to the requester, e.g. `Fri, 01 Jun 2012 12:00:00 GMT`. With the `Authorization` header, you must specify either the `x-amz-date` or `Date` header. If you specify both, the value for this header takes precedence.
