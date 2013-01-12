---
title: HTTP Reset Bucket Properties
project: riak
version: 1.3+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Bucket Operations"
---

Resets the bucket properties like "n_val" and "allow_mult" back to the default settings.

## Request
```bash
DELETE /buckets/bucket/props
```

Resetting bucket properties is not available via the old API format. 

## Response

Normal status codes:

* `204 No Content`

Typical error codes:

* `405 Method Not Allowed` - returned if the operation is attempted using the old API 
format `/riak/bucket`. 

## Example

```bash
curl -XDELETE -v localhost:8098/buckets/bucket/props                                                                                                             {13:47}
* About to connect() to localhost port 8098 (#0)
*   Trying 127.0.0.1...
* connected
* Connected to localhost (127.0.0.1) port 8098 (#0)
> DELETE /buckets/bucket/props HTTP/1.1
> User-Agent: curl/7.24.0 (x86_64-apple-darwin12.0) libcurl/7.24.0 OpenSSL/0.9.8r zlib/1.2.5
> Host: localhost:8098
> Accept: */*
> 
< HTTP/1.1 204 No Content
< Vary: Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.2 (someone had painted it blue)
< Date: Tue, 06 Nov 2012 21:56:17 GMT
< Content-Type: application/json
< Content-Length: 0
< 
* Connection #0 to host localhost left intact
* Closing connection #0
```