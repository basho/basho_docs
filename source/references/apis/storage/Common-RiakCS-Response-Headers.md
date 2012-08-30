---
title: RiakCS Common Response Headers
project: riakcs
version: 1.2.0+
document: api
toc: true
index: false
audience: advanced
keywords: [reference, api]
---

These are the headers that are common to all Riak CS REST responses.

**Content-Length** - The length in bytes of the response body.

* *Type*: String
* *Default*: None

**Connection** - Whether the connection to the server is open or closed.

* *Type*: Enum
* *Valid Values*: open|close
* *Default*: None

**Date** - The date and time that Riak CS responded, for example, Fri, 01 Jun 2012 12:00:00 GMT.

* *Type*: String
* *Default*: None

**ETag** - The entity tag is an MD5 hash of the object and reflects only changes to the object contents, not the object's metadata. The ETag is set when an object is created.


* *Type*: String

**Server** - The name of the server that created the response.

* *Type*: String
* *Default*: None
