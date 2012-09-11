---
title: RiakCS Storage API
project: riakcs
version: 1.2.0+
document: api
toc: true
index: true
audience: advanced
keywords: [api, http]
---


The storage API is compatabile with the Amazon S3 REST API which means that any of the operations listed can be executed using any of the commonly available S3 libraries or tools.

## Service-level Operations

* [[GET Service|RiakCS GET Service]] - Returns a list of all buckets owned by the user who sent the request

## Bucket-level Operations

* [[GET Bucket|RiakCS GET Bucket]] - Returns a list of the objects within a bucket
* [[GET Bucket ACL|RiakCS GET Bucket ACL]] - Returns the ACL associated with a bucket
* [[PUT Bucket|RiakCS PUT Bucket]] - Creates a new bucket
* [[PUT Bucket ACL|RiakCS PUT Bucket ACL]] - Sets the ACL permissions for a bucket
* [[DELETE Bucket|RiakCS DELETE Bucket]] - Deletes a bucket

## Object-level Operations

* [[GET Object|RiakCS GET Object]]- Retrieves an object
* [[GET Object ACL|RiakCS GET Object ACL]] - Returns the ACLs associated with an object
* [[PUT Object|RiakCS PUT Object]] - Stores an object to a bucket
* [[PUT Object ACL|RiakCS PUT Object ACL]] - Sets the ACLs associated with an object
* [[HEAD Object|RiakCS HEAD Object]] - Retrieves object metadata (not the full content of the object)
* [[DELETE Object|RiakCS DELETE Object]]- Deletes an object

## Common Headers

* [[Common RiakCS Request Headers]]
* [[Common RiakCS Response Headers]]
