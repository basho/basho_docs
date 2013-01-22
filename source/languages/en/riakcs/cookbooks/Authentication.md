---
title: Authentication
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator]
---

Currently, the only authentication scheme available to use with Riak CS is the S3 authentication scheme. A signature is calculated using several elements from each request and the user's `key_id` and `key_secret`. This signature is included in the Authorization header of the request. Once a request is received by the server, the server also calculates the signature for the request and compares the result with the signature presented in then Authorization header. If they match then the request is authenticated; otherwise, the authentication fails.

Full details are available in the [S3 authentication scheme documentation](http://docs.amazonwebservices.com/AmazonS3/latest/dev/RESTAuthentication.html).

Riak CS also supports authentication using the query parameter method described in the Amazon authentication document at the above URL.

This basically allows issuing of pre-signed requests that can be used to grant public access to private Riak CS data. It also supports an expiry timestamp so that the pre-signed URL can be invalidated after a certain period of time.