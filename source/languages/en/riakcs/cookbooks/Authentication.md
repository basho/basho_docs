---
title: Authentication
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, authentication]
---

## Signing and Authenticating REST Requests

The primary authentication scheme available to use with Riak CS is the S3
authentication scheme. A signature is calculated using several elements from
each request and the user's `key_id` and `key_secret`. This signature is
included in the `Authorization` header of the request. Once a request is
received by the server, the server also calculates the signature for the
request and compares the result with the signature presented in then
`Authorization` header. If they match then the request is authenticated;
otherwise, the authentication fails.

Full details are available in the
[S3 authentication scheme documentation](http://docs.amazonwebservices.com/AmazonS3/latest/dev/RESTAuthentication.html).

## Query String Authentication

Riak CS also supports authentication using a query parameter. This allows
issuing of pre-signed requests that can be used to grant public access to
private Riak CS data. It also supports an expiry timestamp so that the
pre-signed URL can be invalidated after a certain period of time.

The signature in the query string secures the request and you can specify any
future expiration time in epoch or UNIX time.

1. Create a query.
2. Specify an expiration time for the query.
3. Sign it with your signature.
4. Place the data in an HTTP request.
5. Distribute the request to a user or embed the request in a web page

### Query String Parameters

**AWSAccessKeyId** - Your Riak CS Access Key ID.

* *Type*: String

**Expires** - The time when the signature expires, specified as the number of seconds since the epoch.

* *Type*: Integer

**Signature** - The URL encoding of the Base64 encoding of the HMAC-SHA1 of StringToSign.

* *Type*: String

### Example

For example, a query URL is similar to the following example.

```
http://bucket.data.basho.com/document?AWSAccessKeyId=8EE3UE-UMW1YTPMBC3EB&Expires=1177363698&Signature=vjSAMPLENmGa%2ByT272YEAiv4%3D
```
