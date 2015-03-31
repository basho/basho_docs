---
title: Authentication
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, authentication]
---

## Authentication Options

* S3 Signature Authentication
  * Module name: `riak_cs_s3_auth`
  * [Documentation](http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html)
* Keystone Authentication
  * Module name: `riak-cs_keystone_auth`
  * [Documentation](http://docs.openstack.org/api/openstack-identity-service/2.0/content/index.html)
* S3 Passthru Authentication
  * Module name: `riak_cs_s3_passthru_auth`
  * This module requires a valid user `key_id` to be included in the
    `Authorization` header value, but no signature is required. For
    example, a valid header using this authentication module would look
    like this: `Authorization: AWS 4REM9H9ZKMXW-DZDC8RV`.

    **Warning**: This module is only intended for use in development or
    testing scenarios.

Selecting an authentication method is done by adding or changing the
`auth_module` key in the Riak CS `riak-cs.conf` file, or the old-style
`advanced.config` or `app.config` files in the `riak_cs` section. For example,
to instruct Riak CS to use S3-style request signing as the means of
authentication, ensure the following is contained in your configuration file:

```riakcsconf
auth_module = riak_cs_s3_auth
```

```advancedconfig
{riak_cs, [
           %% Other configs
           {auth_module, riak_cs_s3_auth},
           %% Other configs
          ]}
```

```appconfig
{riak_cs, [
           %% Other configs
           {auth_module, riak_cs_s3_auth},
           %% Other configs
          ]}
```

S3-style authentication is used by default.

## S3 Authentication

### Signing and Authenticating REST Requests

The primary authentication scheme available to use with Riak CS is the S3
authentication scheme. A signature is calculated using several elements from
each request and the user's `key_id` and `key_secret`. This signature is
included in the `Authorization` header of the request. Once a request is
received by the server, the server also calculates the signature for the
request and compares the result with the signature presented in then
`Authorization` header. If they match then the request is authenticated;
otherwise, the authentication fails.

Full details are available in the [S3 authentication scheme
documentation](http://docs.amazonwebservices.com/AmazonS3/latest/dev/RESTAuthentication.html).

### Query String Authentication

Riak CS also supports authentication using a query parameter. This
allows issuing of pre-signed requests that can be used to grant public
access to private Riak CS data. It also supports an expiry timestamp so
that the pre-signed URL can be invalidated after a certain period of
time.

The signature in the query string secures the request and you can
specify any future expiration time in epoch or UNIX time.

1. Create a query
2. Specify an expiration time for the query
3. Sign it with your signature
4. Place the data in an HTTP request
5. Distribute the request to a user or embed the request in a web page

#### Query String Parameters

Parameter | Description | Data type
:---------|:------------|:---------
`AWSAccessKeyId` | Your Riak CS Access Key ID | string
`Expires` | The time when the signature expires, specified as the number of seconds since the epoch | integer
`Signature` | The URL encoding of the Base64 encoding of the HMAC-SHA1 of `StringToSign` | string

#### Example

For example, a query URL is similar to the following example.

```http
http://bucket.data.basho.com/document?AWSAccessKeyId=8EE3UE-UMW1YTPMBC3EB&Expires=1177363698&Signature=vjSAMPLENmGa%2ByT272YEAiv4%3D
```

## Keystone Authentication

More information on using Keystone for authentication with Riak CS can
be found in [[using Riak CS with Keystone]].
