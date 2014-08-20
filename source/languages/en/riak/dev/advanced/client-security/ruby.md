---
title: "Client-side Security: Ruby"
project: riak
version: 2.0.0+
document: tutorial
audience: advanced
keywords: [developers, security, ssl, ruby]
---

This tutorial shows you how to set up a Riak Ruby client to authenticate
itself when connecting to Riak using all four of the [[available
security sources|Managing Security Sources]]:

* [[trust|Managing Security Sources#PAM-based-Authentication]]
* [[password|Managing Security Sources#Password-based-Authentication]]
* [[certificates|Managing Security Sources#Certificate-based-Authentication]]
* [[pluggable authentication modules (PAM)|Managing Security
  Sources#PAM-based-Authentication]])

**Note**: This tutorial does not cover certificate generation. It
assumes that all necessary certificates have already been created and
are stored in a directory called `/ssl_dir`. This directory name is used
only for example purposes.

## Ruby Client Basics

When connecting to Riak using a Ruby-based client, you typically
instantiate an object of from the `Riak::Client` class that then handles
all interactions with Riak. All authentication-related information that
needs to be used by the client object can be passed to the object upon
instantiation in an `authentication` hash.

If you are using Riak Security, _all_ connecting clients should have
access to the same Certificate Authority (CA) used on the server side,
regardless of which [[security source|Managing Security Sources]] you
choose. All clients should also provide a username. The example below
sets up a client object (we'll simply call it `client`) that connects
to Riak on `localhost` and on port 8087, specifies `riakuser` as a
username, and points the client to a CA located at
`/ssl_dir/cacertfile.pem`.

```ruby
client = Riak::Client.new(
  host: '127.0.0.1',
  pb_port: 8087,
  authentication: {
    ca_file: '/ssl_dir/cacertfile.pem',
    user: 'riakuser'
  }
)
```

This client object is currently not set up to use any of the available
security sources, except trust-based auth, provided that the CIDR from
which the client is connecting has been specified as trusted. More on
this in [[Trust-based Authentication|Managing Security
Sources#Trust-based-Authentication]].

## Password-based Authentication

To enable our client to use password-based auth, we can use most of the
information from the example above, with the exception that we will
specify a password for the client in the `authentication` hash. We'll
use the password `rosebud` here and in the rest of the examples.

```ruby
client = Riak::Client.new(
  # Using the host and pb_port from above
  authentication: {
    ca_file: '/ssl_dir/cacertfile.pem',
    user: 'riakuser',
    password: 'rosebud'
  }
)
```

## Certificate-based Authentication

Using certificate-based authentication requires us to specify the
location of a CA (as with all security sources), a username, a
client-specific CA, a CA-generated cert, and a private key. We'll assume
that all certs are stored in `/ssl_dir`, as in the previous examples.

```ruby
client = Riak::Client.new(
  # Using the host and pb_port from above
  authentication: {
    ca_file: '/path/to/cacertfile.pem',
    user: 'riakuser',
    client_ca: '/path/to/client_cert.pem',
    cert: '/path/to/cert.pem',
    key: '/path/to/key.pem'
  }
)
```

The `client_ca` must be specified if you intend to use a CA that is
different from the CA used by Riak, e.g. if you are integrating with
an existing single sign-on (SSO) system. If the client and server CA are
the same, you don't need to specify `client_ca`. The client cert and
key, however, must always be specified.

The `client_ca`, `cert`, and `key` fields are all flexible in their
usage. You can use a string specifying a filename (as in the example
above), or you can pass in an appropriate OpenSSL object, e.g. an SSL
object created using the
[OpenSSL](http://ruby-doc.org/stdlib-2.0/libdoc/openssl/rdoc/OpenSSL.html)
gem. If you use specify filenames, those files will be loaded and
converted into the appropriate OpenSSL object.

## Certificate Revocation Lists

If you create certificates specifying a CA-signed Certificate Revocation
List (CRL), those certs will be checked against the CRLs specified. You
can specify the location of the list in the `authentication` hash:

```ruby
client = Riak::Client.new(
  # Using the host and pb_port from above
  authentication: {
    ca_file: '/ssl_dir/cacertfile.pem',
    user: 'riakuser',
    # Using the cert paths from above
    crl_file: '/ssl_dir/revocation.crl'
  }
)
```

CRL checking can sometimes be a slow process. To disable it, you can set
`crl` to `false` in the `authentication` hash when instantiating your
client object.

## Online Certificate Status Protocol

If you create certificates with a specified Online Certificate Status
Protocol ([OCSP](http://en.wikipedia.org/wiki/Online_Certificate_Status_Protocol)),
the OCSP endpoint will automatically be checked. If that endpoint is not
available or if checking is running slowly, you can disable OCSP
checking by setting `ocsp` to `false` in the `authentication` hash.
