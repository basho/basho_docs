---
title_supertext: "Client Security:"
title: "Python"
description: ""
project: "riak_kv"
project_version: "2.0.8"
menu:
  riak_kv-2.0.8:
    name: "Python"
    identifier: "usage_security_python"
    weight: 102
    parent: "usage_security"
toc: true
aliases:
  - /riak/2.0.8/dev/advanced/client-security/python
  - /riak/kv/2.0.8/dev/advanced/client-security/python
---

This tutorial shows you how to set up a Riak Python client to
authenticate itself when connecting to Riak.

If you are using [trust-]({{<baseurl>}}riak/kv/2.0.8/using/security/managing-sources/) or [PAM-]({{<baseurl>}}riak/kv/2.0.8/using/security/managing-sources/#pam-based-authentication), you can use the security
setup described [below](#python-client-basics). [Password]({{<baseurl>}}riak/kv/2.0.8/using/security/managing-sources/#password-based-authentication)-based authentication is covered
in a [later section](#password-based-authentication). If you are using
[certificate]({{<baseurl>}}riak/kv/2.0.8/using/security/managing-sources/#certificate-based-authentication)-based authentication, follow
the instructions in the [section below](#certificate-based-authentication).

{{% note title="Note on certificate generation" %}}
This tutorial does not cover certificate generation. It assumes that all
necessary certificates have already been created and are stored in a directory
called `/ssl_dir`. This directory name is used only for example purposes.
{{% /note %}}

## OpenSSL Versions

The Riak Python client requires that you install OpenSSL 1.0.1g or
later. If you have an earlier version installed, you will receive a
warning along the following lines:

```
Found OpenSSL 0.9.8za 5 Jun 2014 version, but expected at least OpenSSL 1.0.1g.  Security may not support TLS 1.2.
```

## Python Client Basics

When connecting to Riak using a Python-based client, you typically
instantiate an object from the `RiakClient` class that then handles all
interactions with Riak. All authentication-related information that
needs to be used by the client object can be passed to the object upon
instantiation by creating a `SecurityCreds` object.

If you are using Riak Security, _all_ connecting clients should have
access to the same Certificate Authority (CA) used on the server side,
regardless of which [security source]({{<baseurl>}}riak/kv/2.0.8/using/security/managing-sources/) you
choose. All clients should also provide a username. The example below
sets up a client object (we'll simply call it `client`) that connects to
Riak on `localhost` and on port 8087 without any security credentials:

```python
from riak import RiakClient

client = RiakClient(host='127.0.0.1', pb_port=8087)
```

To provide security credentials, we'll create an object called `creds`
and specify `riakuser` as the username. We'll also point the client to a
CA stored at `/ssl_dir/cacertfile.pem`.

```python
creds = SecurityCreds(username='riakuser',
                      cacert_file='/ssl_dir/cacertfile.pem')
```

Now we can specify those credentials when we create our `client` object.

```python
client = RiakClient(host='127.0.0.1', pb_port=8087, credentials=creds)
```

This client object is not currently set up to use any of the
available security sources with the exception of trust-based auth,
provided that the
[CIDR](http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing) from
which the client is connecting has been specified as trusted. More on
specifying trusted CIDRs can be found in [Trust-based
Authentication]({{<baseurl>}}riak/kv/2.0.8/using/security/managing-sources/#Trust-based-Authentication).

**Note**: The examples in the following sections specify certs on the
basis of their filepaths, e.g. `/ssl_dir/cacertfile.pem`. In addition to
specifying certs by location, you can also provide OpenSSL objects
instead. You can find out how to do so in [Using OpenSSL Objects](#using-openssl-objects) below.

## Password-based Authentication

To enable our client to use password-based auth, we can use most of the
information from the above, with the exception that we'll also specify a
password for the client in the `creds` object from above. We'll use the
password `rosebud` here and in the rest of the examples.

```python
creds = SecurityCreds(username='riakuser',
                      cacert_file='/ssl_dir/cacertfile.pem',
                      password='rosebud')
```

## PAM-based Authentication

If you have specified that a specific client be authenticated using
[PAM]({{<baseurl>}}riak/kv/2.0.8/using/security/managing-sources/#pam-based-authentication), you will
need to provide a CA as well as the username and password that you
specified when creating the user in Riak. For more, see our
documentation on [User Management]({{<baseurl>}}riak/kv/2.0.8/using/security/basics/#user-management).

## Certificate-based Authentication

Using certificated-based authentication requires us to specify the
location of a general CA (as with all security sources), a username, a
CA-generated cert, and a private key. We'll assume that all certs are
stored in `/ssl_dir`, as in the previous examples.

```python
creds = SecurityCreds(username='riakuser',
                      cacert_file='/ssl_dir/cacertfile.pem',
                      cert_file='/ssl_dir/cert.pem',
                      pkey_file='/ssl_dir/key.pem')
```

## Specifying a Certificate Revocation List

If you are using a CA-generated Certificate Revocation List (CRL), you
can specify its filepath using the `crl_file` parameter.

```python
creds = SecurityCreds(username='riakuser',
                      # Using the cert information from above
                      crl_file='/ssl_dir/revocation.crl')
```

## Specifying Ciphers

To specify a list of preferred [security ciphers]({{<baseurl>}}riak/kv/2.0.8/using/security/basics/#security-ciphers), you can pass in a colon-delimited
string to the `ciphers` parameter:

```python
creds = SecurityCreds(username='riakuser',
                      # Using the cert information from above
                      ciphers='X-CIPHER-1:X-CIPHER-2:X-CIPHER-3:ETC')
```

## Using OpenSSL Objects

Whenever you specify certs, you have the option of either passing in
file paths as strings (as in the examples above) or properly created
OpenSSL objects, e.g. objects created using the
[pyOpenSSL](https://pyopenssl.readthedocs.org/en/latest/) library. If
you generate OpenSSL objects this way, you should note that they must
be specified differently when creating a `SecurityCreds` object. The
table below lists the appropriate parameter names for each method, as
well as the pyOpenSSL class to which each cert must belong if you create
OpenSSL objects.

Cert | File path | OpenSSL object | Class
:----|:----------|:---------------|:-----
Certificate Authority (CA) | `cacert_file` | `cacert` | `OpenSSL.crypto.X509`
Private key | `key_file` | `key` | `OpenSSL.crypto.PKey`
CA-generated cert | `cert` | `cert_file` | `OpenSSL.crypto.X509`
CRL | `crl` | `crl_file` | `OpenSSL.crypto.CRL`

If you specify filepaths, the appropriate certs will be loaded and
converted into the appropriate OpenSSL object. The functions used for
this are `OpenSSL.crypto.load_privatekey()` for the private key and
`OpenSSL.crypto.load_certificate` for the cert and CA cert.
