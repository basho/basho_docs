---
title: "Client Security"
description: ""
project: "riak_kv"
project_version: "2.0.1"
menu:
  riak_kv-2.0.1:
    name: "Security"
    identifier: "usage_security"
    weight: 114
    parent: "developing_usage"
toc: true
aliases:
  - /riak/2.0.1/dev/advanced/client-security
  - /riak/kv/2.0.1/dev/advanced/client-security
---

Versions of Riak 2.0 and later come equipped with a [security subsystem]({{<baseurl>}}riak/kv/2.0.1/using/security/basics) that enables you to choose

* which Riak users/clients are authorized to perform a wide variety of
  Riak operations, and
* how those users/clients are required to authenticate themselves.

The following four authentication mechanisms, aka [security sources]({{<baseurl>}}riak/kv/2.0.1/using/security/managing-sources/) are available:

* [Trust]({{<baseurl>}}riak/kv/2.0.1/using/security/managing-sources/#trust-based-authentication)-based
  authentication enables you to specify trusted
  [CIDR](http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing)s
  from which all clients will be authenticated by default
* [Password]({{<baseurl>}}riak/kv/2.0.1/using/security/managing-sources/#password-based-authentication)-based authentication requires
  that clients provide a username and password
* [Certificate]({{<baseurl>}}riak/kv/2.0.1/using/security/managing-sources/#certificate-based-authentication)-based authentication
  requires that clients
* [Pluggable authentication module (PAM)]({{<baseurl>}}riak/kv/2.0.1/using/security/managing-sources/#pam-based-authentication)-based authentication requires
  clients to authenticate using the PAM service specified using the
  `[riak-admin security]({{<baseurl>}}riak/kv/2.0.1/using/security/managing-sources/#managing-sources)`
  command line interface

Riak's approach to security is highly flexible. If you choose to use
Riak's security feature, you do not need to require that all clients
authenticate via the same means. Instead, you can specify authentication
sources on a client-by-client, i.e. user-by-user, basis. This means that
you can require clients performing, say, [MapReduce]({{<baseurl>}}riak/kv/2.0.1/developing/usage/mapreduce/)
operations to use certificate auth, while clients performing [K/V Operations]({{<baseurl>}}riak/kv/2.0.1/developing/usage) have to use username and password. The approach
that you adopt will depend on your security needs.

This document provides a general overview of how that works. For
managing security in Riak itself, see the following documents:

* [Authentication and Authorization]({{<baseurl>}}riak/kv/2.0.1/using/security/basics)
* [Managing Security Sources]({{<baseurl>}}riak/kv/2.0.1/using/security/managing-sources/)

We also provide client-library-specific guides for the following
officially supported clients:

* [Java]({{<baseurl>}}riak/kv/2.0.1/developing/usage/security/java)
* [Ruby]({{<baseurl>}}riak/kv/2.0.1/developing/usage/security/ruby)
* [PHP]({{<baseurl>}}riak/kv/2.0.1/developing/usage/security/php)
* [Python]({{<baseurl>}}riak/kv/2.0.1/developing/usage/security/python)
* [Erlang]({{<baseurl>}}riak/kv/2.0.1/developing/usage/security/erlang)

## Certificates, Keys, and Authorities

If Riak security is enabled, all client operations, regardless of the
security source you choose for those clients, must be over a secure SSL
connection. If you are using a self-generated Certificate Authority
(CA), Riak and connecting clients will need to share that CA.

To use certificate-based auth, you will need to create a Public Key
Infrastructure (PKI) based on
[x.509](http://en.wikipedia.org/wiki/X.509) certificates. The central
foundation of your PKI should be a Certificate Authority (CA), created
inside of a secure environment, that can be used to sign certificates.
In addition to a CA, your client will need to have access to a private
key shared only by the client and Riak as well as a CA-generated
certificate.

To prevent so-called [Man-in-the-Middle
attacks](http://en.wikipedia.org/wiki/Man-in-the-middle_attack), private
keys should never be shared beyond Riak and connecting clients.

> **HTTP not supported**
>
> Certificate-based authentication is available only through Riak's
[Protocol Buffers]({{<baseurl>}}riak/kv/2.0.1/developing/api/protocol-buffers/) interface. It is not available through the
[HTTP API]({{<baseurl>}}riak/kv/2.0.1/developing/api/http).

### Default Names

In Riak's [configuration files]({{<baseurl>}}riak/kv/2.0.1/configuring/reference/#security), the
default certificate file names are as follows:

Cert | Filename
:----|:-------
Certificate authority (CA) | `cacertfile.pem`
Private key | `key.pem`
CA-generated cert | `cert.pem`

These filenames will be used in the client-library-specific tutorials.
