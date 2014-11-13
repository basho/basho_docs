---
title: Client-side Security
project: riak
version: 2.0.0+
document: tutorial
audience: advanced
keywords: [developers, security, ssl, certificate]
---

Versions of Riak 2.0 and later come equipped with a [[security
subsystem|Authentication and Authorization]] that enables you to choose
(a) which Riak users/clients are authorized to perform a wide variety of
Riak operations and (b) how those users/clients are required to
authenticate themselves. The following four authentication mechanisms,
aka [[security sources|Managing Security Sources]] are available:

* [[Trust|Managing Security Sources#Trust-based-Authentication]]-based
  authentication enables you to specify trusted
  [CIDR](http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing)s
  from which all clients will be authenticated by default
* [[Password|Managing Security
  Sources#Password-based-Authentication]]-based authentication requires
  that clients provide a username and password
* [[Certificate|Managing Security
  Sources#Certificate-based-Authentication]]-based authentication
  requires that clients
* [[Pluggable authentication module (PAM)|Managing Security
  Sources#PAM-based-Authentication]]-based authentication requires
  clients to authenticate using the PAM service specified using the
  `[[riak-admin security|Authentication and Authorization#Managing-Sources]]`
  command line interface

Riak's approach to security is highly flexible. If you choose to use
Riak's security feature, you do not need to require that all clients
authenticate via the same means. Instead, you can specify authentication
sources on a client-by-client, i.e. user-by-user, basis. This means that
you can require clients performing, say, [[MapReduce|Using MapReduce]]
operations to use certificate auth, while clients performing [[K/V
Operations|The Basics]] have to use username and password. The approach
that you adopt will depend on your security needs.

This document is an introduction to client-side authentication in Riak.
For managing security in Riak itself, see the following documents:

* [[Authentication and Authorization]]
* [[Managing Security Sources]]

We also provide client-library-specific guides for the following
officially supported clients:

* [[Java|Client-side Security: Java]]
* [[Ruby|Client-side Security: Ruby]]
* [[Python|Client-side Security: Python]]
* [[Erlang|Client-side Security: Erlang]]

## Certificates, Keys, and Authorities

If Riak security is enabled, all client operations, regardless
of the security source you choose for those clients, must be over a
secure SSL connection. If you are using a self-generated Certificate
Authority (CA), Riak and connecting clients will need to share that CA.

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

<div class="note">
<div class="title">HTTP not supported</div>
Certificate-based authentication is available only through Riak's
[[Protocol Buffers|PBC API]] interface. It is not available through the
[[HTTP API]].
</div>

### Default Names

In Riak's [[configuration files|Configuration Files#Security]], the
default certificate file names are as follows:

Cert | Filename
:----|:-------
Certificate authority (CA) | `cacertfile.pem`
Private key | `key.pem`
CA-generated cert | `cert.pem`

These filenames will be used in the client-library-specific tutorials.
