---
title: Client-side Security
project: riak
version: 2.0.0+
document: tutorial
audience: advanced
keywords: [developers, security, ssl, certificate]
---

Versions of Riak 2.0 and later provide you the option of requiring that
all connecting clients authenticate themselves over a secure SSL
connection prior to being able to perform specific actions in Riak. You
can assign one of four [[security sources|Managing Security Sources]] to
connecting clients:

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
Riak's security features, you do not need to require that all clients
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

* [[Ruby|Client-side Security: Ruby]]

## Certificates, Keys, and Authorities

To use client-side authentication with Riak, you will need to create a
Public Key Infrastructure (PKI) based on
[x.509](http://en.wikipedia.org/wiki/X.509) certificates. The central
foundation of your PKI should be a Certificate Authority (CA), created
inside of a secure environment, that can be used to sign certificates.

### Server-side Certificates

Assuming that you control both the CA and the server, you should use
your CA to generate a [private key](http://www.comodo.com/resources/small-business/digital-certificates2.php)
and a [Certificate Signing Request](http://en.wikipedia.org/wiki/Certificate_signing_request) \(CSR)
on behalf of the server. The certificate generated

### Default Names

In Riak's [[configuration files|Configuration Files#Security]], the
default certificate file names are as follows:

Cert | Filename
:----|:-------
Certificate authority (CA) | `cacertfile.pem`
Private key | `key.pem`
CA-generated cert | `cert.pem`

These filenames will be used in this document as well as in the
client-library-specific tutorials.

## Certificate Generation Example


