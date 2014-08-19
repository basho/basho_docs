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

All of the above security sources can be used simultaneously in a single
Riak cluster. You can specify, for example, that clients performing
[[MapReduce|Using MapReduce]] operations authenticate using certficates
while clients performing [[K/V Operations|The Basics]] authenticate
themselves via username and password.

This document is an introduction to client-side authentication in Riak.
For implementing security in Riak itself, see the following documents:

* [[Authentication and Authorization]]
* [[Managing Security Sources]]

There are also client-library-specific guides for the following
officially supported clients:

* [[Ruby|Client-side Security: Ruby]]

## Certificates, Keys, and Authorities




