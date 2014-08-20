---
title: "Client-side Security: Java"
project: riak
version: 2.0.0+
document: tutorial
audience: advanced
keywords: [developers, security, ssl, python]
---

This tutorial shows you how to set up a Java Riak client to authenticate
itself when connecting to Riak using all four of the [[avaliable
security sources|Managing Security Sources]].

* [[trust|Managing Security Sources#PAM-based-Authentication]]
* [[password|Managing Security Sources#Password-based-Authentication]]
* [[certificates|Managing Security Sources#Certificate-based-Authentication]]
* [[pluggable authentication modules (PAM)|Managing Security
  Sources#PAM-based-Authentication]])

**Note**: This tutorial does not cover certificate generation. It
assumes that all necessary certificates have already been created and
are stored in a directory called `/ssl_dir`. This directory name is used
only for example purposes.

## Java Client Basics
