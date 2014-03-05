---
title: Advanced Security
project: riak
version: 2.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, security]
---

This document provides information on using certificate- and pluggable authentication module (PAM)-based security in conjunction with Riak. If you're looking for more general information on Riak Security, it may be best to start with our guide to [[authentication and authorization]].

## Certificate-based Authentication




This guide assumes that you have set up a [Root Certificate Authority](http://en.wikipedia.org/wiki/Root_certificate) and have created and signed certificates, using [OpenSSL](https://www.openssl.org/) or another tool.




## References

- [OpenSSL tutorial](http://pages.cs.wisc.edu/~zmiller/ca-howto/)