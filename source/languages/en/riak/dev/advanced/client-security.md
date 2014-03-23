---
title: Client-Side Security
project: riak
version: 2.0.0+
document: tutorial
audience: intermediate
keywords: [developers, security, ssl, certificate]
---

If you have chosen `certificate` as your [[security source|Managing Security Sources]], any client attempting to connect to Riak will have to authenticate itself using a signed SSL certificate. This tutorial will show you to use certificate-based authentication for Riak's officially supported [[client libraries]].

## Initial Setup

This tutorial will not cover the basics of generating certificates, and will assume that you have `ca.crt` and `server.crt` stored in the directory `/ssl`. You will need to point your client at those certificates upon instantiation. Let's say that we're instantiating a client for the host `127.0.0.1` and port `10017`.

```ruby
client = Riak::Client.new(
                           host: '127.0.0.1',
                           pb_port: 10017,
                           authentication: {
                             user: 'riakuser',
                             password: 'rosebud',
                             server_ca: File.read('/ssl/ca.crt'),
                             server_cert: File.read('/ssl/server.crt')
                           }
                         )
```

## Resources

* [OpenSSL tutorial](http://www.madboa.com/geek/openssl/)