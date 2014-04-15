---
title: Client-Side Security
project: riak
version: 2.0.0+
document: tutorial
audience: advanced
keywords: [developers, security, ssl, certificate]
---

If you have enabled [[Riak Security|Authentication and Authorization]] and chosen `certificate` as your [[security source|Managing Security Sources]], any client attempting to connect to Riak will have to authenticate itself using a signed SSL certificate that matches the certificate on your Riak server.

This tutorial will show you to use certificate-based authentication for each of Riak's officially supported [[client libraries]]. A guide to server-side certificate management can be found in our documentation on [[security source management|Managing Security Sources#certificate-based-authentication]].

## Initial Setup

Let's say that (a) your server-side SSL certificates have already been created and signed and that your Riak servers have been configured to recognize those certificates, and (b) you have copies of those certificates in an `/ssl` directory that is accessible to your Riak client. In that directory are stored two SSL certificates: a `ca.crt` and a `server.crt`.




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

## Connecting

Once your client has been set up to

## Resources

* [OpenSSL tutorial](http://www.madboa.com/geek/openssl/)