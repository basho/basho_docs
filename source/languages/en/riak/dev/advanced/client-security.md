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

Let's say that (a) your server-side SSL certificates have already been created and signed and that your Riak servers have been configured to recognize those certificates, (b) you have copies of those certificates in an `/ssl` directory that is accessible to your Riak client, and (c) your client's username is `riakuser` and the password is `rosebud`.

This tutorial will not cover the basics of generating certificates, and will assume that your signing authority and cert correspond to the files specified in your `riak.conf` [[configuration file|Configuration Files]] under `ssl.cacertfile` and `ssl.certfile`, respectively. For this example, we'll use the default names for those files: `cacertfile.pem` and `cert.pem`.

You will need to point your client to those certificates upon instantiation. Let's say that you're instantiating a client for a one-node cluster, using the host `127.0.0.1` and port `10017`.

```ruby
client = Riak::Client.new(
  host: '127.0.0.1',
  pb_port: 10017,
  authentication: {
    user: 'riakuser',
    password: 'rosebud',
    server_ca: File.read('/path/to/ssl/cacertfile.pem'),
    server_cert: File.read('/path/to/ssl/cert.pem')
  }
)
```

```java
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

// Specify the location of the CA and create an input stream:
File cacert = new File("/path/to/ssl/cacertfile.pem");
FileInputStream inputStream = new FileInputStream(cacert);
CertificateFactory cFactory = CertificateFactory.getInstance("X.509");
X509Certificate caCert = (X509Certificate) cFactory.generateCertificate(inputStream);
inputStream.close();
KeyStore ks = KeyStore.getInstance(KeyStore.getDefaultType());
ks.load(null, "rosebud".toCharArray());
ks.setCertificateEntry("mycert", caCert);

// Now, a node can be created with the certificate specified upon creation:
RiakNode node = new RiakNode.Builder()
        .withAuth("riakuser", "rosebud")
```

## Connecting

Once your client has been set up to

## Resources

* [OpenSSL tutorial](http://www.madboa.com/geek/openssl/)