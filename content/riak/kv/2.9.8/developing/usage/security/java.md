---
title_supertext: "Client Security:"
title: "Java"
description: ""
project: "riak_kv"
project_version: 2.9.8
menu:
  riak_kv-2.9.8:
    name: "Java"
    identifier: "usage_security_java"
    weight: 100
    parent: "usage_security"
toc: true
aliases:
  - /riak/2.9.8/dev/advanced/client-security/java
  - /riak/kv/2.9.8/dev/advanced/client-security/java
---

This tutorial shows you how to set up a Riak Java client to authenticate
itself when connecting to Riak.

If you are using [trust-]({{<baseurl>}}riak/kv/2.9.8/using/security/managing-sources/#trust-based-authentication) or [PAM]({{<baseurl>}}riak/kv/2.9.8/using/security/managing-sources/#pam-based-authentication)-based authentication, you can use the
security setup described [below](#java-client-basics). [Certificate]({{<baseurl>}}riak/kv/2.9.8/using/security/managing-sources/#certificate-based-authentication)-based authentication is not
yet supported in the Java client.

{{% note title="Note on certificate generation" %}}
This tutorial does not cover certificate generation. It assumes that all
necessary certificates have already been created and are stored in a directory
called `/ssl_dir`. This directory name is used only for example purposes.
{{% /note %}}

## Java Client Basics

When connecting to Riak using a Java-based client, you typically do so
by instantiating separate `RiakNode` objects for each node in your
cluster, a `RiakCluster` object registering those `RiakNode` objects,
and finally a `RiakClient` object that registers the general cluster
configuration. In this document, we will be working with only one node.

If you are using Riak security, _all_ connecting clients should have
access to the same Certificate Authority (CA) used on the server side,
regardless of which [security source]({{<baseurl>}}riak/kv/2.9.8/using/security/managing-sources/) you
choose. All clients should also provide a username, regardless of
security source. The example below sets up a single node object (we'll
simply call it `node`) that connects to Riak on `localhost` and on port
8087 and specifies `riakuser` as a username. That object will be used to
create a cluster object (we'll call it `cluster`), which will in turn be
used to create a `client` object. The setup below does not specify a CA:

```java
import com.basho.riak.client.api.RiakClient;
import com.basho.riak.client.api.RiakCluster;
import com.basho.riak.client.api.RiakNode;

RiakNode node = new RiakNode.Builder()
        .withRemoteAddress("127.0.0.1")
        .withRemotePort(8087)
        // This will specify a username but no password or keystore:
        .withAuth("riakuser", null, null)
        .build();

RiakCluster cluster = new RiakCluster.Builder(node)
        .build();

RiakClient client = new RiakClient(cluster);
```

This client object is not currently set up to use any of the available
security sources. This will change in the sections below.

## Password-based Authentication

To enable our client to use password-based auth, we can use most of the
setup from the example above, with the exception that we will specify a
password for the client in the `withAuth` method in the `node` object's
constructor rather than leaving it as `null`. We will also pass a
`KeyStore` object into that method.

```java
import java.io.FileInputStream;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

// Generate an InputStream from the CA cert
InputStream inputStream = new InputStream("/ssl_dir/cacertfile.pem");

// Generate an X509Certificate from the InputStream and close the stream
CertificateFactory certFactory = CertificateFactory.getInstance("X.509");
X509Certificate caCert = (X509Certificate) certFactory.generateCertificate(inputStream);
inputStream.close();

// Generate a KeyStore object
KeyStore ks = KeyStore.getInstance(KeyStore.getDefaultType());
ks.load(null, "password".toCharArray());
ks.setCertificateEntry("cacert", caCert);

RiakNode node = new RiakNode.Builder()
        .withRemoteAddress("127.0.0.1")
        .withRemotePort(8087)
        .withAuth("riakuser", "rosebud", ks)
        .build();

// Construct the cluster and client object in the same fashion as above
```

## PAM- and Trust-based Authentication

If you are using PAM- or trust-based authentication, the only difference
from password-based authentication is that you do not need to specify a
password.

## Certificate-based Authentication

Certificate-based authentication is not currently supported in the
official Riak Java client.




