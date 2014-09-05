---
title: "Client-side Security: Java"
project: riak
version: 2.0.0+
document: tutorial
audience: advanced
keywords: [developers, security, ssl, java]
---

This tutorial shows you how to set up a Riak Java client to authenticate
itself when connecting to Riak.

If you are using [[trust-|Managed Security
Sources#Trust-based-Authentication]] or [[PAM|Managing Security
Sources#PAM-based-Authentication]]-based authentication, you can use the
security setup described [[below|Client-side Security:
Java#Java-Client-Basics]]. [[Certificate|Managing Security
Sources#Certificate-based-Authentication]] is not currently supported in
the Java client.

<div class="note">
<div class="title">Note on certificate generation</div>
This tutorial does not cover certificate generation. It assumes that all
necessary certificates have already been created and are stored in a
directory called `/ssl_dir`. This directory name is used only for
example purposes.
</div>

## Java Client Basics

When connecting to Riak using a Java-based client, you typically do so
by instantiating separate `RiakNode` objects for each node in your
cluster, a `RiakCluster` object registering those `RiakNode` objects,
and finally a `RiakClient` object that registers the general cluster
configuration.

If you are using Riak security, _all_ connecting clients should have
access to the same Certificate Authority (CA) used on the server side,
regardless of which [[security source|Managing Security Sources]] you
choose. All clients should also provide a username, regardless of
security source. The example below sets up a single node object (we'll
simply call it `node`) that connects to Riak on `localhost` and on port
8087 and specifies `riakuser` as a username. That object will be used to
create a cluster object (we'll call it `cluster`), which will in turn be
used to create a `client` object.

```java
import com.basho.riak.client.api.RiakClient;
import com.basho.riak.client.api.RiakCluster;
import com.basho.riak.client.api.RiakNode;

RiakNode node = new RiakNode.Builder()
        .withRemoteAddress("127.0.0.1")
        .withRemotePort(8087)
        .build();

RiakCluster cluster = new RiakCluster.Builder(node)
        .build();

RiakClient client = new RiakClient(cluster);
```

This client object is currently not set up to use any of the available
security sources, except trust-based auth, provided that the CIDR from
which the client is connecting has been specified as trusted. More on
this in [[Trust-based Authentication|Managing Security
Sources#Trust-based-Authentication]].

## Password-based Authentication

To enable our client to use password-based auth, we can use most of the
information from the examble above, with the exception that we will
specify a password for the client in the `withAuth` method in the `node`
object's constructor rather than leaving it as `null`.

```java
RiakNode node = new RiakNode.Builder()
        .withRemoteAddress("127.0.0.1")
        .withRemotePort(8087)
        .withAuth("riakuser", "rosebud", null)
        .build();

// Construct the cluster and client object proceed in the same fashion
// as above
```
