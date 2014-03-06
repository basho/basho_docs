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

This guide assumes that you have set up a [Root Certificate Authority](http://en.wikipedia.org/wiki/Root_certificate) and have created and signed certificates, perhaps using [OpenSSL](https://www.openssl.org/) or another tool.

<div class="note">
<div class="title">Note</div>
At this time, client certificates are not supported in Riak's HTTP interface, and can be used only through the [[protocol buffers interface|PBC API]].
</div>

Let's say that you want to specify that a user with the username `riakuser` is going to be authenticated using a certificate on a network with a [CIDR](http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing) of 199.199.9.9/24. This needs to be same name as the common name, aka `CN`, specified when you generated your certificate.

First, you need to create that user:

```bash
riak-admin security add-user riakuser
```

This registers the user in your Riak cluster, but doesn't specify how that user will be authenticated. To do that, we must add the `certificate` security source for `riakuser` as well as the network in which authentication will take place:

```bash
riak-admin security add-source riakuser 199.199.9.9/24 certificate
```

You can then verify that the source and network have been entered properly:

```bash
riak-admin security print-sources
```

**Note**: This command will print security sources for all users, not just `riakuser`.

You should see output like this:

```
+--------------------+--------------+-----------+----------+
|       users        |     cidr     |  source   | options  |
+--------------------+--------------+-----------+----------+
|      riakuser      |199.199.9.9/24|certificate|    []    |
+--------------------+--------------+-----------+----------+
```

Copy all relevant `.pem` files to your Riak cluster. The default directory is `/etc`, and specify where those files are located in your `[[riak.conf|Configuration Files]]` by either uncommenting those lines if you're using the defaults, or setting them yourself:

```riakconf
ssl.certfile = /path/to/cert.pem
ssl.keyfile = /path/to/key.pem
ssl.cacertfile = /path/to/cacert.pem
```

<div class="note">
<div class="title">Note</div>
You will want to generate a server certificate <em>for each node in your Riak cluster</em>, using a <tt>CN</tt> that matches each node's host name, e.g. <tt>riak-node-1</tt>, <tt>riak-node-2</tt>, etc.
</div>

Once certificates have been properly generated and configured on all of the nodes in your Riak cluster, you need to perform a [[rolling restart]]. Once that process is complete, you can use the client certificate that you generated for the user `riakuser`.

At that point, clients can use that certificate to authenticate themselves. This process varies from client to client, so make sure and check your Riak client's documentation.

## References

- [OpenSSL tutorial](http://pages.cs.wisc.edu/~zmiller/ca-howto/)