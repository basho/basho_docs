---
title: Managing Security Sources
project: riak
version: 2.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, security]
---

If you're looking for more general information on Riak Security, it may be best to start with our general guide to [[authentication and authorization]].

This document provides more granular information on the four available authentication sources in Riak Security: trusted networks, password, pluggable authentication modules (PAM), and certificates. These sources correspond to `trust`, `password`, `pam`, and `certificate`, respectively, in the `riak-admin security` interface.

The examples below will assume that the network in question is `127.0.0.1/32` and that a Riak user named `riakuser` has been [[created|Authentication and Authorization#User-Management]] and that security has been [[enabled|Authentication and Authorization#The-Basics]].

<div class="note">
<div class="title">Note</div>
If you use <em>any</em> of the aforementioned security sources, you will need to do so via a secure connection, even <tt>trust</tt>.
</div>

## Trust-based Authentication

This form of authentication enables you to specify trusted [CIDRs](http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing) from which all clients will be authenticated by default.

```bash
riak-admin security add-source all 127.0.0.1/32 trust
```

Here, we have specified that anyone connecting to Riak from the designated CIDR (in this case `localhost`) will be successfully authenticated:

```curl
curl https://localhost:8098/types/<type>/buckets/<bucket>/keys/<key>
```

If this request returns `not found` or a Riak object, then things have been set up appropriately. You can specify any number of trusted networks in the same fashion.

You can also specify users as trusted users, as in the following example:

```bash
riak-admin security add-source riakuser 127.0.0.1/32 trust
```

Now, `riakuser` can interact with Riak without providing credentials. Here's an example in which only the username is passed to Riak:

```curl
curl -u riakuser: https://localhost:8098/types/<type>/buckets/<bucket>/keys/<key>
```

## Password-based Authentication

Authenticating via the `password` source requires that our `riakuser` be given a password. `riakuser` can be assigned a password upon creation, as in this example:

```bash
riak-admin security add-user riakuser password=captheorem4life
```

Or a password can be assigned to an already existing user by modifying that user's characteristics:

```bash
riak-admin security alter-user riakuser password=captheorem4life
```

You can specify that _all_ users must authenticate themselves via password:

```bash
riak-admin security add-source all 127.0.0.1/32 password
```

Or you can 




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

TODO

Note about enabling security

## PAM-based Authentication

PAM service definition file
`/usr/local/etc/pam.d/riak`
This sets up a `riak` PAM service

Example file:
```
auth        required    /path/to/pampwd_file.dat
account     required    pam_permit.so
session     required    pam_permit.so
password    required    pam_deny.so
```

Only `auth` is required

```bash
riak-admin security add-source all 199.199.9.9/32 pam service=riak
```

```bash
riak-admin security print-sources
```

```
+--------------------+----------------+----------+--------------------+
|       users        |      cidr      |  source  |      options       |
+--------------------+----------------+----------+--------------------+
|                    | 199.199.9.9/32 |   pam    |[{"service","riak"}]|
|        all         | 199.199.9.9/32 |   pam    |[{"service","riak"}]|
+--------------------+----------------+----------+--------------------+
```

This shows that the PAM service named `riak` is now recognized by Riak

```bash
riak-admin security print-users
```

```
+------------+-------+----------------+---------------+
|  username  | roles |    password    |    options    |
+------------+-------+----------------+---------------+
|  riakuser  |       |                |      []       |
+------------+-------+----------------+---------------+
```

You can test that setup most easily using `curl`:

```bash
curl http://localhost:8098/types/<type>/buckets/<bucket>/keys/<key>
```

The response will indicate that this action is not authorized:

```
<html><head><title>401 Unauthorized</title></head><body><h1>Unauthorized</h1>Unauthorized<p><hr><address>mochiweb+webmachine web server</address></body></html>
```

But if you pass in the appropriate username and password information, the operation will succeed:

```curl
curl -u 'riakuser:rosebud' \
  curl http://localhost:8098/types/<type>/buckets/<bucket>/keys/<key>
```

Assuming that no object is stored in the bucket type/bucket/key location specified, this read should return `not found`, which shows that 

## References

- [OpenSSL tutorial](http://pages.cs.wisc.edu/~zmiller/ca-howto/)