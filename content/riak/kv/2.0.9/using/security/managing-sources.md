---
title: "Managing Security Sources"
description: ""
project: "riak_kv"
project_version: "2.0.9"
menu:
  riak_kv-2.0.9:
    name: "Managing Security Sources"
    identifier: "security_manage_sources"
    weight: 101
    parent: "managing_security"
toc: true
aliases:
  - /riak/2.0.9/ops/running/security-sources
  - /riak/kv/2.0.9/ops/running/security-sources
---

If you're looking for more general information on Riak Security, it may
be best to start with our general guide to [authentication and authorization]({{<baseurl>}}riak/kv/2.0.9/using/security/basics).

This document provides more granular information on the four available
authentication sources in Riak Security: trusted networks, password,
pluggable authentication modules (PAM), and certificates. These sources
correspond to `trust`, `password`, `pam`, and `certificate`,
respectively, in the `riak-admin security` interface.

The examples below will assume that the network in question is
`127.0.0.1/32` and that a Riak user named `riakuser` has been
[created]({{<baseurl>}}riak/kv/2.0.9/using/security/basics/#user-management) and that
security has been [enabled]({{<baseurl>}}riak/kv/2.0.9/using/security/basics/#the-basics).

{{% note title="Note on SSL connections" %}}
If you use _any_ of the aforementioned security sources, even `trust`, you
will need to do so via a secure SSL connection.
{{% /note %}}

## Trust-based Authentication

This form of authentication enables you to specify trusted
[CIDRs](http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing)
from which all clients will be authenticated by default.

```bash
riak-admin security add-source all 127.0.0.1/32 trust
```

Here, we have specified that anyone connecting to Riak from the
designated CIDR (in this case `localhost`) will be successfully
authenticated:

```curl
curl https://localhost:8098/types/<type>/buckets/<bucket>/keys/<key>
```

If this request returns `not found` or a Riak object, then things have
been set up appropriately. You can specify any number of trusted
networks in the same fashion.

You can also specify users as trusted users, as in the following
example:

```bash
riak-admin security add-source riakuser 127.0.0.1/32 trust
```

Now, `riakuser` can interact with Riak without providing credentials.
Here's an example in which only the username is passed to Riak:

```curl
curl -u riakuser: \
  https://localhost:8098/types/<type>/buckets/<bucket>/keys/<key>
```

## Password-based Authentication

Authenticating via the `password` source requires that our `riakuser` be
given a password. `riakuser` can be assigned a password upon creation,
as in this example:

```bash
riak-admin security add-user riakuser password=captheorem4life
```

Or a password can be assigned to an already existing user by modifying
that user's characteristics:

```bash
riak-admin security alter-user riakuser password=captheorem4life
```

You can specify that _all_ users must authenticate themselves via
password when connecting to Riak from `localhost`:

```bash
riak-admin security add-source all 127.0.0.1/32 password
```

Or you can specify that any number of specific users must do so:

```bash
riak-admin security add-source riakuser 127.0.0.1/32 password
riak-admin security add-source otheruser 127.0.0.1/32 password

# etc
```

Now, our `riakuser` must enter a username and password to have any
access to Riak whatsoever:

```curl
curl -u riakuser:captheorem4life \
  https://localhost:8098/types/<type>/buckets/<bucket>/keys/<key>
```

## Certificate-based Authentication

This form of authentication (`certificate`) requires that Riak and a
specified client---or clients---interacting with Riak bear certificates
signed by the same [Root Certificate
Authority](http://en.wikipedia.org/wiki/Root_certificate).

> **Note**
>
> At this time, client certificates are not supported in Riak's HTTP
interface, and can be used only through the [protocol buffers interface]({{<baseurl>}}riak/kv/2.0.9/developing/api/protocol-buffers/).

Let's specify that our user `riakuser` is going to be authenticated
using a certificate on `localhost`:

```bash
riak-admin security add-source riakuser 127.0.0.1/32 certificate
```

When the `certificate` source is used, `riakuser` must also be entered
as the common name, aka `CN`, that you specified when you generated your
certificate, as in the following OpenSSL example:

```bash
openssl req -new ... '/CN=riakuser'
```

You can add a `certificate` source to any number of clients, as long as
their `CN` and Riak username match.

On the server side, you need to configure Riak by specifying a path to
your certificates. First, copy all relevant files to your Riak cluster.
The default directory for certificates is `/etc`, though you can specify
a different directory in your [`riak.conf`]({{<baseurl>}}riak/kv/2.0.9/configuring/reference/) by either uncommenting those lines if you choose to use the defaults or setting the paths yourself:

```riakconf
ssl.certfile = /path/to/cert.pem
ssl.keyfile = /path/to/key.pem
ssl.cacertfile = /path/to/cacert.pem
```

In the client-side example above, the client's `CN` and Riak username
needed to match. On the server (i.e. Riak) side, the `CN` specified _on
each node_ must match the node's name as registered by Riak. You can
find the node's name in [`riak.conf`]({{<baseurl>}}riak/kv/2.0.9/configuring/reference/) under the parameter `nodename`. And so if the `nodename` for a cluster is
`riak-node-1`, you would need to generate your certificate with that in
mind, as in this OpenSSL example:

```bash
openssl req -new ... '/CN=riak-node-1'
```

Once certificates have been properly generated and configured on all of
the nodes in your Riak cluster, you need to perform a [rolling restart]({{<baseurl>}}riak/kv/2.0.9/using/repair-recovery/rolling-restart/). Once that process is complete, you can use the client
certificate that you generated for the user `riakuser`.

How to use Riak clients in conjunction with OpenSSL and other
certificates varies from client library to client library. We strongly
recommend checking the documentation of your client library for further
information.

## PAM-based Authentication

This section assumes that you have set up a PAM service bearing the name
`riak_pam`, e.g. by creating a `pam.d/riak_pam` service definition
specifying `auth` and/or other PAM services set up to authenticate a
user named `riakuser`. As in the certificate-based authentication
example above, the user's name must be the same in both your
authentication module and in Riak Security.

If we want the user `riakuser` to use this PAM service on `localhost`,
we need to add a `pam` security source in Riak and specify the name of
the service:

```bash
riak-admin security add-source all 127.0.0.1/32 pam service=riak_pam
```

**Note**: If you do not specify a name for your PAM service, Riak will
use the default, which is `riak`.

To verify that the source has been properly specified:

```bash
riak-admin security print-sources
```

That command should output the following:

```
+--------------------+------------+----------+------------------------+
|       users        |    cidr    |  source  |        options         |
+--------------------+------------+----------+------------------------+
|      riakuser      |127.0.0.1/32|   pam    |[{"service","riak_pam"}]|
+--------------------+------------+----------+------------------------+
```

You can test that setup most easily by using `curl`. A normal request to
Riak without specifying a user will return an `Unauthorized` message:

```curl
curl -u riakuser: \
  https://localhost:8098/types/<type>/buckets/<bucket>/keys/<key>
```

Response:

```
<html><head><title>401 Unauthorized</title></head><body><h1>Unauthorized</h1>Unauthorized<p><hr><address>mochiweb+webmachine web server</address></body></html>
```

If you identify yourself as `riakuser` and are successfully
authenticated by your PAM service, you should get either `not found` or
a Riak object if one is stored in the specified bucket type/bucket/key
path:

```curl
curl -u riakuser:<pam_password> \
  https://localhost:8098/types/<type>/buckets/<bucket>/keys/<key>
```

## How Sources Are Applied

When managing security sources---any of the sources explained
above---you always have the option of applying a source to either a
single user, multiple users, or all users (`all`). If specific users and
`all` have no sources in common, this presents no difficulty. But what
happens if one source is applied to `all` and a different source is
applied to a specific user?

The short answer is that the more specifically assigned source---i.e. to
the user---will be consider a user's security source. We'll illustrate
that with the following example, in which the `certificate` source is
assigned to `all`, but the `password` source is assigned to `riakuser`:

```bash
riak-admin security add-source all 127.0.0.1/32 certificate
riak-admin security add-source riakuser 127.0.0.1/32 password
```

If we run `riak-admin security print-sources`, we'll get the following
output:

```
+--------------------+------------+-----------+----------+
|       users        |    cidr    |  source   | options  |
+--------------------+------------+-----------+----------+
|      riakuser      |127.0.0.1/32| password  |    []    |
|                    |127.0.0.1/32|certificate|    []    |
|        all         |127.0.0.1/32|certificate|    []    |
+--------------------+------------+-----------+----------+
```

As we can see, `password` is set as the security source for `riakuser`,
whereas everyone else will authenticate using `certificate`.
