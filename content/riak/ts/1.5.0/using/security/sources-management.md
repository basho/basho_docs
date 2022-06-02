---
title_supertext: "Security"
title: "Sources Management"
description: "Managing sources and means of authentication in Riak TS."
menu:
  riak_ts-1.5.0:
    name: "Sources Management"
    identifier: "security_sources_management"
    weight: 130
    parent: "security"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/using/security/sources-management
---

[cidr]: http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing
[pbkdf2]: http://en.wikipedia.org/wiki/PBKDF2
[security users]: ../user-management
[security enabling]: ../enable-disable/#enabling-security
[security add user]: ../user-management/#add-user
[root cert]: http://en.wikipedia.org/wiki/Root_certificate
[rolling restart]: {{<baseurl>}}riak/kv/2.2.0/using/repair-recovery/rolling-restart/
[config ref security]: {{<baseurl>}}riak/kv/2.2.0/configuring/reference/#security
[xss]: http://en.wikipedia.org/wiki/Cross-site_scripting
[request forgery]: http://en.wikipedia.org/wiki/Cross-site_request_forgery
[http referer]: http://en.wikipedia.org/wiki/HTTP_referer

While [user management][security users] enables you to control user authorization, _security sources_ provide an interface for managing means of authentication. If you create users and grant them access to some or all of Riak TS's functionality as described in the [User Management][security users] section, you will need to define security sources required for authentication.

## Available Sources

There are four available authentication sources in Riak TS Security:

Source   | Description
:--------|:-----------
[trusted networks](#trust-based-authentication) (`trust`) | Always authenticate successfully if access has been granted to a user or all users on the specified [CIDR][cidr] range
[password](#password-based-authentication) (`password`) | Check the user's password against the [PBKFD2][pbkdf2]-hashed password stored in Riak TS
[pluggable authentication modules (PAM)](#pam-based-authentication) (`pam`)  | Authenticate against the given pluggable authentication module (PAM) service
[certificates](#certificate-based-authentication) (`certificate`) | Authenticate using a client certificate

## Add a Source

Riak TS security sources may be applied to a specific user, multiple users,
or all users (`all`).

In general, the `add-source` command takes the following form:

```bash
riak-admin security add-source all|<users> <CIDR> <source> [<option>=<value>[...]]
```

Using `all` indicates that the authentication source can be added to
all users. A source can be added to a specific user, for example `add-source superuser`, or to a list of users separated by commas, for example `add-source jane,bill,admin`.

The examples in the following sections will assume that the network in question is `127.0.0.1/32` and that a Riak TS user named `riakuser` has been [created][security add user] and that security has been [enabled][security enabling].

### Trust-based Authentication

This form of authentication (`trust`) enables you to specify trusted
[CIDRs][cidr] from which all clients will be authenticated by default.

Let's say that we want to give all users trusted access to securables
(without a password) when requests come from `localhost`:

```bash
riak-admin security add-source all 127.0.0.1/32 trust
```

At that point, the `riak-admin security print-sources` command would
print the following:

```
+--------------------+------------+----------+----------+
|       users        |    cidr    |  source  | options  |
+--------------------+------------+----------+----------+
|        all         |127.0.0.1/32|  trust   |    []    |
+--------------------+------------+----------+----------+
```

You can also specify users as trusted users, as in the following
example:

```bash
riak-admin security add-source riakuser 127.0.0.1/32 trust
```

Now, `riakuser` can interact with Riak TS without providing credentials.
Running the `riak-admin security print-sources` command would print:

```
+--------------------+------------+-----------+----------+
|       users        |    cidr    |  source   | options  |
+--------------------+------------+-----------+----------+
|      riakuser      |127.0.0.1/32|   trust   |    []    |
|        all         |127.0.0.1/32|   trust   |    []    |
+--------------------+------------+-----------+----------+
```

### Password-based Authentication

Authenticating with the `password` source requires that our `riakuser` be
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

Running the `riak-admin security print-sources` command would print:

```
+--------------------+------------+----------+----------+
|       users        |    cidr    |  source  | options  |
+--------------------+------------+----------+----------+
|      riakuser      |127.0.0.1/32| password |    []    |
+--------------------+------------+----------+----------+
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
```

Now, our `riakuser` must enter a username and password to have any
access to Riak TS. We can check this by using the `riak-admin security print-sources` command:

```
+--------------------+------------+----------+----------+
|       users        |    cidr    |  source  | options  |
+--------------------+------------+----------+----------+
|      riakuser      |127.0.0.1/32| password |    []    |
|      otheruser     |127.0.0.1/32| password |    []    |
|        all         |127.0.0.1/32| password |    []    |
+--------------------+------------+----------+----------+
```


### Certificate-based Authentication

This form of authentication (`certificate`) requires that Riak TS and a
specified client (or clients) interacting with Riak TS have certificates
signed by the same [Root Certificate Authority][root cert].

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
their `CN` and Riak TS username match.

On the server side, you need to configure Riak TS by specifying a path to
your certificates. First, copy all relevant files to your Riak TS cluster.
The default directory for certificates is `/etc`. You can specify
a different directory in your `riak.conf` by either uncommenting those lines (if you choose to use the defaults) or setting the paths yourself:

```riak.conf
ssl.certfile = /path/to/cert.pem
ssl.keyfile = /path/to/key.pem
ssl.cacertfile = /path/to/cacert.pem
```

In the client-side example above, the client's `CN` and Riak TS username
needed to match. On the server (i.e. Riak TS) side, the `CN` specified _on
each node_ must match the node's name as registered by Riak TS.

You can find the node's name in `riak.conf` under the parameter `nodename`. And so if the `nodename` for a cluster is `riakts-node-1`, you would need to generate your certificate with that in mind, as in this OpenSSL example:

```bash
openssl req -new ... '/CN=riakts-node-1'
```

Once certificates have been generated and configured on all the nodes in your Riak TS cluster, you need to perform a [rolling restart][rolling restart]. Once that process is complete, you can use the client certificate that you generated for the user `riakuser`.

How to use Riak TS clients in conjunction with OpenSSL and other
certificates varies from client library to client library. We strongly
recommend checking the documentation of your client library for further
information.

### PAM-based Authentication

This section assumes you have set up a pluggable authentication module (PAM) service bearing the name `riak_pam`, either by creating a `pam.d/riak_pam` service definition specifying `auth` or other PAM services set up to authenticate a user named `riakuser`.

As in the [certificate-based authentication](#certificate-based-authentication) example above, the user's name must be the same in both your authentication module and in Riak TS Security.

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

## Delete Source

If we wish to remove the `trust` source that we granted to `all` in the
example above, we can simply use the `del-source` command and specify
the CIDR.

```bash
riak-admin security del-source all 127.0.0.1/32
```

Note that this does not require that you specify which type of source is
being deleted. You only need to specify the user(s) or `all`, because
only one source can be applied to a user or `all` at any given time.

The following command would remove the source for `riakuser` on
`localhost`, regardless of which source is being used:

```bash
riak-admin security del-source riakuser 127.0.0.1/32
```

{{% note title="Note on Removing Sources" %}}
If you apply a security source both to `all` and to specific
users and then wish to remove that source, you will need to do so in
separate steps. The `riak-admin security del-source all ...`
command by itself is not sufficient.

For example, if you have assigned the source `password` to
both `all` and to the user `riakuser` on the
network `127.0.0.1/32`, the following two-step process would
be required to fully remove the source:

```bash
riak-admin security del-source all 127.0.0.1/32 password
riak-admin security del-source riakuser 127.0.0.1/32 password
```
{{% /note %}}

## Security Ciphers

To view a list of currently available security ciphers or change Riak's
preferences, use the `ciphers` command:

```bash
riak-admin security ciphers
```

That command by itself will return a large list of available ciphers:

```
Configured ciphers

ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256: ...

Valid ciphers(35)

ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256: ...

Unknown/Unsupported ciphers(32)

ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256: ...
```

To alter the list, i.e. to constrain it and/or to set preferred ciphers
higher in the list:

```bash
riak-admin security ciphers DHE-RSA-AES256-SHA:AES128-GCM-SHA256
```

The list of configured ciphers should now look like this:

```
Configured ciphers

DHE-RSA-AES256-SHA:AES128-GCM-SHA256

Valid ciphers(1)

DHE-RSA-AES256-SHA

Unknown/Unsupported ciphers(1)

AES128-GCM-SHA256
```

A list of available ciphers on a server can be obtained using the
`openssl` command:

```bash
openssl ciphers
```

That should return a list structured like this:

```
DHE-RSA-AES256-SHA:DHE-DSS-AES256-SHA:AES256-SHA:EDH-RSA-DES-CBC3-SHA: # and so on
```

Riak's cipher preferences were taken from [Mozilla's Server-Side TLS
documentation](https://wiki.mozilla.org/Security/Server_Side_TLS).

### Client vs. Server Cipher Order

By default, Riak TS prefers the cipher order setting, `honor_cipher_order`, to be set to `on`. If you prefer, however, that clients' preferred cipher
order dictate which cipher is chosen, set `honor_cipher_order` to `off`.

## Enabling SSL

SSL is disabled by default. In order to use any authentication or authorization features, you must enable SSL for Riak TS.

Enabling SSL on a given node requires you to specify a host and port for the node:

```riak.conf
listener.protobuf.$name = {"127.0.0.1",8087}
```

As well as specify a [certification configuration](#certificate-configuration).

## TLS Settings

When using Riak TS security, you can choose which versions of SSL/TLS are
allowed. By default, only TLS 1.2 is allowed, but this version can be
disabled and others enabled by setting the following [configurable parameters][config ref security] to `on` or `off`:

* `tls_protocols.tlsv1`
* `tls_protocols.tlsv1.1`
* `tls_protocols.tlsv1.2`
* `tls_protocols.sslv3`

Three things to note:

* Among the four available options, only TLS version 1.2 is enabled by
  default
* You can enable more than one protocol at a time
* We strongly recommend that you do NOT use SSL version 3 unless
  absolutely necessary

## Certificate Configuration

If you are using any of the available [security sources](#available-sources), including [trust-based authentication](#trust-based-authentication), you will need to do so
over a secure SSL connection.

In order to establish a secure connection, ensure that each Riak TS node's configuration files point to the proper paths for your generated certs. By default, Riak TS assumes that all certs are stored in each node's `/etc` directory.

If you are using the newer, `riak.conf`-based configuration system, you
can change the location of the `/etc` directory by modifying the
`platform_etc_dir`:

<table class="riak-conf">
  <thead>
    <tr>
      <th>Type</th>
      <th>Parameter</th>
      <th>Default</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><strong>Signing authority</strong></td>
      <td><code>ssl.cacertfile</code></td>
      <td><code>#(platform_etc_dir)/cacertfile.pem</code></td>
    </tr>
    <tr>
      <td><strong>Cert</strong></td>
      <td><code>ssl.certfile</code></td>
      <td><code>#(platform_etc_dir)/cert.pem</code></td>
    </tr>
    <tr>
      <td><strong>Key file</strong></td>
      <td><code>ssl.keyfile</code></td>
      <td><code>#(platform_etc_dir)/key.pem</code></td>
    </tr>
  </tbody>
</table>

## Referer Checks and Certificate Revocation Lists

In order to provide safeguards against
[cross-site-scripting][xss] (XSS) and [request-forgery][request forgery]
attacks, Riak TS performs [secure referer checks][http referer] by default. Those checks make it impossible to serve data directly from Riak TS. To disable those checks, set the `secure_referer_check` parameter to `off`.

If you are using [certificate-based authentication](#certificate-based-authentication), Riak TS will check the certificate revocation list (CRL) of connecting clients' certificate by default. To disable this behavior, set the `check_crl` parameter to
`off`.

## How Sources Are Applied

When managing security sources---any of the sources explained
above---you always have the option of applying a source to either a
single user, multiple users, or all users (`all`). If specific users and
`all` have no sources in common, this presents no difficulty. But what
happens if one source is applied to `all` and a different source is
applied to a specific user?

The more-specifically assigned source (i.e. the source applied to the user) will be considered a user's security source. We'll illustrate
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
