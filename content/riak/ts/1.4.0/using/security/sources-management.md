---
title_supertext: "Security"
title: "Sources Management"
description: "Managing sources and means of authentication in Riak TS."
menu:
  riak_ts-1.4.0:
    name: "Sources Management"
    identifier: "security_sources_management"
    weight: 130
    parent: "security"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/security/sources-management
canonical_link: "https://docs.basho.com/riak/ts/latest/using/security/sources-management/"
---

While [user management](../user-management) enables you to control _authorization_ with regard to users, security **sources** provide you with an interface for managing means of _authentication_. If you create users and grant them access to some or all of Riak TS's functionality as described in the [User Management](../user-management) section, you will then need to define security sources required for authentication.

An more in-depth tutorial can be found in [Managing Security Sources](/riak/kv/2.1.4/using/security/managing-sources/).

## Add Source

Riak TS security sources may be applied to a specific user, multiple users,
or all users (`all`).

### Available Sources

Source   | Description
:--------|:-----------
`trust` | Always authenticates successfully if access has been granted to a user or all users on the specified CIDR range
`password` | Check the user's password against the [PBKFD2](http://en.wikipedia.org/wiki/PBKDF2)-hashed password stored in Riak
`pam`  | Authenticate against the given pluggable authentication module (PAM) service
`certificate` | Authenticate using a client certificate

### Example: Adding a Trusted Source

Security sources can be added either to a specific user, multiple users,
or all users (`all`).

In general, the `add-source` command takes the following form:

```bash
riak-admin security add-source all|<users> <CIDR> <source> [<option>=<value>[...]]
```

Using `all` indicates that the authentication source can be added to
all users. A source can be added to a specific user, e.g. `add-source
superuser`, or to a list of users separated by commas, e.g. `add-source
jane,bill,admin`.

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

### More Usage Examples

This section provides only a very brief overview of the syntax for
working with sources. For more information on using the `trust`,
`password`, `pam`, and `certificate` sources, please see our [Managing Security Sources](/riak/kv/2.1.4/using/security/managing-sources/) document.

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

By default, Riak prefers the cipher order that you set on the server,
i.e. the [`honor_cipher_order`](/riak/kv/2.1.4/configuring/reference/#security) setting is set to `on`. If you prefer, however, that clients' preferred cipher
order dictate which cipher is chosen, set `honor_cipher_order` to `off`.

> **Note on Erlang versions**
>
> Riak's default cipher order behavior has the potential to crash Erlang
VMs that do not support it. Erlang VMs that are known to support it
include Basho's patched version of Erlang R16. Instructions on
installing it can be found in [Installing Erlang](/riak/kv/2.1.4/setup/installing/source/erlang). This issue should
not affect Erlang 17.0 and later.

## Enabling SSL

In order to use any authentication or authorization features, you must
enable SSL for Riak. **SSL is disabled by default**, but you will need
to enable it prior to enabling security. If you are using [Protocol Buffers](/riak/kv/2.1.4/developing/api/protocol-buffers/) as a transport protocol for Riak (which we strongly recommend), enabling SSL on a given node requires only that you specify a [host and port](/riak/kv/2.1.4/configuring/reference/#client-interfaces) for the node
as well as a [certification configuration](#certificate-configuration).

If, however, you are using the [HTTP API](/riak/kv/2.1.4/developing/api/http) for Riak and would like to
configure HTTPS, you will need to not only establish a [certificate configuration](#certificate-configuration) but also specify an HTTPS host
and port. The following configuration would establish port 8088 on
`localhost` as the HTTPS port:

```riakconf
listener.https.$name = 127.0.0.1:8088

# By default, "internal" is used as the "name" setting
```

```appconfig
{riak_core, [
             %% Other configs
             {https, [{"127.0.0.1", 8088}]},
             %% Other configs
            ]}
```

## TLS Settings

When using Riak security, you can choose which versions of SSL/TLS are
allowed. By default, only TLS 1.2 is allowed, but this version can be
disabled and others enabled by setting the following [configurable parameters](/riak/kv/2.1.4/configuring/reference/#security) to `on` or `off`:

* `tls_protocols.tlsv1`
* `tls_protocols.tlsv1.1`
* `tls_protocols.tlsv1.2`
* `tls_protocols.sslv3`

Three things to note:

* Among the four available options, only TLS version 1.2 is enabled by
  default
* You can enable more than one protocol at a time
* We strongly recommend that you do _not_ use SSL version 3 unless
  absolutely necessary

## Certificate Configuration

If you are using any of the available [security sources](/riak/kv/2.1.4/using/security/managing-sources/), including [trust-based authentication](/riak/kv/2.1.4/using/security/managing-sources/#trust-based-authentication), you will need to do so
over a secure SSL connection. In order to establish a secure connection,
you will need to ensure that each Riak node's [configuration files](/riak/kv/2.1.4/configuring/reference/#security) point to the proper paths for your
generated certs. By default, Riak assumes that all certs are stored in
each node's `/etc` directory.

If you are using the newer, `riak.conf`-based configuration system, you
can change the location of the `/etc` directory by modifying the
`platform_etc_dir`. More information can be found in our documentation
on [configuring directories](/riak/kv/2.1.4/configuring/reference/#directories).

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

If you are using the older, `app.config`-based configuration system,
these paths can be set in the `ssl` subsection of the `riak_core`
section. The corresponding parameters are shown in the example below:

```appconfig
{riak_core, [
    %% Other configs

    {ssl, [
           {certfile, "./etc/cert.pem"},
           {keyfile, "./etc/key.pem"},
           {cacertfile, "./etc/cacertfile.pem"}
          ]},

    %% Other configs
]}
```

## Referer Checks and Certificate Revocation Lists

In order to provide safeguards against
[cross-site-scripting](http://en.wikipedia.org/wiki/Cross-site_scripting)
(XSS) and
[request-forgery](http://en.wikipedia.org/wiki/Cross-site_request_forgery)
attacks, Riak TS performs [secure referer checks](http://en.wikipedia.org/wiki/HTTP_referer) by default. Those checks make it impossible to serve data directly from Riak. To disable those checks, set the `secure_referer_check` parameter to `off`.

If you are using [certificate-based authentication](/riak/kv/2.1.4/using/security/managing-sources/#certificate-based-authentication), Riak TS will check the certificate revocation list (CRL) of connecting clients' certificate by default. To disable this behavior, set the `check_crl` parameter to
`off`.
