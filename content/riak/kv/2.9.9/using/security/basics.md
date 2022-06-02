---
title: "Security Basics"
description: ""
project: "riak_kv"
project_version: 2.9.9
menu:
  riak_kv-2.9.9:
    name: "Security Basics"
    identifier: "security_basics"
    weight: 100
    parent: "managing_security"
toc: true
aliases:
  - /riak/2.9.9/ops/running/authz
  - /riak/kv/2.9.9/ops/running/authz
---

> **Note on Network security**
>
> This document covers only the 2.0 authentication and authorization
features. For a look at network security in Riak, see [Security and Firewalls]({{<baseurl>}}riak/kv/2.9.9/using/security/managing-sources/).

As of version 2.0, Riak administrators can selectively apportion
access to a wide variety of Riak's functionality, including accessing,
modifying, and deleting objects, changing bucket properties, and
running MapReduce jobs.

## Terminology

* **Authentication** is the process of identifying a user.
* **Authorization** is verifying whether a user has access to perform
  the requested operation.
* **Groups** can have permissions assigned to them, but cannot be
  authenticated.
* **Users** can be authenticated and authorized; permissions
  (authorization) may be granted directly or via group membership.
* **Sources** are used to define authentication mechanisms. A user
  cannot be authenticated to Riak until a source is defined.

## Security Checklist

There are a few key steps that all applications will need to undertake
when turning on Riak security. Missing one of these steps will almost
certainly break your application, so make sure that you have done each
of the following **before** enabling security:

1. Make certain that the original Riak Search (version 1) and link
   walking are not required. Enabling security will break this
   functionality. If you wish to use security and Search together, you
   will need to use the [new Search feature]({{<baseurl>}}riak/kv/2.9.9/developing/usage/search/).
1. Because Riak security requires a secure SSL connection, you will need
   to generate appropriate SSL certs, [enable SSL](#enabling-ssl) and establish a [certificate configuration](#certificate-configuration) on each node. **If you
   enable security without having established a functioning SSL
   connection, all requests to Riak will fail**.
1. Define [users](#user-management)
   and, optionally, [groups](#add-group)
1. Define an [authentication source](#managing-sources) for each user
1. Grant the necessary [permissions](#managing-permissions) to each user (and/or group)
1. Check any Erlang MapReduce code for invocations of Riak modules other
   than `riak_kv_mapreduce`. Enabling security will prevent those from
   succeeding unless those modules are available via the `add_path`
   mechanism documented in [Installing Custom Code]({{<baseurl>}}riak/kv/2.9.9/using/reference/custom-code).
1. Make sure that your client software will work properly:
    * It must pass authentication information with each request
    * It must support HTTPS or encrypted [Protocol Buffers]({{<baseurl>}}riak/kv/2.9.9/developing/api/protocol-buffers/)
      traffic
    * If using HTTPS, the proper port (presumably 443) is open from
      client to server
    * Code that uses Riak's deprecated link walking feature **will
      not work** with security enabled
1. If you have applications that rely on an already existing Riak
   cluster, make sure that those applications are prepared to gracefully
   transition into using Riak security once security is enabled.

Security should be enabled only after all of the above steps have been
performed and your security setup has been properly vetted.

Clients that use [Protocol Buffers]({{<baseurl>}}riak/kv/2.9.9/developing/api/protocol-buffers/) will typically have to be
reconfigured/restarted with the proper credentials once security is
enabled.

## Security Basics

Riak security may be checked, enabled, or disabled by an administrator
through the command line. This allows an administrator to change
security settings for the whole cluster quickly without needing to
change settings on a node-by-node basis.

**Note**: Currently, Riak security commands can be run only through
the command line, using the `riak-admin security` command. In future
versions of Riak, administrators may have the option of issuing
those commands through the Protocol Buffers and HTTP interfaces.

### Enabling Security

> **Warning: Enable security with caution**
>
> Enabling security will change the way your client libraries and
your applications interact with Riak.
>
> Once security is enabled, all client connections must be encrypted and all permissions will be denied by default. Do not enable this in production until you have worked through the [security checklist](#security-checklist) above and tested everything in a non-production environment.

Riak security is disabled by default. To enable it:

```bash
riak-admin security enable
```

**As per the warning above, do not enable security in production without
taking the appropriate precautions.**

All users, groups, authentication sources, and permissions can be
configured while security is disabled, allowing you to create a
security configuration of any level of complexity without prematurely
impacting the service. This should be borne in mind when you are
[managing users](#user-management) and [managing sources](#managing-sources).

### Disabling Security

If you disable security, this means that you have disabled all of the
various permissions checks that take place when executing operations
against Riak. Users, groups, and other security attributes remain
available for configuration while security is disabled, and will be
applied if and when security is re-enabled.

```bash
riak-admin security disable
```

While security is disabled, clients will need to be reconfigured to no
longer require TLS and send credentials.

### Checking Security Status

To check whether security is currently enabled for the cluster, use the
`status` command:

```bash
riak-admin security status
```

This command will usually return `Enabled` or `Disabled`, but if
security is enabled on a mixed-mode cluster (running a combination of
Riak 2.0 and older versions) it will indicate that security is enabled
but not yet available.

## User Management

Riak security enables you to control _authorization_ by creating,
modifying, and deleting user characteristics and granting users
selective access to Riak functionality (and also to revoke access).
Users can be assigned one or more of the following characteristics:

* `username`
* `groups`
* `password`

You may also assign users characteristics beyond those listed
above---e.g., listing email addresses or other information---but those
values will carry no special significance for Riak.

**Note**: The `username` is the one user characteristic that cannot be
changed once a user has been created.

### Retrieve a Current User or Group List

A list of currently existing users can be accessed at any time:

```bash
riak-admin security print-users
```

The same goes for groups:

```bash
riak-admin security print-groups
```

Example output, assuming user named `riakuser` with an assigned
password:

```
+----------+--------+----------------------+------------------------------+
| username | groups |       password       |           options            |
+----------+--------+----------------------+------------------------------+
| riakuser |        |983e8ae1421574b8733824|              []              |
+----------+--------+----------------------+------------------------------+
```

**Note**: All passwords are displayed in encrypted form in console
output.

If the user `riakuser` were assigned to the group `dev` and a `name` of
`lucius`, the output would look like this:

```bash
+----------+----------------+----------------------+---------------------+
| username |     groups     |       password       |       options       |
+----------+----------------+----------------------+---------------------+
| riakuser |      dev       |983e8ae1421574b8733824| [{"name","lucius"}] |
+----------+----------------+----------------------+---------------------+
```

If you'd like to see which permissions have been assigned to
`riakuser`, you would need to use the `print-grants` command, detailed
below.

The `security print-user` or `security-print-group` (singular) commands
can be used with a name as argument to see the same information as
above, except for only that user or group.

### Permissions Grants For a Single User or Group

You can retrieve authorization information about a specific user or
group using the `print-grants` command, which takes the form of
`riak-admin security print-grants <username>`.

The output will look like this if the user `riakuser` has been
explicitly granted a `riak_kv.get` permission on the bucket
`shopping_list` and inherits a set of permissions from the `admin`
group:

```bash
Inherited permissions (user/riakuser)

+--------+----------+----------+----------------------------------------+
| group  |   type   |  bucket  |                 grants                 |
+--------+----------+----------+----------------------------------------+
| admin  |    *     |    *     |      riak_kv.get, riak_kv.delete,      |
|        |          |          |              riak_kv.put               |
+--------+----------+----------+----------------------------------------+

Dedicated permissions (user/riakuser)

+----------+-------------+----------------------------------------+
|   type   |   bucket    |                 grants                 |
+----------+-------------+----------------------------------------+
|   ANY    |shopping_list|               riak_kv.get              |
+----------+-------------+----------------------------------------+

Cumulative permissions (user/riakuser)

+----------+-------------+----------------------------------------+
|   type   |   bucket    |                 grants                 |
+----------+-------------+----------------------------------------+
|    *     |      *      |      riak_kv.get, riak_kv.delete,      |
|          |             |               riak_kv.put              |
|   ANY    |shopping_list|               riak_kv.get              |
+----------+-------------+----------------------------------------+
```

**Note**: The term `admin` is not a reserved term in Riak security. It
is used here only for illustrative purposes.

Because the same name can represent both a user and a group, a prefix
(`user/` or `group/`) can be used before the name (e.g., `print-grants
user/admin`). If a name collides and no prefix is supplied, grants for
both will be listed separately.

### Add Group

For easier management of permissions across several users, it is
possible to create groups to be assigned to those users.

```bash
riak-admin security add-group admin
```

### Add User

To create a user with the username `riakuser`, we use the `add-user`
command:

```bash
riak-admin security add-user riakuser
```

Using the command this way will create the user `riakuser` without _any_
characteristics beyond a username, which is the only attribute that you
must assign upon user creation.

Alternatively, a password---or other attributes---can be assigned to the
user upon creation. Here, we'll assign a password:

```bash
riak-admin security add-user riakuser password=Test1234
```

### Assigning a Password and Altering Existing User Characteristics

While passwords and other characteristics can be set upon user creation,
it often makes sense to change user characteristics after the user has
already been created. Let's say that the user `riakuser` was created
without a password (or created _with_ a password that we'd like to
change). The `alter-user` command can be used to modify our `riakuser`
user:

```bash
riak-admin security alter-user riakuser password=opensesame
```

When creating or altering a user, any number of `<option>=<value>`
pairs can be appended to the end of the command. Any non-standard
options will be stored and displayed via the `riak-admin security
print-users` command.

```bash
riak-admin security alter-user riakuser name=bill age=47 fav_color=red
```

Now, the `print-users` command should return this:

```
+----------+--------+----------+--------------------------------------------------+
| username | groups | password |                     options                      |
+----------+--------+----------+--------------------------------------------------+
| riakuser |        |          |[{"fav_color","red"},{"age","47"},{"name","bill"}]|
+----------+--------+----------+--------------------------------------------------+
```

**Note**: Usernames _cannot_ be changed using the `alter-user` command.
For example, running `riak-admin security alter-user riakuser
username=other-name`, will instead add the
`{"username","other-name"}` tuple to `riakuser`'s options.

### Managing Groups for a User

If we have a user `riakuser` and we'd like to assign her to the
`admin` group, we assign the value `admin` to the option `groups`:

```bash
riak-admin security alter-user riakuser groups=admin
```

If we'd like to make the user `riakuser` both an `admin` and an
`archoverlord`:

```bash
riak-admin security alter-user riakuser groups=admin,archoverlord
```

There is no way to incrementally add groups; even if `riakuser` was
already an `admin`, it is necessary to list it again when adding the
`archoverlord` group. Thus, to remove a group from a user, use
`alter-user` and list all *other* groups.

If the user should be removed from all groups, use `groups=` with no
list:

```bash
riak-admin security alter-user riakuser groups=
```

### Managing Groups for Groups

Groups can be added to other groups for cascading permissions.

```bash
riak-admin security alter-group admin groups=dev
```

### Deleting a User or Group

If you'd like to remove a user, use the `del-user` command:

```
riak-admin security del-user riakuser
```

For groups, use the `del-group` command:

```
riak-admin security del-group admin
```

### Adding or Deleting Multiple Users

The `riak-admin security` command does not currently allow you to
add or delete multiple users using a single command. Instead, they must
be added or deleted one by one.

## Managing Permissions

Permission to perform a wide variety of operations against Riak can be
granted to---or revoked from---users via the `grant` and `revoke`
commands.

### Basic Form

The `grant` command takes one of the following forms:

```bash
riak-admin security grant <permissions> on any to all|{<user>|<group>[,...]}
riak-admin security grant <permissions> on <bucket-type> to all|{<user>|<group>[,...]}
riak-admin security grant <permissions> on <bucket-type> <bucket> to all|{<user>|<group>[,...]}
```

The `revoke` command is essentially the same, except that `to` is
replaced with `from` of `to`:

```bash
riak-admin security revoke <permissions> on any from all|{<user>|<group>[,...]}
riak-admin security revoke <permissions> on <bucket-type> from all|{<user>|<group>[,...]}
riak-admin security revoke <permissions> on <bucket-type> <bucket> from all|{<user>|<group>[,...]}
```

If you select `any`, this means that the permission (or set of
permissions) is granted/revoked for all buckets and [bucket types]({{<baseurl>}}riak/kv/2.9.9/developing/usage/bucket-types). If you specify a bucket type only, then the permission
is granted/revoked for all buckets of that type. If you specify a bucket
type _and_ a bucket, the permission is granted/revoked only for that
bucket type/bucket combination.

**Note**: You cannot grant/revoke permissions with respect to a bucket
alone. You must specify either a bucket type by itself or a bucket type
and bucket. This limitation reflects the naming structure underlying
buckets and bucket types.

Selecting `all` grants or revokes a permission (or set of permissions)
for all users in all groups. When specifying the user(s)/group(s) to
which you want to apply a permission (or set of permissions), you may
list any number of users or groups comma-separated with no whitespace.
Here is an example of granting multiple permissions across all buckets
and bucket types to multiple users:

```bash
riak-admin security grant riak_kv.get,riak_search.query on any to jane,ahmed
```

If the same name is used for both a user and a group, the `grant`
command will ask for the name to be prefixed with `user/` or `group/`
to disambiguate.

### Key/Value Permissions

Permissions that can be granted for basic key/value access
functionality:

Permission | Operation |
:----------|:----------|
`riak_kv.get` | Retrieve objects
`riak_kv.put` | Create or update objects
`riak_kv.delete` | Delete objects
`riak_kv.index` | Index objects using secondary indexes (2i)
`riak_kv.list_keys` | List all of the keys in a bucket
`riak_kv.list_buckets` | List all buckets

{{% note title="Note on Listing Keys and Buckets" %}}
`riak_kv.list_keys` and `riak_kv.list_buckets` are both very expensive
operations that should be performed very rarely and never in production.
Access to this functionality should be granted very carefully.
{{% /note %}}

If you'd like to create, for example, a `client` account that is
allowed only to run `GET` and `PUT` requests on all buckets:

```bash
riak-admin security add-user client
riak-admin security grant riak_kv.get,riak_kv.put on any to client
```

### MapReduce Permissions

Permission to perform [MapReduce]({{<baseurl>}}riak/kv/2.9.9/developing/usage/mapreduce/) jobs can be assigned
using `riak_kv.mapreduce`. The following example grants MapReduce
permissions to the user `mapreduce-power-user` for all buckets and
bucket types:

```bash
riak-admin security grant riak_kv.mapreduce on any to mapreduce-power-user
```

### Bucket Type Permissions

In versions 2.0 and later, Riak users can manage [bucket types]({{<baseurl>}}riak/kv/2.9.9/developing/usage/bucket-types) in addition to setting bucket properties. `riak-admin
security` allows you to manage the following bucket type-related
permissions:

Permission | Operation
:----------|:---------
`riak_core.get_bucket` | Retrieve the `props` associated with a bucket
`riak_core.set_bucket` | Modify the `props` associated with a bucket
`riak_core.get_bucket_type` | Retrieve the set of `props` associated with a bucket type
`riak_core.set_bucket_type` | Modify the set of `props` associated with a bucket type

### Search Query Permission (Riak Search version 1)

Security is incompatible with the original (and now deprecated) Riak
Search. Riak Search version 1 will stop working if security is enabled.

### Search Query Permissions (Riak Search version 2, aka Yokozuna)

If you are using the new Riak Search, i.e. the Solr-compatible search
capabilities included with Riak versions 2.0 and greater, the following
search-related permissions can be granted/revoked:

Permission | Operation
:----------|:---------
`search.admin` | The ability to perform search admin-related tasks, such as creating and deleting indexes and adding and modifying search schemas
`search.query` | The ability to query an index

> **Note on Search Permissions**
>
> Search must be enabled in order to successfully grant/revoke Search
permissions. If you attempt to grant/revoke permissions while Search is
disabled, you will get the following error:
>
> `{error,{unknown_permission,"search.query"}}`
>
> More information on Riak Search and how to enable it can be found in the
[Riak Search Settings]({{<baseurl>}}riak/kv/2.9.9/configuring/search/) document.

#### Usage Examples

To grant the user `riakuser` the ability to query all indexes:

```bash
riak-admin security grant search.query on index to riakuser

# To revoke:
# riak-admin security revoke search.query on index from riakuser
```

To grant the user `riakuser` the ability to query all schemas:

```bash
riak-admin security grant search.query on schema to riakuser

# To revoke:
# riak-admin security revoke search.query on schema from riakuser
```

To grant the user `riakuser` admin privileges only on the index
`riakusers_index`:

```bash
riak-admin security grant search.admin on index riakusers_index to riakuser

# To revoke:
# riak-admin security revoke search.admin on index riakusers_index from riakuser
```

To grant `riakuser` querying and admin permissions on the index
`riakusers_index`:

```bash
riak-admin security grant search.query,search.admin on index riakusers_index to riakuser

# To revoke:
# riak-admin security revoke search.query,search.admin on index riakusers_index from riakuser
```

## Managing Sources

While user management enables you to control _authorization_ with regard
to users, security **sources** provide you with an interface for
managing means of _authentication_. If you create users and grant them
access to some or all of Riak's functionality as described in the [User Management](#user-management) section,
you will then need to define security sources required for
authentication.

An more in-depth tutorial can be found in [Managing Security Sources]({{<baseurl>}}riak/kv/2.9.9/using/security/managing-sources/).

### Add Source

Riak security sources may be applied to a specific user, multiple users,
or all users (`all`).

#### Available Sources

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

### Deleting Sources

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
If you apply a security source both to `all` and to specific users and then
wish to remove that source, you will need to do so in separate steps. The
`riak-admin security del-source all ...` command by itself is not sufficient.

For example, if you have assigned the source `password` to both `all` and to
the user `riakuser` on the network `127.0.0.1/32`, the following two-step
process would be required to fully remove the source:

```bash
riak-admin security del-source all 127.0.0.1/32 password
riak-admin security del-source riakuser 127.0.0.1/32 password
```
{{% /note %}}

### More Usage Examples

This section provides only a very brief overview of the syntax for
working with sources. For more information on using the `trust`,
`password`, `pam`, and `certificate` sources, please see our [Managing Security Sources]({{<baseurl>}}riak/kv/2.9.9/using/security/managing-sources/) document.

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
i.e. the [`honor_cipher_order`]({{<baseurl>}}riak/kv/2.9.9/configuring/reference/#security) setting is set to `on`. If you prefer, however, that clients' preferred cipher
order dictate which cipher is chosen, set `honor_cipher_order` to `off`.

> **Note on Erlang versions**
>
> Riak's default cipher order behavior has the potential to crash Erlang
VMs that do not support it. Erlang VMs that are known to support it
include Basho's patched version of Erlang R16. Instructions on
installing it can be found in [Installing Erlang]({{<baseurl>}}riak/kv/2.9.9/setup/installing/source/erlang). This issue should
not affect Erlang 17.0 and later.

## Enabling SSL

In order to use any authentication or authorization features, you must
enable SSL for Riak. **SSL is disabled by default**, but you will need
to enable it prior to enabling security. If you are using [Protocol Buffers]({{<baseurl>}}riak/kv/2.9.9/developing/api/protocol-buffers/) as a transport protocol for Riak (which we strongly recommend), enabling SSL on a given node requires only that you specify a [host and port]({{<baseurl>}}riak/kv/2.9.9/configuring/reference/#client-interfaces) for the node
as well as a [certification configuration](#certificate-configuration).

If, however, you are using the [HTTP API]({{<baseurl>}}riak/kv/2.9.9/developing/api/http) for Riak and would like to
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
disabled and others enabled by setting the following [configurable parameters]({{<baseurl>}}riak/kv/2.9.9/configuring/reference/#security) to `on` or `off`:

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

If you are using any of the available [security sources]({{<baseurl>}}riak/kv/2.9.9/using/security/managing-sources/), including [trust-based authentication]({{<baseurl>}}riak/kv/2.9.9/using/security/managing-sources/#trust-based-authentication), you will need to do so
over a secure SSL connection. In order to establish a secure connection,
you will need to ensure that each Riak node's [configuration files]({{<baseurl>}}riak/kv/2.9.9/configuring/reference/#security) point to the proper paths for your
generated certs. By default, Riak assumes that all certs are stored in
each node's `/etc` directory.

If you are using the newer, `riak.conf`-based configuration system, you
can change the location of the `/etc` directory by modifying the
`platform_etc_dir`. More information can be found in our documentation
on [configuring directories]({{<baseurl>}}riak/kv/2.9.9/configuring/reference/#directories).

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
attacks, Riak performs [secure referer
checks](http://en.wikipedia.org/wiki/HTTP_referer) by default. Those
checks make it impossible to serve data directly from Riak. To disable
those checks, set the `secure_referer_check` parameter to `off`.

If you are using [certificate-based authentication]({{<baseurl>}}riak/kv/2.9.9/using/security/managing-sources/#certificate-based-authentication), Riak will check the certificate revocation list (CRL) of connecting clients' certificate by
default. To disable this behavior, set the `check_crl` parameter to
`off`.




