---
title: Authentication and Authorization
project: riak
version: 2.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, security]
---

<div class="info">
<div class="title">Network security</div>
This document covers only the 2.0 authentication and authorization features. For a look at network security in Riak, see [[Security and Firewalls]].
</div>

As of version 2.0, Riak administrators can selectively apportion
access to a wide variety of Riak's functionality, including accessing,
modifying, and deleting objects, changing bucket properties, and
running MapReduce jobs.

**Note**: Currently, Riak security commands can be run only through
  the command line using the `riak-admin security` command. In future
  versions of Riak, administrators may have the option of issuing
  those commands through the Protocol Buffers and HTTP interfaces.

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

## Security Basics

Riak security may be checked, enabled, or disabled by an operator through the command line. This allows an operator to change security settings for the whole cluster quickly, avoiding changing per-node configuration files.

### Enabling Security

<div class="note"> <div class="title">Danger</div>
<b>Enabling security will change the way your client libraries and your
applications interact with Riak.</b> Once security is enabled, all
client connections must be encrypted and all permissions will be
denied by default. Do not enable this in production until you have
verified that your libraries support Riak security, including
encrypted HTTP or protocol buffers traffic, and that your applications
are assigned user accounts with the proper permissions.  </div>

Riak security is disabled by default. To enable it:

```bash
riak-admin security enable
```

*As per the warning above, do not enable security in production
 without taking the appropriate precautions.*

All users, groups, authentication sources, and permissions can be
configured while security is disabled, allowing you to create a
security configuration of any level of complexity without prematurely
impacting the service.

### Disabling Security

Disabling security only disables the various permissions checks that
take place when executing operations against Riak. Users, groups, and
other security attributes remain untouched.

```bash
riak-admin security disable
```

If security is successfully disabled, the console will return no
response, and the database will no longer require (but will still
permit) encrypted client traffic.

### Checking Security Status

To check whether security is currently enabled for the cluster, use the `status` command:

```bash
riak-admin security status
```

This command will usually return `Enabled` or `Disabled`, but if
security is enabled on a mixed-mode cluster (running a combination of
Riak 2.0 and older versions) it will indicate that security is enabled
but not yet available.

## User Management

Riak security enables you to control _authorization_ by creating, modifying, and deleting user characteristics and to grant users selective access to Riak functionality (and also to revoke access). Users can be assigned one or more of the following characteristics:

* `username`
* `groups`
* `password`

You may also assign users characteristics beyond those listed above---e.g., listing email addresses or other information---but those values will carry no special significance for Riak.

### Retrieve a Current User List

A list of currently existing users can be accessed at any time:

```bash
riak-admin security print-users
```

Example output, assuming one user with an assigned password:

```bash
+----------+-------+----------------------+------------------------------+
| username | groups |       password       |           options            |
+----------+-------+----------------------+------------------------------+
| riakuser |       |983e8ae1421574b8733824|              []              |
+----------+-------+----------------------+------------------------------+
```

**Note**: All passwords are displayed in encrypted form in console output.

If the user `riakuser` were assigned to the group `dev` and a `name`
of `lucius`, the output would look like this:

```bash
+----------+----------------+----------------------+---------------------+
| username |     groups      |       password       |       options       |
+----------+----------------+----------------------+---------------------+
| riakuser |      dev       |983e8ae1421574b8733824| [{"name","lucius"}] |
+----------+----------------+----------------------+---------------------+
```

If you'd like to see which permissions have been assigned to
`riakuser`, you would need to use the `print-user` command, detailed
below.

### Retrieving Information About a Single User

You can retrieve authorization information about a specific user using the `print-user` command, which takes the form of `riak-admin security print-user <username>`.

The output will look like this if the user `riakuser` has been
explicitly granted a `riak_kv.get` permission on the bucket
`shopping_list` and inherits a set of permissions from the `admin`
group:

```bash
Inherited permissions

+--------+----------+----------+----------------------------------------+
| group  |   type   |  bucket  |                 grants                 |
+--------+----------+----------+----------------------------------------+
| admin  |    *     |    *     |      riak_kv.get, riak_kv.delete,      |
|        |          |          |              riak_kv.put               |
+--------+----------+----------+----------------------------------------+

Dedicated permissions (riakuser)

+----------+-------------+----------------------------------------+
|   type   |   bucket    |                 grants                 |
+----------+-------------+----------------------------------------+
|   ANY    |shopping_list|               riak_kv.get              |
+----------+-------------+----------------------------------------+

Cumulative permissions (riakuser)

+----------+-------------+----------------------------------------+
|   type   |   bucket    |                 grants                 |
+----------+-------------+----------------------------------------+
|    *     |      *      |      riak_kv.get, riak_kv.delete,      |
|          |             |               riak_kv.put              |
|   ANY    |shopping_list|               riak_kv.get              |
+----------+-------------+----------------------------------------+
```

**Note**: The term `admin` is not a reserved term in Riak security. It is used here only for illustrative purposes.

### Add Group

For easier management of permissions across several users, it is
possible to create groups to be assigned to those users.

```bash
riak-admin security add-group admin
```

### Add User

To create a user with the username `riakuser`, we use the `add-user` command:

```bash
riak-admin security add-user riakuser
```

Using the command this way will create the user `riakuser` without _any_ characteristics beyond a username, which is the only attribute that you must assign upon user creation.

Alternatively, a password---or other attributes---can be assigned to the user upon creation. Here, we'll assign a password:

```bash
riak-admin security add-user riakuser password=Test1234
```

### Assign a Password and Altering Existing User Characteristics

While passwords and other characteristics can be set upon user creation, it often makes sense to change user characteristics after the user already exists. Let's say that the user `riakuser` was created without a password (or created _with_ a password that we'd like to change). The `alter-user` command can be used to modify our `riakuser` user:

```bash
riak-admin security alter-user riakuser password=opensesame
```

When creating or altering a user, any number of `<option>=<value>`
pairs can be appended to the end of the command. Any non-standard
options will be stored and displayed via the `riak-admin security
print-users` command.

```bash
riak-admin security alter-user riakuser name=bill age=47 fav_color=red
riak-admin security print-users
+----------+--------+----------+--------------------------------------------------+
| username | groups | password |                     options                      |
+----------+--------+----------+--------------------------------------------------+
| riakuser |        |          |[{"fav_color","red"},{"age","47"},{"name","bill"}]|
+----------+--------+----------+--------------------------------------------------+
```

**Note**: Usernames _cannot_ be changed using the `alter-user`
  command. If you attempt to do so by running, for example,
  `alter-user riakuser username=other-name`, then this will add
  the `{"username","other-name"}` tuple to `riakuser`'s options.

### Managing Groups for a User

If we have a user `jane_goodall` and we'd like to assign her to the
`admin` group, we assign the value `admin` to the option `groups`:

```bash
riak-admin security alter-user jane_goodall groups=admin
```

If we'd like to make the user `jane_goodall` both an `admin` and an
`archoverlord`:

```bash
riak-admin alter-user jane_goodall groups=admin,archoverlord
```

There is no way to incrementally add groups; even if `jane_goodall` was
already an `admin`, it is necessary to list it again when adding the
`archoverlord` group.

Thus, to remove a group from a user, use `alter-user` and list all
*other* groups.

If the user should be removed from all groups, use `groups=` with no
list:

```bash
riak-admin alter-user jane_goodall groups=
```

### Managing Groups for Groups

Groups can be added to other groups for cascading permissions.

```bash
riak-admin alter-group admin groups=dev
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
add or delete multiple users using a single command.

## Managing Permissions

Permission to perform a wide variety of operations against Riak can be granted to---or revoked from---users via the `grant` and `revoke` commands.

### Basic Form

The `grant` command takes one of the following forms:

```bash
riak-admin security grant <permissions> ON ANY TO all|{<user>|<group>[,...]}
riak-admin security grant <permissions> ON <bucket-type> TO all|{<user>|<group>[,...]}
riak-admin security grant <permissions> ON <bucket-type> <bucket> TO all|{<user>|<group>[,...]}
```

The `revoke` command is the same, but with `FROM` instead of `TO`:

```bash
riak-admin security revoke <permissions> ON ANY FROM all|{<user>|<group>[,...]}
riak-admin security revoke <permissions> ON <bucket-type> FROM all|{<user>|<group>[,...]}
riak-admin security revoke <permissions> ON <bucket-type> <bucket> FROM all|{<user>|<group>[,...]}
```

In each case, `ANY` represents all bucket types and all buckets with
them, `all` indicates that all users and groups should receive (or
lose) the indicated permissions, and any number of users or groups may
be listed, comma-separated, with no whitespace.

Here is an example of granting multiple permissions across all buckets
and bucket types to multiple users:

```bash
riak-admin security grant riak_kv.get,riak_search.query ON ANY TO jane,ahmed
```

### Key/Value Permissions

Permissions that can be granted for basic key/value access functionality:

Permission | Operation |
:----------|:----------|
`riak_kv.get` | Retrieve objects
`riak_kv.put` | Create or update objects
`riak_kv.delete` | Delete objects
`riak_kv.index` | Index objects using secondary indexes (2i)
`riak_kv.list_keys` | List all of the keys in a bucket
`riak_kv.list_buckets` | List all buckets

<div class="note"><div class="title">Note</div>
`riak_kv.list_keys` and `riak_kv.list_buckets` are both very expensive operations that should be performed very rarely and never in production.
</div>

If you'd like to create, for example, a `client` account that is
allowed only to run `GET` and `PUT` requests on all buckets:

```bash
riak-admin security add-user client
riak-admin security grant riak_kv.get,riak_kv.put ON ANY TO client
```

### MapReduce Permissions

Permission to perform MapReduce jobs can be assigned using `riak_kv.mapreduce`:

```bash
riak-admin security grant riak_kv.mapreduce ON ANY TO mapreduce-power-user
```

### Bucket Type Permissions

In versions 2.0 and later, Riak users can manage [[bucket types|Using Bucket Types]] in addition to setting bucket properties. `riak-admin security` allows you to manage the following bucket type-related permissions:

Permission | Operation |
:----------|:----------|
`riak_core.get_bucket` | Retrieve the `props` associated with a bucket |
`riak_core.set_bucket` | Modify the `props` associated with a bucket |
`riak_core.get_bucket_type` | Retrieve the set of `props` associated with a bucket type |
`riak_core.set_bucket_type` | Modify the set of `props` associated with a bucket type |

### Search Query Permission (Riak Search version 1)

Security is incompatible with the original Riak Search.

### Search Query Permissions (Riak Search version 2, aka Yokozuna)

If you are using the search capabilities included with Riak versions 2.0 and greater, the following search-related permissions can be granted/revoked:

Permission | Operation |
:----------|:----------|
`search.admin` | The ability to perform search admin-related tasks, such as creating and deleting indexes and adding and modifying search schemas |
`search.query` | The ability to query an index |

#### Usage Examples

To grant the user `riakuser` the ability to query all indexes:

```bash
riak-admin security grant search.query ON index TO riakuser

# To revoke:
# riak-admin security revoke search.query ON index FROM riakuser
```

To grant the user `riakuser` the ability to query all schemas:

```bash
riak-admin security grant search.query ON schema TO riakuser

# To revoke:
# riak-admin security revoke search.query ON schema FROM riakuser
```

To grant the user `riakuser` admin privileges only on the index `riakusers_index`:

```bash
riak-admin security grant search.admin ON index riakuser_index TO riakuser

# To revoke:
# riak-admin security revoke search.admin ON index riakuser_index FROM riakuser
```

To grant `riakuser` querying and admin permissions on the index `riakuser_index`:

```bash
riak-admin security grant search.query,search.admin ON index TO riakuser

# To revoke:
# riak-admin security revoke search.query,search.admin ON index FROM riakuser
```

More comprehensive information on search-related security can be found under [[Riak Search Security]].

## Managing Sources

While user management enables you to control _authorization_ with regard to users, security **sources** provide you with an interface for managing means of _authentication_. If you create users and grant them access to some or all of Riak's functionality as described in the [[User Management|Authentication and Authorization#User-Management]] section, you will then need to define security sources required for authentication.

### Add Source

Riak security sources may be applied to all users or only to a specific user.

#### Available Sources

Source   | Description |
:--------|:------------|
`trust` | Always authenticates successfully if access has been granted to a user or all users on the specified CIDR range |
`password` | Check the user's password against the [PBKSD2](http://en.wikipedia.org/wiki/PBKDF2) hashed password stored in Riak |
`pam`  | Authenticate against the given pluggable authentication module (PAM) service |
`certificate` | Authenticate using a client certificate |

### Adding a Trusted Source

Security sources can be added either to specific users or to all users.

In general, the `add-source` command takes the following form:

```bash
riak-admin security add-source all|<users> <CIDR> <source> [<option>=<value>[...]]
```

Using `all` indicates that the authentication source can be added to
all users. A source can be added to a specific user or to a list of
names separated by commas, e.g. `add-source jane,bill,terry,chris`.

Let's say that we want to give all users trusted access to securables (without a password) when requests come from `localhost`:

```bash
riak-admin security add-source all 127.0.0.1/32 trust
```

At that point, the `riak-admin security print-sources` command would print the following:

```bash
+--------------------+------------+----------+----------+
|       users        |    cidr    |  source  | options  |
+--------------------+------------+----------+----------+
|        all         |127.0.0.1/32|  trust   |    []    |
+--------------------+------------+----------+----------+
```

#### Source Management Usage Examples

To require a password from users `juliette` and `sanjay` when they connect from the class C network at `10.0.0.0`:

```bash
riak-admin security add-source juliette,sanjay 10.0.0.0/24 password
```

Instructions on assigning passwords are [[above|Authentication and Authorization#User-Management]].

To require all users to authenticate through a PAM login service:

```bash
riak-admin security add-source all 0.0.0.0/0 pam service=login
```



### Delete source

```
# riak-admin security del-source all 0.0.0.0/0
# riak-admin security print-sources
+--------------------+------------+----------+----------+
|       users        |    cidr    |  source  | options  |
+--------------------+------------+----------+----------+
|        all         |127.0.0.1/32|  trust   |    []    |
+--------------------+------------+----------+----------+
```


## Security Ciphers

To view a list of currently available security ciphers or change Riak's preferences, use the `ciphers` command:

```bash
riak-admin security ciphers
```

That command will return a large list:

```bash
Configured ciphers

ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256: ...

Valid ciphers(35)

ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256: ...

Unknown/Unsupported ciphers(32)

ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256: ...
```

To alter the list (to constrain it and/or to set preferred ciphers higher in the list):

```bash
riak-admin security ciphers DHE-RSA-AES256-SHA:AES128-GCM-SHA256
Configured ciphers

DHE-RSA-AES256-SHA:AES128-GCM-SHA256

Valid ciphers(1)

DHE-RSA-AES256-SHA

Unknown/Unsupported ciphers(1)

AES128-GCM-SHA256
```

A list of available ciphers on a server can be obtained using the `openssl` command:

```bash
openssl ciphers
DHE-RSA-AES256-SHA:DHE-DSS-AES256-SHA:AES256-SHA:EDH-RSA-DES-CBC3-SHA:EDH-DSS-DES-CBC3-SHA:DES-CBC3-SHA:DES-CBC3-MD5:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA:AES128-SHA:DHE-RSA-SEED-SHA:DHE-DSS-SEED-SHA:SEED-SHA:RC2-CBC-MD5:RC4-SHA:RC4-MD5:RC4-MD5:EDH-RSA-DES-CBC-SHA:EDH-DSS-DES-CBC-SHA:DES-CBC-SHA:DES-CBC-MD5:EXP-EDH-RSA-DES-CBC-SHA:EXP-EDH-DSS-DES-CBC-SHA:EXP-DES-CBC-SHA:EXP-RC2-CBC-MD5:EXP-RC2-CBC-MD5:EXP-RC4-MD5:EXP-RC4-MD5
```

Riak's cipher preferences were taken from [Mozilla's Server Side TLS documentation](https://wiki.mozilla.org/Security/Server_Side_TLS).
