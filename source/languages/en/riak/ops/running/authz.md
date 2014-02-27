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

As of version 2.0, Riak administrators can selectively apportion access to a wide variety of Riak's functionality, including accessing, modifying, and deleting objects, changing bucket properties, and running MapReduce jobs. Riak security enables you to create, modify, and delete users, assign roles to users, passwords, and other characteristics, designate security sources, and more.

**Note**: Currently, Riak security commands can be run only through the command line using the `riak-admin security` command. In future versions of Riak, administrators will have the option of issuing those commands through the Protocol Buffers and HTTP interfaces.

## Terminology

* **Authentication** is the process of identifying a role.

* **Authorization** is verifying whether a user has access to perform the requested operation.

* **Users** and **Groups** are the same underlying concept (**Roles**), just used differently by convention.

    Any role can be assigned to other roles to add permissions. Typically authentication
    will be defined for users but not groups, while permissions may be
    assigned to either.

* **Sources** are used to define authentication mechanisms. A user cannot be authenticated to Riak until a source is defined.

## Security Basics

Riak security may be checked, enabled, or disabled by an operator through the command line. This allows an operator to change security settings for the whole cluster quickly, avoiding changing per-node configuration files.

### Enabling Security

Riak security is disabled by default. To enable it at any time, simply run the `security enable` command:

```bash
riak-admin security enable
```

If security is successfully enabled, the console will simply return no response.

Please note that most security-related commands can be run while security is disabled, including the following:

* [[User management|Authentication and Authorization#Modifying-User-Characteristics]] --- Creating/deleting users and modifying user characteristics (more on that in the  section below)
* [[Permissions management|Authentication and Authorization#Permissions-Management]] --- Granting and revoking specific permissions vis-à-vis specific users
* [[Security source management|Authentication and Authorization#Security-Source-Management]] --- Adding and deleting security sources

This enables you to create security configurations of any level of complexity and turn those configurations on and off all at once if you wish.

If enabling security is successful, there should be no response in the console.

### Disabling Security

Disabling security only disables the various permissions checks that take place when executing operations against Riak. Users, roles, and other security attributes remain untouched.

```bash
riak-admin security disable
```

If security is successfully disabled, the console will return no response.

### Checking Security Status

To check whether security is currently enabled for the cluster, simply use the `status` command:

```bash
riak-admin security status
```

This command will usually return `Enabled` or `Disabled`, but if security is enabled on a mixed-mode cluster (running a combination of Riak 2.0 and older versions) it will indicate that security is enabled but not yet available.

## User Management

Riak security enables you to control _authorization_ by creating, modifying, and deleting user characteristics and to grant users selective access to Riak functionality (and also to revoke access). Users can be assigned one or more of the following characteristics:

* `username`
* `roles`
* `password`

You may also assign users characteristics beyond those listed above---e.g. listing email addresses or other information---but those values will bear no special significance for Riak.

### Retrieve a Current User List

A list of currently existing users can be accessed at any time:

```bash
riak-admin security print-users
```

If there is only one currently existing user, `riakuser`, who has been assigned a password in addition to a username, the output will look something like this:

```bash
+----------+-------+----------------------+------------------------------+
| username | roles |       password       |           options            |
+----------+-------+----------------------+------------------------------+
| riakuser |       |983e8ae1421574b8733824|              []              |
+----------+-------+----------------------+------------------------------+
```

**Note**: All passwords are displayed in encrypted form in console output.

If the user `riakuser` were assigned a [[role|Authentication and Authorization#Role-Management]] alongside `other_riakuser` and the `name` of `lucius`, the output would look like this:

```bash
+----------+----------------+----------------------+---------------------+
| username |     roles      |       password       |       options       |
+----------+----------------+----------------------+---------------------+
| riakuser | other_riakuser |983e8ae1421574b8733824| [{"name","lucius"}] |
+----------+----------------+----------------------+---------------------+
```

If you'd like to see which permissions have been assigned to `riakuser`, you would need to use the `print-user` command, detailed in the section below.

If you'd like to preserve a record of your current list, you can simply pipe the output of `print-users` to a file or process, e.g. to a `user_list.txt` file:

```bash
riak-admin security print-users > user_list.txt
```

### Retrieving Information About a Single User

You can retrieve authorization information about a specific user using the `print-user` command, which takes the form of `riak-admin security print-user <username>`.

The output will look like this if the user `riakuser` has been explicitly granted a `riak_kv.get` permission on the bucket `shopping_list` and inherits a set of permissions from the `admin` role:

```bash
Inherited permissions

+--------+----------+----------+----------------------------------------+
|  role  |   type   |  bucket  |                 grants                 |
+--------+----------+----------+----------------------------------------+
| admin  |    *     |    *     |      riak_kv.get, riak_kv.delete,      |
|        |          |          |              riak_kv.put               |
+--------+----------+----------+----------------------------------------+

Applied permissions

+----------+-------------+----------------------------------------+
|   type   |   bucket    |                 grants                 |
+----------+-------------+----------------------------------------+
|    *     |      *      |      riak_kv.get, riak_kv.delete,      |
|          |             |               riak_kv.put              |
|   ANY    |shopping_list|               riak_kv.get              |
+----------+-------------+----------------------------------------+
```

Inherited permissions are those that stem from the user's roles, whereas applied permissions include both inherited permissions and those that have been assigned to the user directly. The `Applied permissions` section thus lists _all_ permissions granted to a user.

**Note**: The term `admin` is not a reserved term in Riak security. It is used here only for illustrative purposes.

### Add User

The previous section presumed that the user `riakuser` had already been created. In this section, we will presume that the user list is empty.

To create a user with the username `riakuser`, we simply use the `add-user` command:

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

**Note**: Only one password may be assigned to a user at a time.

When creating or altering a user, any number of `<option>=<value>` pairs can be appended to the end of the command. Any non-standard options (today, `roles` and `password`) will be stored and displayed via the `riak-admin security print-users` command.

```bash
riak-admin security alter-user riakuser name=bill age=47 fav_color=red
riak-admin security print-users
+----------+-------+----------+--------------------------------------------------+
| username | roles | password |                     options                      |
+----------+-------+----------+--------------------------------------------------+
| riakuser |       |          |[{"fav_color","red"},{"age","47"},{"name","bill"}]|
+----------+-------+----------+--------------------------------------------------+
```

**Note**: Usernames _cannot_ be changed using the `alter-user` command. If you attempt to do so by running, for example, `alter-user riakuser username=other-name`, then this will simply add the `{"username","other-name"}` tuple to `riakuser`'s options, which is most likely _not_ the preferred action.

### Deleting a User

If you'd like to remove a user, e.g. `riakuser`, from the current user list, simply use the `del-user` command:

```
riak-admin security del-user riakuser
```

<div class="note"><div class="title">Note</div>
The <tt>del-user</tt> command is used to delete both users <em>and roles</em> (because users and roles are ultimately the same thing). This means that if you have several users assigned the role <tt>superuser</tt>, running the <tt>del-user superuser</tt> command will remove the role <tt>superuser</tt> from all users currently assigned to that role. This command should thus be used with due care.
</div>

### Deleting Multiple Users

The `riak-admin security` command does not currently allow you to delete multiple users using a single command. An alternative way to do so, however, is to run the `del-user` command for each user you're deleting, e.g. `del-user user1` followed `del-user user2`. A more succinct way is to use a simple `for` loop in your shell (or in a shell script):

```bash
for username in larry moe curly
do
  riak-admin security del-user $username
done
```

## Managing Permissions

Permission to perform a wide variety of operations against Riak can be granted to---or revoked from---users via the `grant` and `revoke` commands.

### Basic Form

The `grant` and `revoke` commands take the following forms, respectively:

```bash
riak-admin security grant <permissions> ON ANY|<type> [bucket] TO <users>
riak-admin security revoke <permissions> ON ANY|<type> [bucket] FROM <users>
```

You can grant/revoke multiple permissions by separating permissions with a comma (with no spaces). You can also manage permissions with respect to several users---or roles---at a time. Here is an example of granting multiple permissions to multiple users:

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

If you'd like to create, for example, a `client` role that is allowed only to run `GET` and `PUT` requests on all buckets:

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

Riak security sources may be applied to all users/roles or only to a specific user or role.

#### Available Sources

Source   | Description |
:--------|:------------|
`trust` | Always authenticates successfully if access has been granted to a user, a role, or all users on the specified CIDR range |
`password` | Check the user's password against the [PBKSD2](http://en.wikipedia.org/wiki/PBKDF2) hashed password stored in Riak |
`pam`  | Authenticate against the given pluggable authentication module (PAM) service |
`certificate` | Authenticate using a client certificate |

### Adding a Trusted Source

Security sources can be added either to specific users or roles or to all users.

In general, the `add-source` command takes the following form:

```bash
riak-admin security add-source all|<users> <CIDR> <source> [<option>=<value>[...]]
```

The `all|<users>` designates that sources can be added to all users/roles. A source can be added to a specific user/role by simply listing the `username`, whereas a grouping of users/roles can be added if the `usernames` are separated by commas, e.g. `add-source jane,bill,terry,chris`.

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

## Managing Roles

Riak security understands security roles slightly differently from other database systems because roles and users are one and the same.

### Creating a New Role

In an example above, a new user was created with the username `riakuser`. It's important to bear in mind that `riakuser` is _also a role_. Roles and users are completely interchangeable. And so if you'd like to create an `admin` role, you could perform permitted operations as `admin` _or_ assign the role of `admin` to other users.

As expected, creating a new role involves the `add-user` command:

```bash
riak-admin security add-user admin
```

### Assigning a Role to a User

If we have a user `jane_goodall` and we'd like to assign her the role `admin`, we simply assign the value `admin` to the option `roles`:

```bash
riak-admin security alter-user jane_goodall roles=admin
```

If we'd like to make the user `jane_goodall` both an `admin` and an `archoverlord`:

```bash
riak-admin alter-user jane_goodall roles=admin,archoverlord
```

There is no way to incrementally add roles; even if `jane_goodall` was already an `admin`, it is necessary to list it again when adding the `archoverlord` role.

### Assigning a Role to Multiple Users

There is no command for assigning a role (or roles) to multiple users at one time, though you may use methods such as `for` loops in your shell:

```bash
for user in larry moe curly
do
  riak-admin security alter-user $user roles=stooge
done
```

### Removing Roles From a User

There is no command for directly removing a user's roles, but you can assign a user an empty value for the `roles` option:

```bash
riak-admin security alter-user riakuser roles=
```

If you wish to unassign a single role from a user while retaining others, simply provide the others to the `alter-user` command.

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
