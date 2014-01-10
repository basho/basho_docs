---
title: Riak Security
project: riak
version: 2.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, security]
---

As of version 2.0, Riak administrators can manage access to a wide variety of Riak's functionality, including accessing, modifying, and deleting objects, changing bucket properties, and running MapReduce jobs. Riak Security enables you to create, modify, and delete users, assign users roles, passwords, and other characteristics, designate security sources, and more.

**Note**: Currently, Riak Security commands can be run only through the command line. In future versions of Riak, administrators will have the option of issuing those commands through the Protocol Buffers and HTTP interfaces.

## Security Basics

This section covers enabling/disabling security and checking for current security status.

### Enabling Security

Riak Security is disabled by default. To enable it, simply run the `security enable` command:

```bash
riak-admin security enable
```

If security is successfully enabled, the console will return no response.

Please note that most security-related commands can be run while security is disabled, including the following:

* User management --- Creating/deleting users and modifying user characteristics (more on that in the [[Modifying User Characteristics|Riak Security#Modifying-User-Characteristics]] section below)
* Permissions management --- Granting and revoking specific permissions vis-à-vis specific users
* Security source management --- Adding and deleting security sources

This enables you to create security configurations of any level of complexity and turn those configurations on and off all at once if you wish.

If enabling security is successful, there should be no response in the terminal.

### Disabling security

Disabling security only disables the various permission checks that take place when executing operations against Riak. Users, roles, and other security attributes remain untouched.

```bash
riak-admin security disable
```

If security is successfully disabled, the console will return no response.

### Checking Security Status

To check whether or not security is currently enabled for a node, simply run the `security status` command:

```bash
riak-admin security status
```

This command will return a simple `Enabled` or `Disabled`.

## User Management

Riak Security enables you to create new users and to modify or delete existing users. Currently, users can bear one or more of the following characteristics:

* `username`
* `roles`
* `password`

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

If the user `riakuser` were assigned a role alongside `other_riakuser` and the `name` of `lucius`, the output would look like this:

```bash
+----------+-------+----------------------+---------------------+
| username | roles |       password       |       options       |
+----------+-------+----------------------+---------------------+
| riakuser |       |983e8ae1421574b8733824| [{"name","lucius"}] |
+----------+-------+----------------------+---------------------+
```

### Add User

The previous section presumed that the user `riakuser` had already been created. In this section, we will presume an empty user list.

To create a user with the username `riakuser`, we simply use the `add-user` command:

```bash
riak-admin security add-user riakuser
```

Using the command this way will create the user `riakuser` without _any_ characteristics beyond a username. Alternatively, a password---or other characteristics---can be assigned to the user upon creation:

```bash
riak-admin security add-user riakuser password=Test1234
```

### Assign a Password and Altering Existing User Characteristics

While passwords and other characteristics can be set upon user creation, it often makes sense to change user characteristics after the user already exists. Let's say that the user `riakuser` was created without a password (or created _with_ a password that we'd like to change). The `alter-user` command can be used to modifying our `riakuser` user:

```bash
riak-admin security alter-user riakuser password=opensesame
```

**Note**: Only one password may be assigned to a user at a time.

When creating or altering a user, any number of `<option>=<value>` pairs can be appended to the end of the command:

```bash
riak-admin security alter-user riakuser name=bill age=47 fav_color=red
```

**Note**: Assigning a `name`, `age`, or `fav_color` to a user may not be terribly useful. This is for example purposes only.

### Deleting a User

If you'd like to remove a user, e.g. `riakuser`, from the current user list, simply use the `del-user` command:

```
riak-admin security del-user riakuser
```

### Deleting Multiple Users

The `riak-admin security` command does not currently allow you to delete multiple users using a single command. One way of deleting multiple users is of course to run the `del-user` command once for each user you're deleting. Another way is to use a simple `for` loop in your shell:

```bash
for username in larry moe curly; do
riak-admin security del-user $username
done
```

## Source Management

### Add source

A user will not have access to resources simply because they have been created via `add-user`. You must add a source as well as grants to provide access to Riak securables, e.g. the ability to make `GET` and `PUT` requests. Sources may apply to all users or to a specific user or role.

Available sources:

Source   | Description |
:--------|:------------|
`trust` | Always authenticates successfully |
`password` | Check the user's password against the one stored in Riak |
`pam` | Authenticate against the given PAM service |
`certificate` | Authenticate using a client certificate |

### Adding a trusted source

Security sources can be added either to specific users or roles or to all users. Let's say that we want to give all users trusted access to securables when requests come from `localhost`:

```bash
riak-admin security add-source all 127.0.0.1/32 trust
```

In general, the `add-source` command takes the following form:

```bash
riak-admin security add-source all|<users> <CIDR> <source> [<option>=<value>[...]]
```

The `all|<users>` designates that sources can be added to all users, a specific user, or a grouping of users, separated by commas, e.g. `add-source jane,bill,terry,chris`. The `<CIDR>` 

The following creates a source giving all users trusted access to
securables when requests come from `localhost`:

```
# riak-admin security add-source all 127.0.0.1/32 trust
# riak-admin security print-sources
+--------------------+------------+----------+----------+
|       users        |    cidr    |  source  | options  |
+--------------------+------------+----------+----------+
|        all         |127.0.0.1/32|  trust   |    []    |
+--------------------+------------+----------+----------+
```

#### Adding a "default" source

The following source requires a password for users connecting from any
host.

```
# riak-admin security add-source all 0.0.0.0/0 password
# riak-admin security print-sources
+--------------------+------------+----------+----------+
|       users        |    cidr    |  source  | options  |
+--------------------+------------+----------+----------+
|        all         |127.0.0.1/32|  trust   |    []    |
|        all         | 0.0.0.0/0  | password |    []    |
+--------------------+------------+----------+----------+
```

If a user connects from `127.0.0.1` they will be trusted, because that
source is more specific than the `0.0.0.0/0 - password` source.

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

## Managing Permissions

Permission to perform a wide variety of operations against Riak can be granted to---or revoked from---users via the `grant` and `revoke` commands.

Permissions that can be granted for basic key/value access functionality:

* `riak_kv.get` --- retrieve an object
* `riak_kv.put` --- save or update object
* `riak_kv.delete` --- delete object
* `riak_kv.index` --- index an object using secondary indexes (2i)
* `riak_kv.list_keys` - list keys in bucket
* `riak_kv.list_buckets` - list buckets


* `riak_kv.mapreduce`

* `riak_core.get_bucket`
* `riak_core.set_bucket`
* `riak_core.get_bucket_type`
* `riak_core.set_bucket_type`

* `riak_search.query`

`search.admin`
`search.query`
```

## Testing Your Security Setup

A good way of ensuring that your security settings have been properly set up is to create a user with a password and specific permissions and then attempt to perform a range of actions as that user.

First, let's create a user, `riakuser`, and assign them the password `rosebud`:

```bash
riak-admin security add-user riakuser password=rosebud
```

Now, let's enable that user to make `GET` requests to the bucket `tweets`:

```bash
riak-admin security grant riak_kv.get ON ANY tweets TO riakuser
```

The `ANY` in this command simply designates that we have not granted any permissions to `riakuser` on the basis of bucket _type_. For more on granting/revoking permissions on the basis of type, see the [[permissions for bucket types|Riak Security#Permissions-for-Bucket-Types]] section above.

If we run the `print-user` command for `riakuser`, we should see the following under `Applied Permissions`:

```bash
+----------+----------+----------------------------------------+
|   type   |  bucket  |                 grants                 |
+----------+----------+----------------------------------------+
|   ANY    |  tweets  |              riak_kv.get               |
+----------+----------+----------------------------------------+
```




## Misc

Snapshot of your current user list:
`riak-admin security print-users > users.txt`

Snapshot of your current sources:
`riak-admin security print-sources > sources.txt`

<!---
RESOURCES:
https://gist.github.com/lukebakken/1dcf90bf2a6d4009c6db
http://vagabond.github.io/2013/11/06/ricon-west-2013-talk-writeup/
-->


