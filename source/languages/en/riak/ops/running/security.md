---
title: Riak Security
project: riak
version: 2.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, security]
---

As of version 2.0, Riak administrators can selectively apportion access to a wide variety of Riak's functionality, including accessing, modifying, and deleting objects, changing bucket properties, and running MapReduce jobs. Riak security enables you to create, modify, and delete users, assign to users roles, passwords, and other characteristics, designate security sources, and more.

**Note**: Currently, Riak security commands can be run only through the command line using the `riak-admin security` command. In future versions of Riak, administrators will have the option of issuing those commands through the Protocol Buffers and HTTP interfaces.

## Security Basics

This section covers enabling/disabling security and checking for current security status.

### Enabling Security

Riak security is disabled by default. To enable it at any time, simply run the `security enable` command:

```bash
riak-admin security enable
```

If security is successfully enabled, the console will return no response.

Please note that most security-related commands can be run while security is disabled, including the following:

* [[User management|Riak Security#Modifying-User-Characteristics]] --- Creating/deleting users and modifying user characteristics (more on that in the  section below)
* [[Permissions management|Riak Security#Permissions-Management]] --- Granting and revoking specific permissions vis-à-vis specific users
* [[Security source management|Riak Security#Security-Source-Management]] --- Adding and deleting security sources

This enables you to create security configurations of any level of complexity and turn those configurations on and off all at once if you wish.

If enabling security is successful, there should be no response in the terminal.

### Disabling Security

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

Riak security enables you to create new users and to modify or delete existing users. Currently, users can be assigned bear one or more of the following characteristics:

* `username`
* `roles`
* `password`

You may also assign users characteristics beyond those listed above---e.g. listing email addresses or other information---but those values will bear no significance to Riak itself.    

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

If the user `riakuser` were assigned a [[role|Riak Security#Role-Management]] alongside `other_riakuser` and the `name` of `lucius`, the output would look like this:

```bash
+----------+--------------+----------------------+---------------------+
| username |    roles     |       password       |       options       |
+----------+--------------+----------------------+---------------------+
| riakuser |other_riakuser|983e8ae1421574b8733824| [{"name","lucius"}] |
+----------+--------------+----------------------+---------------------+
```

If you'd like to see which permissions have been assigned to `riakuser`, you would need to use the `print-user` command, detailed [[below|Riak Security#Retrieving-Information-About-A-Single-User]].

If you'd like to preserve a record of your current list, you can simply pipe the output of `print-users` to a file or process, e.g. to a `user_list.txt` file:

```bash
riak-admin security print-users > user_list.txt
```

### Retrieving Information About a Single User

You can retrieve all information about a specific user using the `print-user` command, which takes the form of `riak-admin security print-user <username>`.

The output will look like this if the user `riakuser` has been explicitly granted a `riak_kv.get` permission on the bucket `shopping_list`:

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
|   ANY    |shopping_list|              riak_kv.get               |
+----------+-------------+----------------------------------------+
```

### Add User

The previous section presumed that the user `riakuser` had already been created. In this section, we will presume an empty user list.

To create a user with the username `riakuser`, we simply use the `add-user` command:

```bash
riak-admin security add-user riakuser
```

Using the command this way will create the user `riakuser` without _any_ characteristics beyond a username, which is the only attribute that you must assign upon user creation.

Alternatively, a password---or other attributes---can be assigned to the user upon creation:

```bash
riak-admin security add-user riakuser password=Test1234
```

### Assign a Password and Altering Existing User Characteristics

While passwords and other characteristics can be set upon user creation, it often makes sense to change user characteristics after the user already exists. Let's say that the user `riakuser` was created without a password (or created _with_ a password that we'd like to change). The `alter-user` command can be used to modify our `riakuser` user:

```bash
riak-admin security alter-user riakuser password=opensesame
```

**Note**: Only one password may be assigned to a user at a time.

When creating or altering a user, any number of `<option>=<value>` pairs can be appended to the end of the command:

```bash
riak-admin security alter-user riakuser name=bill age=47 fav_color=red
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

## Source Management

### Add source

A user will not have access to resources simply because they have been created via `add-user`. You must add a security **source** as well as grants to users to provide access to Riak securables, e.g. the ability to make `GET` and `PUT` requests. Sources may apply to all users or to a specific user or role.

Available sources:

Source   | Description |
:--------|:------------|
`trust` | Always authenticates successfully |
`password` | Check the user's password against the one stored in Riak |
`pam`  | Authenticate against the given PAM service |
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

The `all|<users>` designates that sources can be added to all users, a specific user, or a grouping of users, separated by commas, e.g. `add-source jane,bill,terry,chris`.

<!-- section on CIDRs -->

The following creates a source giving all users trusted access to securables when requests come from `localhost`:

```bash
riak-admin security add-source all 127.0.0.1/32 trust
```

The response from `riak-admin security print-sources`:

```bash
+--------------------+------------+----------+----------+
|       users        |    cidr    |  source  | options  |
+--------------------+------------+----------+----------+
|        all         |127.0.0.1/32|  trust   |    []    |
+--------------------+------------+----------+----------+
```

#### Adding a `default` Source

The following source requires a password for users connecting from any
host:

```bash
riak-admin security add-source all 0.0.0.0/0 password
```

The response from `riak-admin security print-sources`:

```bash
+--------------------+------------+----------+----------+
|       users        |    cidr    |  source  | options  |
+--------------------+------------+----------+----------+
|        all         |127.0.0.1/32|  trust   |    []    |
|        all         | 0.0.0.0/0  | password |    []    |
+--------------------+------------+----------+----------+
```

If a user connects from `127.0.0.1`, they will be trusted because that source is more specific than---and thus covered by---the `0.0.0.0/0 password` source.

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
`riak_kv.list_keys` | List keys in bucket
`riak_kv.list_buckets` | List buckets

<div class="note"><div class="title">Note</div>
`riak_kv.list_keys` and `riak_kv.list_buckets` are both very expensive operations that should be performed very rarely and never in production.
</div>

If you'd like to create, for example, a `client` role that is allowed only to run `GET` and `PUT` requests on all buckets:

```bash
riak-admin security add-user client
riak-admin security grant riak_kv.get,riak_kv.put ON ANY TO client
```

### MapReduce Permission

Permission to perform MapReduce jobs can be assigned using `riak_kv.mapreduce`:

```bash
riak-admin security grant riak_kv.mapreduce ON ANY TO mapreduce-power-user
```

### Bucket Type Permissions

In versions 2.0 and later, Riak users can manage 
`riak-admin security` allows you to manage the following bucket type-related permissions:

Permission | Operation |
:----------|:----------|
`riak_core.get_bucket` |  |
* `riak_core.set_bucket` |  |
* `riak_core.get_bucket_type` | Retrieve a bucket's properties (i.e. `props`) |
* `riak_core.set_bucket_type` | Set a bucket's properties (i.e. `props`) |

### Search Query Permission (Riak Search v1)

`riak_search.query`

### Search Query Permissions (Riak Search v2, aka Yokozuna)

Permission | Operation |
:----------|:----------|
`search.admin` |  |
`search.query` |  |

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

Now, let's run a test `GET` request through the HTTP interface against the bucket `tweets` using `riakuser`'s username and password:

```curl
curl -i -k -u riakuser:rosebud http://localhost:8098/buckets/tweets/tweet1
```

<!-- example response -->

## Managing Roles

Riak security understands security roles slightly differently from other database systems because roles and users _are essentially the same thing_.

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

If we'd like to make the user `jane_goodall` both an `admin` and an `arch-overlord`:

```bash
riak-admin alter-user jane_goodall roles=admin,arch-overlord
```

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

## Scratchpad

Snapshot of your current user list/sources:
`riak-admin security print-sources > sources.txt`

<!---
RESOURCES:
https://gist.github.com/lukebakken/1dcf90bf2a6d4009c6db
http://vagabond.github.io/2013/11/06/ricon-west-2013-talk-writeup/
-->


