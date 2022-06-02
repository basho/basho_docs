---
title_supertext: "Security"
title: "User Management"
description: "Manage user authorization and access to Riak TS."
menu:
  riak_ts-1.5.0:
    name: "User Management"
    identifier: "security_user_management"
    weight: 120
    parent: "security"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/using/security/user-management
---

Riak TS security lets you to control authorization by creating, modifying, and deleting user characteristics and granting users selective access to Riak TS functionality. Users can be assigned one or more of the following characteristics:

* `username`
* `groups`
* `password`

You may also assign users characteristics beyond those listed
above, such as listing email addresses or other information, but those
values will carry no special significance for Riak TS.

{{% note %}}
The `username` cannot be changed once a user has been created.
{{% /note %}}

## Retrieve a Current User or Group List

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

{{% note %}}
All passwords are displayed in encrypted form in console output.
{{% /note %}}

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
`riakuser`, you would need to use the `print-grants` command (see next section).

The `security print-user` or `security print-group` commands
can be used with a name as argument to see the same information as
above, except for only that user or group.

## Permissions Grants For a Single User or Group

You can retrieve authorization information about a specific user or
group using the `print-grants` command, which takes the form of
`riak-admin security print-grants <username>`.

The output will look like this if the user `riakuser` has been
explicitly granted a `riak_ts.get` permission on the table
`shopping_list` and inherits a set of permissions from the `admin`
group:

```bash
Inherited permissions (user/riakuser)

+--------+----------+----------+----------------------------------------+
| group  |   type   |  table   |                 grants                 |
+--------+----------+----------+----------------------------------------+
| admin  |    *     |    *     |      riak_ts.get, riak_ts.delete,      |
|        |          |          |              riak_ts.put               |
+--------+----------+----------+----------------------------------------+

Dedicated permissions (user/riakuser)

+----------+-------------+----------------------------------------+
|   type   |   table     |                 grants                 |
+----------+-------------+----------------------------------------+
|   ANY    |shopping_list|               riak_ts.get              |
+----------+-------------+----------------------------------------+

Cumulative permissions (user/riakuser)

+----------+-------------+----------------------------------------+
|   type   |   table     |                 grants                 |
+----------+-------------+----------------------------------------+
|    *     |      *      |      riak_ts.get, riak_ts.delete,      |
|          |             |               riak_ts.put              |
|   ANY    |shopping_list|               riak_ts.get              |
+----------+-------------+----------------------------------------+
```

{{% note %}}
The term `admin` is not a reserved term in Riak security. It
is used here only for illustrative purposes.
{{% /note %}}

Because the same name can represent both a user and a group, a prefix
(`user/` or `group/`) can be used before the name (e.g., `print-grants
user/admin`). If a name collides and no prefix is supplied, grants for
both will be listed separately.

## Add Group

For easier management of permissions across several users, it is
possible to create groups to be assigned to those users.

```bash
riak-admin security add-group admin
```

## Add User

To create a user with the username `riakuser`, we use the `add-user`
command:

```bash
riak-admin security add-user riakuser
```

Using the command this way creates the user `riakuser` without any
characteristics beyond a username, which is the only attribute that you
must assign upon user creation.

You may also assign a password or other attributes to the user upon creation. Here, we'll assign a password:

```bash
riak-admin security add-user riakuser password=Test1234
```

## Assigning a Password and Altering Existing User Characteristics

While passwords and other characteristics can be set upon user creation,
it often makes sense to change user characteristics after the user has
already been created. Let's say that the user `riakuser` was created
without a password (or created with a password that we'd like to
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

{{% note %}}
Usernames CANNOT be changed using the `alter-user` command.
If you attempt to do so by running `alter-user riakuser
username=other-name`, for example, this will add the
`{"username","other-name"}` tuple to `riakuser`'s options.
{{% /note %}}

## Managing Groups for a User

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
`archoverlord` group. Thus, to remove a group from a user, use
`alter-user` and list all other groups.

If the user should be removed from all groups, use `groups=` with no
list:

```bash
riak-admin alter-user jane_goodall groups=
```

## Managing Groups for Groups

Groups can be added to other groups for cascading permissions.

```bash
riak-admin alter-group admin groups=dev
```

## Deleting a User or Group

If you'd like to remove a user, use the `del-user` command:

```bash
riak-admin security del-user riakuser
```

For groups, use the `del-group` command:

```bash
riak-admin security del-group admin
```

## Adding or Deleting Multiple Users

The `riak-admin security` command does not currently allow you to
add or delete multiple users using a single command. Instead, they must
be added or deleted one by one.

## Managing Permissions

Permission to perform a wide variety of operations against Riak TS can be
granted to, or revoked from, users via the `grant` and `revoke`
commands.

### `grant`

The `grant` command takes one of the following forms:

```bash
riak-admin security grant <permissions> on any to all|{<user>|<group>[,...]}
riak-admin security grant <permissions> on <table> to all|{<user>|<group>[,...]}
```

### `revoke`

The `revoke` command is essentially the same, except that `to` is
replaced with `from` of `to`:

```bash
riak-admin security revoke <permissions> on any from all|{<user>|<group>[,...]}
riak-admin security revoke <permissions> on <table> from all|{<user>|<group>[,...]}
```

### Selecting `any` and `all`

If you select `any`, the permission (or set of permissions) is granted/revoked for all tables. If you specify a table only, then the permission is granted/revoked for all tables of that type.

Selecting `all` grants or revokes a permission (or set of permissions)
for all users in all groups. When specifying the user(s)/group(s) to apply a permission (or set of permissions), you may list any number of users or groups comma-separated with no whitespace. Here is an example of granting multiple permissions across all tables to multiple users:

```bash
riak-admin security grant riak_ts.get on any to jane,ahmed
```

If the same name is used for both a user and a group, the `grant`
command will ask for the name to be prefixed with `user/` or `group/`
to disambiguate.

## Riak TS Permissions

Permissions that can be granted for basic time series access
functionality:

Permission | Operation |
:----------|:----------|
`riak_ts.get` | Retrieve time series data via a single key
`riak_ts.put` | Write time series data via a single key or via a SQL INSERT statement
`riak_ts.delete` | Delete a single record via a key
`riak_ts.list_keys` | Stream a list of primary keys for a table
`riak_ts.coverage` | Return coverage information for a given query
`riak_ts.create_table` | Create a time series table
`riak_ts.query_select` | Retrieve data via a SQL SELECT statement
`riak_ts.describe_table` | Retrieve metadata about a single TS table
`riak_ts.show_tables` | Retrieve a list of TS tables
`riak_ts.query_explain` | Diagnostic information about how a query will be executed

{{% note title="Note on Listing Keys and Tables" %}}
`riak_ts.list_keys` and `riak_ts.show_tables` are
both very expensive operations that should be performed very rarely and
never in production. Access to this functionality should be granted very
carefully.
{{% /note %}}

For example, if you'd like to create a `client` account that is
allowed only to run `GET` and `PUT` requests on all tables:

```bash
riak-admin security add-user client
riak-admin security grant riak_ts.get,riak_ts.put on any to client
```
